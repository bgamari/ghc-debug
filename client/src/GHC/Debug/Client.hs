{-# LANGUAGE TupleSections #-}
module GHC.Debug.Client
  ( Debuggee(..)
  , withDebuggee
  , withDebuggeeSocket
  , pauseDebuggee
  , request
  , Request(..)
  , getInfoTblPtr
  , decodeClosure
  , FieldValue(..)
  , decodeInfoTable
  , lookupInfoTable
  , getDwarfInfo
  , lookupDwarf
  , showFileSnippet
  , DebugClosure(..)
  , dereferenceClosures
  , dereferenceClosure
  , dereferenceStack
  , dereferenceConDesc
  , fullTraversal
  , Tritraversable(..)
  , traceRequestLog
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.Debug.Types
import GHC.Debug.Decode
import GHC.Debug.Decode.Stack
import Network.Socket
import qualified Data.HashMap.Strict as HM
import System.IO
import Data.Word
import Data.Maybe
import System.Endian
import Data.Foldable
import Data.Coerce
import Data.Bitraversable


import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.ADT.Pretty as DwarfPretty
import qualified Data.Dwarf.Elf as Dwarf.Elf

import Data.Dwarf
import Data.Dwarf.ADT
import qualified Data.Text  as T
import Data.List
import System.Process
import System.Environment
import System.FilePath
import System.Directory
import Text.Printf


import Data.IORef


data Debuggee = Debuggee { debuggeeHdl :: Handle
                         , debuggeeInfoTblEnv :: MVar (HM.HashMap InfoTablePtr RawInfoTable)
                         , debuggeeDwarf :: Maybe Dwarf
                         , debuggeeFilename :: FilePath
                         -- Keep track of how many of each request we make
                         , debuggeeRequestCount :: IORef (HM.HashMap CommandId Int)
                         }


-- | Add the request to the request count for debugging
logRequest :: Debuggee -> Request a -> IO ()
logRequest d r = do
  let c = requestCommandId r
  atomicModifyIORef' (debuggeeRequestCount d) ((,()) . HM.alter (Just . maybe 1 (+1)) c)


debuggeeProcess :: FilePath -> FilePath -> IO CreateProcess
debuggeeProcess exe sockName = do
  e <- getEnvironment
  return $
    (proc exe []) { env = Just (("GHC_DEBUG_SOCKET", sockName) : e) }

-- | Open a debuggee, this will also read the DWARF information
withDebuggee :: FilePath  -- ^ path to executable
             -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
             -> (Debuggee -> IO a)
             -> IO a
withDebuggee exeName socketName action = do
    -- Read DWARF information from the executable
    -- Start the process we want to debug
    cp <- debuggeeProcess exeName socketName
    withCreateProcess cp $ \_ _ _ _ -> do
      dwarf <- getDwarfInfo exeName
    -- Now connect to the socket the debuggeeProcess just started
      withDebuggeeSocket exeName socketName (Just dwarf) action


-- | Open a debuggee's socket directly
withDebuggeeSocket :: FilePath  -- ^ executable name of the debuggee
                   -> FilePath  -- ^ debuggee's socket location
                   -> Maybe Dwarf
                   -> (Debuggee -> IO a)
                   -> IO a
withDebuggeeSocket exeName sockName mdwarf action = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix sockName)
    hdl <- socketToHandle s ReadWriteMode
    infoTableEnv <- newMVar mempty
    requestMap <- newIORef (HM.empty)
    action (Debuggee hdl infoTableEnv mdwarf exeName requestMap)

-- | Send a request to a 'Debuggee' paused with 'pauseDebuggee'.
request :: Debuggee -> Request resp -> IO resp
request d r = do
  logRequest d r
  doRequest (debuggeeHdl d) r


lookupInfoTable :: Debuggee -> RawClosure -> IO (RawInfoTable, RawClosure)
lookupInfoTable d rc = do
    let ptr = getInfoTblPtr rc
    itblEnv <- readMVar (debuggeeInfoTblEnv d)
    case HM.lookup ptr itblEnv of
      Nothing -> do
        [itbl] <- request d (RequestInfoTables [ptr])
        modifyMVar_ (debuggeeInfoTblEnv d) $ return . HM.insert ptr itbl
        return (itbl, rc)
      Just itbl ->  return (itbl, rc)

pauseDebuggee :: Debuggee -> IO a -> IO a
pauseDebuggee d =
    bracket_ (void $ request d RequestPause) (void $ request d RequestResume)

getDwarfInfo :: FilePath -> IO Dwarf
getDwarfInfo fn = do
 (dwarf, warnings) <- Dwarf.Elf.parseElfDwarfADT Dwarf.LittleEndian fn
-- mapM_ print warnings
-- print $ DwarfPretty.dwarf dwarf
 return dwarf

lookupDwarf :: Debuggee -> InfoTablePtr -> Maybe ([FilePath], Int, Int)
lookupDwarf d (InfoTablePtr w) = do
  (Dwarf units) <- debuggeeDwarf d
  asum (map (lookupDwarfUnit (fromBE64 w)) units)

lookupDwarfUnit :: Word64 -> Boxed CompilationUnit -> Maybe ([FilePath], Int, Int)
lookupDwarfUnit w (Boxed _ cu) = do
  low <- cuLowPc cu
  high <- cuHighPc cu
  guard (low <= w && w <= high)
  (LNE ds fs ls) <- cuLineNumInfo cu
  (fp, l, c) <- foldl' (lookupDwarfLine w) Nothing (zip ls (tail ls))
  let res_fps = if null ds then [T.unpack (cuCompDir cu) </> fp]
                           else map (\d -> T.unpack (cuCompDir cu) </> T.unpack d </> fp) ds
  return ( res_fps
         , l , c)

lookupDwarfSubprogram :: Word64 -> Boxed Def -> Maybe Subprogram
lookupDwarfSubprogram w (Boxed _ (DefSubprogram s)) = do
  low <- subprogLowPC s
  high <- subprogHighPC s
  guard (low <= w && w <= high)
  return s
lookupDwarfSubprogram _ _ = Nothing

lookupDwarfLine :: Word64
                -> Maybe (FilePath, Int, Int)
                -> (Dwarf.DW_LNE, Dwarf.DW_LNE)
                -> Maybe (FilePath, Int, Int)
lookupDwarfLine w Nothing (d, nd) =
  if lnmAddress d <= w && w <= lnmAddress nd
    then do
      let (LNEFile file _ _ _) = lnmFiles nd !! (fromIntegral (lnmFile nd) - 1)
      Just
        (T.unpack file, fromIntegral (lnmLine nd), fromIntegral (lnmColumn nd))
    else Nothing
lookupDwarfLine _ (Just r) _ =  Just r

showFileSnippet :: Debuggee -> ([FilePath], Int, Int) -> IO ()
showFileSnippet d (fps, l, c) = go fps
  where
    go [] = putStrLn ("No files could be found: " ++ show fps)
    go (fp: fps) = do
      exists <- doesFileExist fp
      -- get file modtime
      if not exists
        then go fps
        else do
          fp `warnIfNewer` debuggeeFilename d
          src <- zip [1..] . lines <$> readFile fp
          let ctx = take 10 (drop (max (l - 5) 0) src)
          putStrLn (fp <> ":" <> show l <> ":" <> show c)
          mapM_ (\(n, l) ->
           let sn = show n
           in putStrLn (sn <> replicate (5 - length sn) ' ' <> l)) ctx

dereferenceClosure :: Debuggee -> ClosurePtr -> IO Closure
dereferenceClosure d c = forgetSize . unDCS . head <$> dereferenceClosures d [c]

dereferenceSizedClosure :: Debuggee -> ClosurePtr -> IO SizedClosure
dereferenceSizedClosure d c = head <$> dereferenceClosures d [c]

dereferenceClosures  :: Debuggee -> [ClosurePtr] -> IO [SizedClosure]
dereferenceClosures d cs = do
    raw_cs <- request d (RequestClosures cs)
    let its = map getInfoTblPtr raw_cs
    --print $ map (lookupDwarf d) its
    raw_its <- request d (RequestInfoTables its)
    zipWithM decodeClosureWithSize raw_its (zip cs raw_cs)

dereferenceStack :: Debuggee -> StackCont -> IO Stack
dereferenceStack d (StackCont sp) = do
  stack <- request d (RequestStack sp)
  let get_bitmap p = request d (RequestBitmap (getInfoTblPtr p))
      get_info_table rc =  lookupInfoTable d rc
  decoded_stack <- decodeStack get_info_table get_bitmap stack
  return decoded_stack


dereferenceConDesc :: Debuggee -> ClosurePtr -> IO ConstrDesc
dereferenceConDesc d i = request d (RequestConstrDesc i)

fullTraversal :: Debuggee -> ClosurePtr -> IO UClosure
fullTraversal d c = do
--  putStrLn ("TIME TO DEREFERENCE: " ++ show c)
  dc <- dereferenceSizedClosure d c
--  putStrLn ("FULL TRAVERSE(" ++ show c ++ ") = " ++ show dc)
  MkFix1 <$> tritraverse (dereferenceConDesc d) (fullStackTraversal d) (fullTraversal d)  dc

fullStackTraversal :: Debuggee -> StackCont -> IO UStack
fullStackTraversal d sc = do
  ds <- dereferenceStack d sc
--  print ("FULL STACK", ds)
  MkFix2 <$> traverse (fullTraversal d) ds

-- | Print a warning if source file (first argument) is newer than the binary (second argument)
warnIfNewer :: FilePath -> FilePath -> IO ()
warnIfNewer fpSrc fpBin = do
    modTimeSource <- getModificationTime fpSrc
    modTimeBinary <- getModificationTime fpBin

    when (modTimeSource > modTimeBinary) $
      hPutStrLn stderr $
        printf "Warning: %s is newer than %s. Code snippets might be wrong!"
        fpSrc
        fpBin


-- | Print out the number of request made for each request type
traceRequestLog :: Debuggee -> IO ()
traceRequestLog d = do
  hm <- readIORef (debuggeeRequestCount d)
  HM.foldMapWithKey (\c n -> putStrLn (show c ++ ": " ++ show n)) hm
