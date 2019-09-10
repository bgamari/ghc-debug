{-# LANGUAGE GADTs #-}

module GHC.Debug.Client
  ( Debuggee
  , DebuggeeAction
  , applyDebuggeeAction
  , withDebuggee
  , withDebuggeeSocket
  , pauseDebuggee
  , request
  , Request(..)
  , getCurrentFrame
  , getInfoTblPtr
  , decodeClosure
  , decodeStack
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
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State.Lazy
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
import Data.Word (Word32)


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

data Debuggee = Debuggee { debuggeeHdl :: Handle
                         , debuggeeInfoTblEnv :: HM.HashMap InfoTablePtr RawInfoTable
                         , debuggeeDwarf :: Maybe Dwarf
                         , debuggeeFilename :: FilePath
                         , debuggeeFrame :: Word32
                         }

type DebuggeeAction a = StateT Debuggee IO a


applyDebuggeeAction :: Debuggee -> DebuggeeAction a -> IO a
applyDebuggeeAction = flip evalStateT


debuggeeProcess :: FilePath -> FilePath -> IO CreateProcess
debuggeeProcess exe sockName = do
  e <- getEnvironment
  return $
    (proc exe []) { env = Just (("GHC_DEBUG_SOCKET", sockName) : e) }

-- | Open a debuggee, this will also read the DWARF information
withDebuggee :: FilePath  -- ^ path to executable
             -> DebuggeeAction a
             -> IO a
withDebuggee exeName action = do
    let sockName = "/tmp/ghc-debug2"
    -- Read DWARF information from the executable
    -- Start the process we want to debug
    cp <- debuggeeProcess exeName sockName
    withCreateProcess cp $ \_ _ _ _ -> do
      dwarf <- getDwarfInfo exeName
    -- Now connect to the socket the debuggeeProcess just started
      withDebuggeeSocket exeName sockName (Just dwarf) action


-- | Open a debuggee's socket directly
withDebuggeeSocket :: FilePath  -- ^ executable name of the debuggee
                   -> FilePath  -- ^ debuggee's socket location
                   -> Maybe Dwarf
                   -> DebuggeeAction a
                   -> IO a
withDebuggeeSocket exeName sockName mdwarf action = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix sockName)
    hdl <- socketToHandle s ReadWriteMode
    evalStateT action (Debuggee hdl mempty mdwarf exeName 0)

-- | Send a request to a 'Debuggee' paused with 'pauseDebuggee'.
request :: Request resp -> DebuggeeAction resp
request req = do
    hdl <- gets debuggeeHdl
    payload <- liftIO $ doRequest hdl req
    -- if we did a successful pause, the payload contains the current frame
    -- number
    case req of
      RequestPause -> modify' $ \d -> d { debuggeeFrame = payload }
      _ -> return ()
    return payload

lookupInfoTable :: RawClosure -> DebuggeeAction (RawInfoTable, RawClosure)
lookupInfoTable rc = do
    let ptr = getInfoTblPtr rc
    itblEnv <- gets debuggeeInfoTblEnv
    case HM.lookup ptr itblEnv of
      Nothing -> do
        [itbl] <- request (RequestInfoTables [ptr])
        infoTblEnv <- gets debuggeeInfoTblEnv
        modify' $ \s -> s { debuggeeInfoTblEnv = HM.insert ptr itbl infoTblEnv }
        return (itbl, rc)
      Just itbl ->  return (itbl, rc)

pauseDebuggee :: DebuggeeAction a -> DebuggeeAction a
pauseDebuggee action = do
    -- TODO: replace poor-mans bracket_ with proper implementation for StateT
    request RequestPause
    rc <- action
    request RequestResume
    return rc

getDwarfInfo :: FilePath -> IO Dwarf
getDwarfInfo fn = do
 (dwarf, warnings) <- Dwarf.Elf.parseElfDwarfADT Dwarf.LittleEndian fn
-- mapM_ print warnings
-- print $ DwarfPretty.dwarf dwarf
 return dwarf

lookupDwarf :: InfoTablePtr -> DebuggeeAction (Maybe ([FilePath], Int, Int))
lookupDwarf (InfoTablePtr w) = do
  mDwarf <- gets debuggeeDwarf
  case mDwarf of
    Nothing -> return Nothing
    Just (Dwarf units) -> return $ asum (map (lookupDwarfUnit (fromBE64 w)) units)

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
lookupDwarfLine w Nothing (d, nd) = do
  if lnmAddress d <= w && w <= lnmAddress nd
    then do
      let (LNEFile file _ _ _) = lnmFiles nd !! (fromIntegral (lnmFile nd) - 1)
      Just (T.unpack file, fromIntegral (lnmLine nd), fromIntegral (lnmColumn nd))
    else Nothing
lookupDwarfLine _ (Just r) _ =  Just r

showFileSnippet :: ([FilePath], Int, Int) -> DebuggeeAction ()
showFileSnippet (fps, l, c) =  do
  dbgFilename <- gets debuggeeFilename
  liftIO $ go dbgFilename fps
  where
    go :: FilePath -> [FilePath] -> IO ()
    go _ [] = putStrLn ("No files could be found: " ++ show fps)
    go dbgFilename (fp:fps) = do
      exists <- liftIO $ doesFileExist $ fp
      -- get file modtime
      if not exists
        then go dbgFilename fps
        else do
          -- TODO: get the modtime of debuggee above
          fp `warnIfNewer` dbgFilename
          src <- zip [1..] . lines <$> readFile fp
          let ctx = take 10 (drop (max (l - 5) 0) src)
          putStrLn (fp <> ":" <> show l <> ":" <> show c)
          mapM_ (\(n, l) ->
           let sn = show n
           in putStrLn (sn <> replicate (5 - length sn) ' ' <> l)) ctx

dereferenceClosure :: ClosurePtr -> DebuggeeAction Closure
dereferenceClosure c = head <$> dereferenceClosures [c]

dereferenceClosures :: [ClosurePtr] -> DebuggeeAction [Closure]
dereferenceClosures cs = do
    raw_cs <- request (RequestClosures cs)
    let its = map getInfoTblPtr raw_cs
    --print $ map (lookupDwarf d) its
    raw_its <- request (RequestInfoTables its)
    return $ map (uncurry decodeClosure) (zip raw_its (zip cs raw_cs))

dereferenceStack :: StackCont -> DebuggeeAction Stack
dereferenceStack (StackCont stack) = do
  liftIO $ print stack
  i <- lookupInfoTable (coerce stack)
  let st_it = decodeInfoTable . fst $ i
  liftIO $ print i
  liftIO $ print st_it
  bt <- request (RequestBitmap (getInfoTblPtr (coerce stack)))
  let decoded_stack = decodeStack stack st_it bt
  liftIO $ print decoded_stack
  return decoded_stack

dereferenceConDesc :: ClosurePtr -> DebuggeeAction ConstrDesc
dereferenceConDesc i = request (RequestConstrDesc i)


fullTraversal :: ClosurePtr -> DebuggeeAction UClosure
fullTraversal c = do
  dc <- dereferenceClosure c
  liftIO $ print dc
  MkFix1 <$> tritraverse dereferenceConDesc fullStackTraversal fullTraversal  dc

fullStackTraversal :: StackCont -> DebuggeeAction UStack
fullStackTraversal sc = do
  ds <- dereferenceStack sc
  liftIO $ print ds
  MkFix2 <$> traverse fullTraversal ds

-- | Print a warning if source file (first argument) is newer than the binary (second argument)
warnIfNewer :: FilePath -> FilePath -> IO ()
warnIfNewer fpSrc fpBin = do
    modTimeSource <- getModificationTime fpSrc
    modTimeBinary <- getModificationTime fpBin
    if modTimeSource > modTimeBinary
    then 
      hPutStrLn stderr $
        printf "Warning: %s is newer than %s. Code snippets might be wrong!"
          fpSrc fpBin
    else
      return ()

-- | Return the current frame number
getCurrentFrame :: DebuggeeAction Word32
getCurrentFrame = gets debuggeeFrame
