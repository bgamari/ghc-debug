module GHC.Debug.Client
  ( Debuggee
  , withDebuggee
  , withDebuggeeSocket
  , pauseDebuggee
  , request
  , Request(..)
  , getInfoTblPtr
  , decodeClosure
  , decodeStack
  , FieldValue(..)
  , decodeInfoTable
  , lookupInfoTable
  , getDwarfInfo
  , lookupDwarf
  , showFileSnippet
  , subtractClosurePtr
  , rawClosureSize
  , dropRawClosure
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

data Debuggee = Debuggee { debuggeeHdl :: Handle
                         , debuggeeInfoTblEnv :: MVar (HM.HashMap InfoTablePtr RawInfoTable)
                         , debuggeeDwarf :: Maybe Dwarf
                         }


debuggeeProcess :: FilePath -> FilePath -> IO CreateProcess
debuggeeProcess exe sockName = do
  e <- getEnvironment
  return $
    (proc exe []) { env = Just (("GHC_DEBUG_SOCKET", sockName) : e) }

-- | Open a debuggee, this will also read the DWARF information
withDebuggee :: FilePath  -- ^ path to executable
             -> (Debuggee -> IO a)
             -> IO a
withDebuggee exeName action = do
    let sockName = "/tmp/ghc-debug2"
    -- Read DWARF information from the executable
    -- Start the process we want to debug
    cp <- debuggeeProcess exeName sockName
    withCreateProcess cp $ \_ _ _ _ -> do
      dwarf <- getDwarfInfo exeName
    -- Now connect to the socket the debuggeeProcess just started
      withDebuggeeSocket sockName (Just dwarf) action


-- | Open a debuggee's socket directly
withDebuggeeSocket :: FilePath  -- ^ debuggee's socket location
                   -> Maybe Dwarf
                   -> (Debuggee -> IO a)
                   -> IO a
withDebuggeeSocket sockName mdwarf action = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix sockName)
    hdl <- socketToHandle s ReadWriteMode
    infoTableEnv <- newMVar mempty
    action (Debuggee hdl infoTableEnv mdwarf)

-- | Send a request to a 'Debuggee' paused with 'pauseDebuggee'.
request :: Debuggee -> Request resp -> IO resp
request (Debuggee hdl _ _) req = doRequest hdl req

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

lookupDwarf :: Debuggee -> InfoTablePtr -> Maybe (FilePath, Int, Int)
lookupDwarf d (InfoTablePtr w) = do
  (Dwarf units) <- debuggeeDwarf d
  asum (map (lookupDwarfUnit (fromBE64 w)) units)

lookupDwarfUnit :: Word64 -> Boxed CompilationUnit -> Maybe (FilePath, Int, Int)
lookupDwarfUnit w (Boxed _ cu) = do
  low <- cuLowPc cu
  high <- cuHighPc cu
  guard (low <= w && w <= high)
  LNE _ _fs ls <- cuLineNumInfo cu
  (fp, l, c) <- foldl' (lookupDwarfLine w) Nothing (zip ls (tail ls))
  return (T.unpack (cuCompDir cu) </> fp, l , c)

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

showFileSnippet :: (FilePath, Int, Int) -> IO ()
showFileSnippet (fp, l, c) = do
  exists <- doesFileExist fp
  if not exists
    then putStrLn ("Can't open file: " ++ fp)
    else do
      src <- zip [1..] . lines <$> readFile fp
      let ctx = take 10 (drop (max (l - 5) 0) src)
      putStrLn (fp <> ":" <> show l <> ":" <> show c)
      mapM_ (\(n, l) ->
       let sn = show n
       in putStrLn (sn <> replicate (5 - length sn) ' ' <> l)) ctx





