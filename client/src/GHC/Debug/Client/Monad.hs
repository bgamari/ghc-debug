{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.Debug.Client.Monad
  ( Debuggee(..)
  , withDebuggee
  , withDebuggeeSocket
  , request
  , Request(..)
  , logRequest


  , DebugM
  , run
  , runTrace
  , Env(..)
  , traceWrite
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

import Haxl.Core hiding (Request, env)
import Data.Typeable

import Data.IORef


data Debuggee = Debuggee { debuggeeHdl :: Handle
                         , debuggeeInfoTblEnv :: MVar (HM.HashMap InfoTablePtr RawInfoTable)
                         , debuggeeDwarf :: Maybe Dwarf
                         , debuggeeFilename :: FilePath
                         -- Keep track of how many of each request we make
                         , debuggeeRequestCount :: IORef (HM.HashMap CommandId Int)
                         }

type DebugM a = GenHaxl Debuggee String a

run :: Env Debuggee String -> DebugM a -> IO a
run = runHaxl

runTrace :: Env Debuggee String -> DebugM a -> IO a
runTrace e act = do
  (r, ws) <- runHaxlWithWrites e act
  mapM putStrLn ws
  return r

traceWrite :: Show a => a -> GenHaxl u String ()
traceWrite = tellWrite . show


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
             -> (Env Debuggee String -> IO a)
             -> IO a
withDebuggee exeName socketName action = do
    -- Read DWARF information from the executable
    -- Start the process we want to debug
    cp <- debuggeeProcess exeName socketName
    withCreateProcess cp $ \_ _ _ _ -> do
      dwarf <- getDwarfInfo exeName
    -- Now connect to the socket the debuggeeProcess just started
      withDebuggeeSocket exeName socketName (Just dwarf) action

getDwarfInfo :: FilePath -> IO Dwarf
getDwarfInfo fn = do
 (dwarf, warnings) <- Dwarf.Elf.parseElfDwarfADT Dwarf.LittleEndian fn
-- mapM_ print warnings
-- print $ DwarfPretty.dwarf dwarf
 return dwarf


-- | Open a debuggee's socket directly
withDebuggeeSocket :: FilePath  -- ^ executable name of the debuggee
                   -> FilePath  -- ^ debuggee's socket location
                   -> Maybe Dwarf
                   -> (Env Debuggee String -> IO a)
                   -> IO a
withDebuggeeSocket exeName sockName mdwarf action = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix sockName)
    hdl <- socketToHandle s ReadWriteMode
    infoTableEnv <- newMVar mempty
    requestMap <- newIORef HM.empty
    let ss = stateSet (RequestState hdl) stateEmpty
    new_env <- initEnv ss (Debuggee hdl infoTableEnv mdwarf exeName requestMap)
    -- Turn on data fetch stats
    let new_flags = defaultFlags { report = 5 }
    action (new_env { Haxl.Core.flags = new_flags })

-- | Send a request to a 'Debuggee' paused with 'pauseDebuggee'.
request :: (Show resp, Typeable resp) => Request resp -> DebugM resp
request = dataFetch

instance StateKey Request where
  data State Request = RequestState Handle

instance DataSourceName Request where
  dataSourceName Proxy = "ghc-debug"

instance ShowP Request where
  showp = show

instance DataSource u Request where
  fetch (RequestState h) fs u = SyncFetch (\rs -> mapM_ do_one rs)
    where
      do_one (BlockedFetch req resp) = do
        case req of
          RequestInfoTables [] -> putSuccess resp []
          _ -> do
            res <- doRequest h req
            putSuccess resp res
