module Server (withServer, withStartedDebuggee, withStartedDebuggeeAndHandles, Handles(..)) where

import Control.Concurrent.Async
import Control.Monad
import System.IO
import System.Process
import Control.Concurrent
import Data.List.Extra (trim)
import System.IO.Extra

import GHC.Debug.Client

withServer :: String -> FilePath -> Bool -> (Handle -> Handle -> ProcessHandle -> IO a) -> IO a
withServer serverExe socketName logStdErr f = do
  let cmd:args = words serverExe
  let p = (proc cmd args) {
        std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = CreatePipe,
        env = Just [("GHC_DEBUG_SOCKET",socketName)]
        }
-- TODO pattern match case where one or more handles are not available (-> error with message)
  withCreateProcess p $ \(Just serverIn) (Just serverOut) (Just serverErr) serverProc -> do
    -- Need to continuously consume to stderr else it gets blocked
    -- Can't pass NoStream either to std_err
    hSetBuffering serverErr NoBuffering
    hSetBinaryMode serverErr True
    let errSinkThread = forever $ hGetLine serverErr >>= when logStdErr . putStrLn
    withAsync errSinkThread $ \_ -> f serverIn serverOut serverProc


withStartedDebuggee :: String  -- ^ executable name
             -> (Debuggee -> IO a) -- ^ action
             -> IO a
withStartedDebuggee exeName action = withTempDir $ \ tempDirPath -> do
  let socketName = tempDirPath ++ "/ghc-debug"
  withServer exeName socketName True $ \serverIn serverOut serverProc -> do
    prog <- readCreateProcess serverExePathCmd []
    withDebuggee (trim prog) socketName action
  where
    serverExePathCmd = shell $ "which " ++ exeName

data Handles = Handles {
  stdin :: Handle,
  stdout :: Handle,
  process :: ProcessHandle
                       }

withStartedDebuggeeAndHandles :: String  -- ^ executable name
             -> (Handles -> Debuggee -> IO a) -- ^ action
             -> IO a
withStartedDebuggeeAndHandles exeName action = withTempDir $ \ tempDirPath -> do
  let socketName = tempDirPath ++ "/ghc-debug"
  withServer exeName socketName True $ \serverIn serverOut serverProc -> do
    prog <- readCreateProcess serverExePathCmd []
    let handles = Handles serverIn serverOut serverProc
    withDebuggee (trim prog) socketName (action handles)
  where
    serverExePathCmd = shell $ "which " ++ exeName
