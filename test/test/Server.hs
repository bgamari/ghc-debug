module Server (withServer, withStartedDebuggee, withStartedDebuggeeAndHandles, Handles(..)) where

import Control.Concurrent.Async
import Control.Monad
import System.IO
import System.Process
import Control.Concurrent
import Data.List.Extra (trim)
import System.IO.Extra

import GHC.Debug.Client

data Handles = Handles {
  stdin :: Handle,
  stdout :: Handle,
  process :: ProcessHandle
}

type TestFunction a = (Handle -> Handle -> ProcessHandle -> IO a)

withServer :: String  -- ^ executable name
           -> FilePath -- ^ socket name
           -> TestFunction a -- ^ test code
           -> IO a
withServer serverExe socketName f = do
  let cmd:args = words serverExe
  let p = (proc cmd args) {
        std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = CreatePipe,
        env = Just [("GHC_DEBUG_SOCKET",socketName)]
        }
  withCreateProcess p $ runTestFunction f

runTestFunction :: TestFunction a -- ^ test code
                -> Maybe Handle -- ^ stdin
                -> Maybe Handle -- ^ stdout
                -> Maybe Handle -- ^ stderr
                -> ProcessHandle
                -> IO a
runTestFunction f (Just serverIn) (Just serverOut) (Just serverErr) serverProc = do
  hSetBuffering serverErr NoBuffering
  hSetBinaryMode serverErr True
  let errSinkThread = forever $ hGetLine serverErr >>= putStrLn
  withAsync errSinkThread $ \_ -> f serverIn serverOut serverProc
runTestFunction _ _ _ _ _ = error "Starting the process failed"

withStartedDebuggee :: String  -- ^ executable name
             -> (Debuggee -> IO a) -- ^ action
             -> IO a
withStartedDebuggee exeName action = withTempDir $ \ tempDirPath -> do
  let socketName = tempDirPath ++ "/ghc-debug"
  withServer exeName socketName $ \serverIn serverOut serverProc -> do
    prog <- readCreateProcess serverExePathCmd []
    withDebuggee (trim prog) socketName action
  where
    serverExePathCmd = shell $ "which " ++ exeName

withStartedDebuggeeAndHandles :: String  -- ^ executable name
             -> (Handles -> Debuggee -> IO a) -- ^ action
             -> IO a
withStartedDebuggeeAndHandles exeName action = withTempDir $ \ tempDirPath -> do
  let socketName = tempDirPath ++ "/ghc-debug"
  withServer exeName socketName $ \serverIn serverOut serverProc -> do
    prog <- readCreateProcess serverExePathCmd []
    let handles = Handles serverIn serverOut serverProc
    withDebuggee (trim prog) socketName (action handles)
  where
    serverExePathCmd = shell $ "which " ++ exeName
