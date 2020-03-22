module Server (withServer) where

import Control.Concurrent.Async
import Control.Monad
import System.IO
import System.Process
import Control.Concurrent

withServer :: String -> FilePath -> Bool -> (Handle -> Handle -> ProcessHandle -> IO a) -> IO a
withServer serverExe socketName logStdErr f = do
  let cmd:args = words serverExe
  let p = (proc cmd args) {
        std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = CreatePipe,
        env = Just [("GHC_DEBUG_SOCKET",socketName)]
        }
  withCreateProcess p $ \(Just serverIn) (Just serverOut) (Just serverErr) serverProc -> do
    -- Need to continuously consume to stderr else it gets blocked
    -- Can't pass NoStream either to std_err
    hSetBuffering serverErr NoBuffering
    hSetBinaryMode serverErr True
    let errSinkThread = forever $ hGetLine serverErr >>= when logStdErr . putStrLn
    withAsync errSinkThread $ \_ -> do
      f serverIn serverOut serverProc
