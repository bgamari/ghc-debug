{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module GHC.Debug.Client.Monad
  ( DebugMonad(..)
  , run
  , DebugM
  , traceWrite
  , runTrace
    -- * Running/Connecting to a debuggee
  , withDebuggeeRun
  , withDebuggeeConnect
  , debuggeeRun
  , debuggeeConnect
  , debuggeeClose
  -- * Snapshot run
  , snapshotInit
    -- * Logging
  , outputRequestLog
  ) where

import Control.Exception (finally)
import Network.Socket
import System.IO
import System.Process
import System.Environment
import GHC.Debug.Client.Monad.Class
import qualified GHC.Debug.Client.Monad.Haxl as H
import qualified GHC.Debug.Client.Monad.Simple as S

-- Modify this to switch between the haxl/non-haxl implementations
-- type DebugM = H.DebugM
type DebugM = S.DebugM

runTrace :: DebugEnv DebugM -> DebugM a -> IO a
runTrace e act = do
  (r, ws) <- runDebugTrace e act
  mapM putStrLn ws
  return r

traceWrite :: DebugMonad m => Show a => a -> m ()
traceWrite = traceMsg . show

-- | Run a @DebugM a@ in the given environment.
run :: DebugEnv DebugM -> DebugM a -> IO a
run = runDebug

-- | Bracketed version of @debuggeeRun@. Runs a debuggee, connects to it, runs
-- the action, kills the process, then closes the debuggee.
withDebuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
                -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                -> (DebugEnv DebugM -> IO a)
                -> IO a
withDebuggeeRun exeName socketName action = do
    -- Start the process we want to debug
    cp <- debuggeeProcess exeName socketName
    withCreateProcess cp $ \_ _ _ _ -> do
    -- Now connect to the socket the debuggeeProcess just started
      withDebuggeeConnect exeName socketName action

-- | Bracketed version of @debuggeeConnect@. Connects to a debuggee, runs the
-- action, then closes the debuggee.
withDebuggeeConnect :: FilePath  -- ^ executable name of the debuggee
                   -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                   -> (DebugEnv DebugM -> IO a)
                   -> IO a
withDebuggeeConnect exeName socketName action = do
    new_env <- debuggeeConnect  exeName socketName
    action new_env
      `finally`
      debuggeeClose new_env

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
            -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
            -> IO (DebugEnv DebugM)
debuggeeRun exeName socketName = do
    -- Start the process we want to debug
    _ <- createProcess =<< debuggeeProcess exeName socketName
    -- Now connect to the socket the debuggeeProcess just started
    debuggeeConnect exeName socketName

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeConnect :: FilePath  -- ^ path to executable to run as the debuggee
                -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                -> IO (DebugEnv DebugM)
debuggeeConnect exeName socketName = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix socketName)
    hdl <- socketToHandle s ReadWriteMode
    new_env <- newEnv @DebugM (Socket hdl)
    return new_env

snapshotInit :: FilePath -> IO (DebugEnv DebugM)
snapshotInit fp = newEnv @DebugM (Snapshot fp)

-- | Close the connection to the debuggee.
debuggeeClose :: DebugEnv DebugM -> IO ()
debuggeeClose _ = putStrLn "TODO: debuggeeClose: cleanly disconnect from debuggee"

debuggeeProcess :: FilePath -> FilePath -> IO CreateProcess
debuggeeProcess exe sockName = do
  e <- getEnvironment
  return $
    (proc exe []) { env = Just (("GHC_DEBUG_SOCKET", sockName) : e) }

outputRequestLog :: DebugEnv DebugM -> IO ()
outputRequestLog = printRequestLog @DebugM

