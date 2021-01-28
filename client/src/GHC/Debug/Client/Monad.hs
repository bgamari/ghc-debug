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
  , Debuggee
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
  , snapshotRun
    -- * Logging
  , outputRequestLog
  ) where

import Control.Exception (finally)
import Network.Socket
import System.IO
import System.Process
import System.Environment
import GHC.Debug.Client.Monad.Class
import GHC.Debug.Types (Request(..))
import qualified GHC.Debug.Client.Monad.Haxl as H
import qualified GHC.Debug.Client.Monad.Simple as S
import System.IO

-- Modify this to switch between the haxl/non-haxl implementations
-- type DebugM = H.DebugM
type DebugM = S.DebugM

newtype Debuggee = Debuggee { debuggeeEnv :: DebugEnv DebugM }

runTrace :: Debuggee -> DebugM a -> IO a
runTrace (Debuggee e) act = do
  (r, ws) <- runDebugTrace e act
  mapM putStrLn ws
  return r

traceWrite :: DebugMonad m => Show a => a -> m ()
traceWrite = traceMsg . show

-- | Run a @DebugM a@ in the given environment.
run :: Debuggee -> DebugM a -> IO a
run (Debuggee d) = runDebug d

-- | Bracketed version of @debuggeeRun@. Runs a debuggee, connects to it, runs
-- the action, kills the process, then closes the debuggee.
withDebuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
                -> FilePath  -- ^ filename of socket (e.g. @"\/tmp\/ghc-debug"@)
                -> (Debuggee -> IO a)
                -> IO a
withDebuggeeRun exeName socketName action = do
    -- Start the process we want to debug
    cp <- debuggeeProcess exeName socketName
    withCreateProcess cp $ \_ _ _ _ -> do
    -- Now connect to the socket the debuggeeProcess just started
      withDebuggeeConnect socketName action

-- | Bracketed version of @debuggeeConnect@. Connects to a debuggee, runs the
-- action, then closes the debuggee.
withDebuggeeConnect :: FilePath  -- ^ filename of socket (e.g. @"\/tmp\/ghc-debug"@)
                    -> (Debuggee -> IO a)
                    -> IO a
withDebuggeeConnect socketName action = do
    new_env <- debuggeeConnect socketName
    action new_env
      `finally`
      debuggeeClose new_env

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
            -> FilePath  -- ^ filename of socket (e.g. @"\/tmp\/ghc-debug"@)
            -> IO Debuggee
debuggeeRun exeName socketName = do
    -- Start the process we want to debug
    _ <- createProcess =<< debuggeeProcess exeName socketName
    -- Now connect to the socket the debuggeeProcess just started
    debuggeeConnect socketName

-- | Connect to a debuggee on the given socket. Use @debuggeeClose@ when you're done.
debuggeeConnect :: FilePath  -- ^ filename of socket (e.g. @"\/tmp\/ghc-debug"@)
                -> IO Debuggee
debuggeeConnect socketName = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix socketName)
    hdl <- socketToHandle s ReadWriteMode
    new_env <- newEnv @DebugM (SocketMode hdl)
    return (Debuggee new_env)

-- | Create a debuggee by loading a snapshot created by 'snapshot'.
snapshotInit :: FilePath -> IO Debuggee
snapshotInit fp = Debuggee <$> newEnv @DebugM (SnapshotMode fp)

-- | Start an analysis session using a snapshot. This will not connect to a
-- debuggee. The snapshot is created by 'snapshot'.
snapshotRun :: FilePath -> (Debuggee -> IO a) -> IO a
snapshotRun fp k = do
  denv <- snapshotInit fp
  k denv

-- | Close the connection to the debuggee.
debuggeeClose :: Debuggee -> IO ()
debuggeeClose d = run d $ request RequestResume

debuggeeProcess :: FilePath -> FilePath -> IO CreateProcess
debuggeeProcess exe sockName = do
  e <- getEnvironment
  return $
    (proc exe []) { env = Just (("GHC_DEBUG_SOCKET", sockName) : e) }

outputRequestLog :: Debuggee -> IO ()
outputRequestLog = printRequestLog @DebugM . debuggeeEnv

