{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module GHC.Debug.Client.Monad
  ( DebugMonad(..)
  , run
  , DebugM
  , traceWrite
  , runTrace
  , withDebuggee
  , withDebuggeeSocket
  , outputRequestLog
  ) where

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

run :: DebugEnv DebugM -> DebugM a -> IO a
run = runDebug

-- | Open a debuggee
withDebuggee :: FilePath  -- ^ path to executable
             -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
             -> (DebugEnv DebugM -> IO a)
             -> IO a
withDebuggee exeName socketName action = do
    -- Start the process we want to debug
    cp <- debuggeeProcess exeName socketName
    withCreateProcess cp $ \_ _ _ _ -> do
    -- Now connect to the socket the debuggeeProcess just started
      withDebuggeeSocket exeName socketName action

-- | Open a debuggee's socket directly
withDebuggeeSocket :: FilePath  -- ^ executable name of the debuggee
                   -> FilePath  -- ^ debuggee's socket location
                   -> (DebugEnv DebugM -> IO a)
                   -> IO a
withDebuggeeSocket exeName sockName action = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix sockName)
    hdl <- socketToHandle s ReadWriteMode
    new_env <- newEnv @DebugM exeName sockName hdl
    action new_env

debuggeeProcess :: FilePath -> FilePath -> IO CreateProcess
debuggeeProcess exe sockName = do
  e <- getEnvironment
  return $
    (proc exe []) { env = Just (("GHC_DEBUG_SOCKET", sockName) : e) }

outputRequestLog :: DebugEnv DebugM -> IO ()
outputRequestLog = printRequestLog @DebugM

