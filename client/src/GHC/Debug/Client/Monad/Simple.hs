{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
-- | This module provides a simple implementation, which can be a lot faster if
-- network latency is not an issue.
module GHC.Debug.Client.Monad.Simple
  ( Debuggee(..)
  , DebugM
  ) where

import Control.Concurrent
import GHC.Debug.Types
import qualified Data.HashMap.Strict as HM
import System.IO
import Data.IORef

import GHC.Debug.Client.BlockCache
import GHC.Debug.Client.RequestCache
import GHC.Debug.Client.Monad.Class

import Control.Monad.Reader


data Debuggee = Debuggee { debuggeeFilename :: FilePath
                         -- Keep track of how many of each request we make
                         , debuggeeRequestCount :: IORef (HM.HashMap CommandId Int)
                         , debuggeeBlockCache :: IORef BlockCache
                         , debuggeeRequestCache :: MVar RequestCache
                         , debuggeeHandle :: Handle
                         }



instance DebugMonad DebugM where
  type DebugEnv DebugM = Debuggee
  request = simpleReq
  requestBlock = blockReq
  traceMsg = liftIO . putStrLn
  printRequestLog _ = putStrLn "No request log in Simple(TM) mode"
  runDebug = runSimple
  runDebugTrace e a = (,[]) <$> runDebug e a
  newEnv = mkEnv


runSimple :: Debuggee -> DebugM a -> IO a
runSimple d (DebugM a) = runReaderT a d

mkEnv :: FilePath -> FilePath -> Handle -> IO Debuggee
mkEnv exeName _sockName h = do
  count <- newIORef HM.empty
  bc <- newIORef emptyBlockCache
  rc <- newMVar emptyRequestCache
  return $ Debuggee exeName count bc rc h

simpleReq :: Request resp -> DebugM resp
simpleReq req = do
  rc_var <- asks debuggeeRequestCache
  rc <- liftIO $ readMVar rc_var
  case lookupReq req rc of
    Just res -> return res
    Nothing -> do
      h <- asks debuggeeHandle
      res <- liftIO $ doRequest h req
      liftIO $ modifyMVar_ rc_var (return . cacheReq req res)
      return res

blockReq :: BlockCacheRequest resp -> DebugM resp
blockReq req = do
  hdl <- asks debuggeeHandle
  bc  <- asks debuggeeBlockCache
  liftIO $ handleBlockReq hdl bc req

newtype DebugM a = DebugM (ReaderT Debuggee IO a)
                    deriving (MonadReader Debuggee, MonadFail, MonadIO, Functor, Applicative, Monad)


