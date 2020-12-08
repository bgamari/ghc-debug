{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module provides a simple implementation, which can be a lot faster if
-- network latency is not an issue.
module GHC.Debug.Client.Monad.Simple
  ( Debuggee
  , DebugM
  ) where

import Control.Concurrent
import GHC.Debug.Types
import qualified Data.HashMap.Strict as HM
import System.IO
import Data.IORef
import Data.List
import Data.Ord

import GHC.Debug.Client.BlockCache
import GHC.Debug.Client.RequestCache
import GHC.Debug.Client.Monad.Class

import Control.Monad.Reader


data Debuggee = Debuggee { debuggeeFilename :: FilePath
                         -- Keep track of how many of each request we make
                         , debuggeeRequestCount :: Maybe (IORef (HM.HashMap CommandId FetchStats))
                         , debuggeeBlockCache :: IORef BlockCache
                         , debuggeeRequestCache :: MVar RequestCache
                         , debuggeeHandle :: MVar Handle
                         }

data FetchStats = FetchStats { _networkRequests :: !Int, _cachedRequests :: !Int }

logRequestIO :: Bool -> IORef (HM.HashMap CommandId FetchStats) -> Request resp -> IO ()
logRequestIO cached hmref req =
  atomicModifyIORef' hmref ((,()) . HM.alter alter_fn (requestCommandId req))

  where
    alter_fn = Just . maybe emptyFetchStats upd_fn
    emptyFetchStats = FetchStats 1 0
    upd_fn (FetchStats nr cr)
      | cached = FetchStats nr (cr + 1)
      | otherwise = FetchStats (nr + 1) cr

logRequest :: Bool -> Request resp -> ReaderT Debuggee IO ()
logRequest cached req = do
  mhm <- asks debuggeeRequestCount
  case mhm of
    Just hm -> liftIO $ logRequestIO cached hm req
    Nothing -> return ()

ppRequestLog :: HM.HashMap CommandId FetchStats -> String
ppRequestLog hm = unlines (map row items)
  where
    row (cid, FetchStats net cache) = unwords [show cid ++ ":", show net, show cache]
    items = sortBy (comparing fst) (HM.toList hm)


instance DebugMonad DebugM where
  type DebugEnv DebugM = Debuggee
  request = DebugM . simpleReq
  requestBlock = blockReq
  traceMsg = DebugM . liftIO . putStrLn
  printRequestLog e = do
    case debuggeeRequestCount e of
      Just hm_ref -> do
        readIORef hm_ref >>= putStrLn . ppRequestLog
      Nothing -> putStrLn "No request log in Simple(TM) mode"
  runDebug = runSimple
  runDebugTrace e a = (,[]) <$> runDebug e a
  newEnv = mkEnv


runSimple :: Debuggee -> DebugM a -> IO a
runSimple d (DebugM a) = runReaderT a d

mkEnv :: FilePath -> FilePath -> Handle -> IO Debuggee
mkEnv exeName _sockName h = do
  let enable_stats = False
  mcount <- if enable_stats then Just <$> newIORef HM.empty else return Nothing
  bc <- newIORef emptyBlockCache
  rc <- newMVar emptyRequestCache
  mhdl <-  newMVar h
  return $ Debuggee exeName mcount bc rc mhdl

simpleReq :: Request resp -> ReaderT Debuggee IO resp
simpleReq req | isWriteRequest req = ask >>= \Debuggee{..} -> liftIO $ do
  atomicModifyIORef' debuggeeBlockCache (const (emptyBlockCache, ()))
  modifyMVar_ debuggeeRequestCache (return . const emptyRequestCache)
  doRequest debuggeeHandle req
simpleReq req = do
  rc_var <- asks debuggeeRequestCache
  rc <- liftIO $ readMVar rc_var
  case lookupReq req rc of
    Just res -> do
      logRequest True req
      return res
    Nothing -> do
      h <- asks debuggeeHandle
      res <- liftIO $ doRequest h req
      liftIO $ modifyMVar_ rc_var (return . cacheReq req res)
      logRequest False req
      return res

blockReq :: BlockCacheRequest resp -> DebugM resp
blockReq req = DebugM $ do
  hdl <- asks debuggeeHandle
  bc  <- asks debuggeeBlockCache
  liftIO $ handleBlockReq hdl bc req

newtype DebugM a = DebugM (ReaderT Debuggee IO a)
                   -- Only derive the instances that DebugMonad needs
                    deriving (MonadFail, Functor, Applicative, Monad)


