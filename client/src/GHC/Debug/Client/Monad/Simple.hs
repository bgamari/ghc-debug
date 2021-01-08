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
import Data.Binary
--import Debug.Trace


data Debuggee = Debuggee { -- Keep track of how many of each request we make
                           debuggeeRequestCount :: Maybe (IORef (HM.HashMap CommandId FetchStats))
                         , debuggeeBlockCache :: IORef BlockCache
                         , debuggeeRequestCache :: MVar RequestCache
                         , debuggeeHandle :: Maybe (MVar Handle)
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

data Snapshot = Snapshot {
                    _version :: Word32
                  , _rqc :: RequestCache
                  , _bc  :: BlockCache
                  }

snapshotVersion :: Word32
snapshotVersion = 0

instance Binary Snapshot where
  get = do
    v <- get
    if v == snapshotVersion
      then Snapshot <$> pure v <*> get <*> get
      else fail ("Wrong snapshot version.\nGot: " ++ show v ++ "\nExpected: " ++ show snapshotVersion)
  put (Snapshot v c1 c2) = do
    put v
    put c1
    put c2


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
  newEnv m = case m of
               SnapshotMode f -> mkSnapshotEnv f
               SocketMode h -> mkHandleEnv h

  loadCache fp = DebugM $ do
    (Snapshot _ new_req_cache new_block_cache) <- lift $ decodeFile fp
    Debuggee{..} <- ask
    lift $ swapMVar debuggeeRequestCache new_req_cache
    lift $ writeIORef debuggeeBlockCache new_block_cache

  saveCache fp = DebugM $ do
    Debuggee{..} <- ask
    Just req_cache <- lift $ tryReadMVar debuggeeRequestCache
    block_cache <- lift $ readIORef debuggeeBlockCache
    lift $ encodeFile fp (Snapshot snapshotVersion req_cache block_cache)





runSimple :: Debuggee -> DebugM a -> IO a
runSimple d (DebugM a) = runReaderT a d

mkEnv :: (RequestCache, BlockCache) -> Maybe Handle -> IO Debuggee
mkEnv (req_c, block_c) h = do
  let enable_stats = False
  mcount <- if enable_stats then Just <$> newIORef HM.empty else return Nothing
  bc <- newIORef  block_c
  rc <- newMVar req_c
  mhdl <-  traverse newMVar h
  return $ Debuggee mcount bc rc mhdl

mkHandleEnv :: Handle -> IO Debuggee
mkHandleEnv h = mkEnv (emptyRequestCache, emptyBlockCache) (Just h)

mkSnapshotEnv :: FilePath -> IO Debuggee
mkSnapshotEnv fp = do
  Snapshot _ req_c block_c <- decodeFile fp
  mkEnv (req_c, block_c) Nothing



-- TODO: Sending multiple pauses will clear the cache, should keep track of
-- the pause state and only clear caches if the state changes.
simpleReq :: Request resp -> ReaderT Debuggee IO resp
simpleReq req | isWriteRequest req = ask >>= \Debuggee{..} -> liftIO $ withWriteRequest req (error "non-write") $ \wreq -> do
  case debuggeeHandle of
    Just h -> do
      atomicModifyIORef' debuggeeBlockCache (const (emptyBlockCache, ()))
      modifyMVar_ debuggeeRequestCache (return . clearMovableRequests)
      doRequest h wreq
    -- Ignore write requests in snapshot mode
    Nothing -> return ()
simpleReq req = do
  rc_var <- asks debuggeeRequestCache
  rc <- liftIO $ readMVar rc_var
  case lookupReq req rc of
    Just res -> do
      logRequest True req
      return res
    Nothing -> do
      mh <- asks debuggeeHandle
      case mh of
        Nothing -> error ("Cache Miss:" ++ show req)
        Just h -> do
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


