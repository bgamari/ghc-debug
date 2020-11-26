{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.Debug.Client.Monad.Haxl
  ( Debuggee(..)
  , Request(..)
  , DebugM
  , Env(..)
  ) where

import GHC.Debug.Types
import qualified Data.HashMap.Strict as HM
import System.IO


import GHC.Debug.Client.BlockCache
import GHC.Debug.Client.Monad.Class

import Haxl.Core hiding (Request, env)
import Data.Typeable

import Data.IORef

data Debuggee = Debuggee { debuggeeFilename :: FilePath
                         -- Keep track of how many of each request we make
                         , debuggeeRequestCount :: IORef (HM.HashMap CommandId Int)
                         , debuggeeBatchMode :: BatchMode
                         }

data BatchMode = Batch | OneByOne deriving (Eq, Show)

instance DebugMonad (GenHaxl Debuggee String) where
  type DebugEnv (GenHaxl Debuggee String) = Env Debuggee String
  request = dataFetch
  requestBlock = dataFetch
  traceMsg = tellWrite
  printRequestLog = traceRequestLog
  runDebug = runHaxl
  runDebugTrace = runHaxlWithWrites
  newEnv = mkEnv

type DebugM = GenHaxl Debuggee String


{- MP:
- In some profiles it seemed that the caching step was causing quite a bit
- of overhead, but still using the cache is about 2-3x faster than without
- a cache. (ie using `doRequest` directly or `uncachedRequest`.
-}

-- | Send a request to a 'Debuggee' paused with 'withPause'.
--request :: (Show resp, Typeable resp) => Request resp -> DebugM resp
--request = dataFetch


instance StateKey Request where
  data State Request = RequestState Handle

instance DataSourceName Request where
  dataSourceName Proxy = "ghc-debug"

instance ShowP Request where
  showp = show


-- | Group together RequestClosures and RequestInfoTables to avoid
-- some context switching.
groupFetches :: Handle -> [([ClosurePtr], ResultVar [RawClosure])] -> [([InfoTablePtr], ResultVar [(StgInfoTableWithPtr, RawInfoTable)])] -> [BlockedFetch Request] -> [BlockedFetch Request] -> IO ()
groupFetches h cs is todo [] = dispatch h cs is (reverse todo)
groupFetches h cs is todo (b@(BlockedFetch r resp) : bs) =
  case r of
    RequestInfoTables is' -> groupFetches h cs ((is', resp):is) todo bs
    RequestClosures cs' -> groupFetches h ((cs', resp):cs) is todo bs
    _ -> groupFetches h cs is (b:todo) bs

dispatch :: Handle
         -> [([ClosurePtr], ResultVar [RawClosure])]
         -> [([InfoTablePtr], ResultVar [(StgInfoTableWithPtr, RawInfoTable)])]
         -> [BlockedFetch Request]
         -> IO ()
dispatch h cs its other = do
  mapM_ do_one other
  -- These can be used to inspect how much batching is happening
--  print ("BATCHED_CLOSURES", length cs, map fst cs)
--  print (length its, map fst its)
  do_many RequestClosures cs
  do_many RequestInfoTables its
  where
    do_one (BlockedFetch req resp) = do
      res <- doRequest h req
      putSuccess resp res

    do_many :: ([a] -> Request [b]) -> [([a], ResultVar [b])] -> IO ()
    do_many _ [] = return ()
    do_many mk_req ms = do
      let req = mk_req (concatMap fst ms)
      results <- doRequest h req
      recordResults results ms



-- | Write the correct number of results to each result var
recordResults :: [a] -> [([b], ResultVar [a])] -> IO ()
recordResults [] [] = return ()
recordResults res ((length -> n, rvar):xs) =
  putSuccess rvar here >> recordResults later xs
  where
    (here, later) = splitAt n res
recordResults _ _ = error ("Impossible recordResults")


_singleFetches :: Handle -> [BlockedFetch Request] -> IO ()
_singleFetches h bs = mapM_ do_one bs
      where
        do_one (BlockedFetch req resp) = do
          res <- doRequest h req
          putSuccess resp res

instance DataSource Debuggee Request where
  fetch (RequestState h) _fs u =
    -- Grouping together fetches only shaves off about 0.01s on the simple
    -- benchmark
    SyncFetch $
      case debuggeeBatchMode u of
        Batch -> groupFetches h [] [] []
        OneByOne -> _singleFetches h



instance StateKey BlockCacheRequest where
  data State BlockCacheRequest = BCRequestState (IORef BlockCache) Handle

instance DataSourceName BlockCacheRequest where
  dataSourceName Proxy = "block-cache"

instance ShowP BlockCacheRequest where
  showp = show


instance DataSource u BlockCacheRequest where
  fetch (BCRequestState ref h) _fs _u =
    SyncFetch (mapM_ do_one)
    where
      do_one :: BlockedFetch BlockCacheRequest -> IO ()
      do_one (BlockedFetch bcr resp) = do
        res <- handleBlockReq h ref bcr
        putSuccess resp res



mkEnv :: FilePath -> FilePath -> Handle -> IO (Env Debuggee String)
mkEnv exeName _sock hdl = do
  requestMap <- newIORef HM.empty
  bc <- newIORef emptyBlockCache
  let ss = stateSet (BCRequestState bc hdl) (stateSet (RequestState hdl) stateEmpty)
  new_env <- initEnv ss (Debuggee exeName requestMap Batch)
  -- Turn on data fetch stats with report = 3
  let new_flags = defaultFlags { report = 0 }
  return $ new_env { Haxl.Core.flags = new_flags }

-- | Print out the number of request made for each request type
traceRequestLog :: Env u w -> IO ()
traceRequestLog d = do
  s <- readIORef (statsRef d)
  putStrLn (ppStats s)

_traceProfile :: Env u w -> IO ()
_traceProfile e = do
  p <- readIORef (profRef e)
  print (profile p)
