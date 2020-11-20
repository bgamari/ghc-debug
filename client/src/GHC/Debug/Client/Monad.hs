{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
module GHC.Debug.Client.Monad
  ( Debuggee(..)
  , withDebuggee
  , withDebuggeeSocket
  , request
  , Request(..)
  , logRequest


  , DebugM
  , run
  , runTrace
  , Env(..)
  , traceWrite
  , traceMsg

  , BlockCacheRequest(..)
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.Debug.Types
import GHC.Debug.Decode
import GHC.Debug.Decode.Stack
import Network.Socket
import qualified Data.HashMap.Strict as HM
import System.IO
import Data.Word
import Data.Maybe
import System.Endian
import Data.Foldable
import Data.Coerce
import Data.Bitraversable
import Data.Hashable


import qualified Data.Text  as T
import Data.List
import System.Process
import System.Environment
import System.FilePath
import System.Directory
import GHC.Debug.Client.BlockCache

import Haxl.Core hiding (Request, env)
import Data.Typeable

import Data.IORef


data Debuggee = Debuggee { debuggeeFilename :: FilePath
                         -- Keep track of how many of each request we make
                         , debuggeeRequestCount :: IORef (HM.HashMap CommandId Int)
                         , debuggeeBatchMode :: BatchMode
                         }

data BatchMode = Batch | OneByOne deriving (Eq, Show)

type DebugM a = GenHaxl Debuggee String a

run :: Env Debuggee String -> DebugM a -> IO a
run = runHaxl

runTrace :: Env Debuggee String -> DebugM a -> IO a
runTrace e act = do
  (r, ws) <- runHaxlWithWrites e act
  mapM putStrLn ws
  return r

traceWrite :: Show a => a -> GenHaxl u String ()
traceWrite = tellWrite . show

traceMsg :: String -> GenHaxl u String ()
traceMsg = tellWrite


-- | Add the request to the request count for debugging
logRequest :: Debuggee -> Request a -> IO ()
logRequest d r = do
  let c = requestCommandId r
  atomicModifyIORef' (debuggeeRequestCount d) ((,()) . HM.alter (Just . maybe 1 (+1)) c)


debuggeeProcess :: FilePath -> FilePath -> IO CreateProcess
debuggeeProcess exe sockName = do
  e <- getEnvironment
  return $
    (proc exe []) { env = Just (("GHC_DEBUG_SOCKET", sockName) : e) }

-- | Open a debuggee
withDebuggee :: FilePath  -- ^ path to executable
             -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
             -> (Env Debuggee String -> IO a)
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
                   -> (Env Debuggee String -> IO a)
                   -> IO a
withDebuggeeSocket exeName sockName action = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix sockName)
    hdl <- socketToHandle s ReadWriteMode
    requestMap <- newIORef HM.empty
    bc <- newIORef emptyBlockCache
    let ss = stateSet (BCRequestState bc hdl) (stateSet (RequestState hdl) stateEmpty)
    new_env <- initEnv ss (Debuggee exeName requestMap Batch)
    -- Turn on data fetch stats with report = 3
    let new_flags = defaultFlags { report = 0 }
    action (new_env { Haxl.Core.flags = new_flags })

-- | Send a request to a 'Debuggee' paused with 'pauseDebuggee'.
request :: (Show resp, Typeable resp) => Request resp -> DebugM resp
request = dataFetch


instance StateKey Request where
  data State Request = RequestState Handle

instance DataSourceName Request where
  dataSourceName Proxy = "ghc-debug"

instance ShowP Request where
  showp = show


-- | Group together RequestClosures and RequestInfoTables to avoid
-- some context switching.
groupFetches :: Handle -> [([ClosurePtr], ResultVar [RawClosure])] -> [([InfoTablePtr], ResultVar [RawInfoTable])] -> [BlockedFetch Request] -> [BlockedFetch Request] -> IO ()
groupFetches h cs is todo [] = dispatch h cs is (reverse todo)
groupFetches h cs is todo (b@(BlockedFetch r resp) : bs) =
  case r of
    RequestInfoTables is' -> groupFetches h cs ((is', resp):is) todo bs
    RequestClosures cs' -> groupFetches h ((cs', resp):cs) is todo bs
    _ -> groupFetches h cs is (b:todo) bs

dispatch :: Handle
         -> [([ClosurePtr], ResultVar [RawClosure])]
         -> [([InfoTablePtr], ResultVar [RawInfoTable])]
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
recordResults res ((length -> n, var):xs) =
  putSuccess var here >> recordResults later xs
  where
    (here, later) = splitAt n res


_singleFetches :: Handle -> [BlockedFetch Request] -> IO ()
_singleFetches h bs = mapM_ do_one bs
      where
        do_one (BlockedFetch req resp) = do
          res <- doRequest h req
          putSuccess resp res

instance DataSource Debuggee Request where
  fetch (RequestState h) fs u =
    -- Grouping together fetches only shaves off about 0.01s on the simple
    -- benchmark
    SyncFetch $
      case debuggeeBatchMode u of
        Batch -> groupFetches h [] [] []
        OneByOne -> _singleFetches h

data BlockCacheRequest a where
  LookupClosure :: ClosurePtr -> BlockCacheRequest RawClosure


deriving instance Show (BlockCacheRequest a)
deriving instance Eq (BlockCacheRequest a)

instance Hashable (BlockCacheRequest a) where
  hashWithSalt s (LookupClosure cpt) = s `hashWithSalt` (1 :: Int) `hashWithSalt` cpt

instance StateKey BlockCacheRequest where
  data State BlockCacheRequest = BCRequestState (IORef BlockCache) Handle

instance DataSourceName BlockCacheRequest where
  dataSourceName Proxy = "block-cache"

instance ShowP BlockCacheRequest where
  showp = show


instance DataSource u BlockCacheRequest where
  fetch (BCRequestState ref h) fs u =
    SyncFetch (mapM_ do_one)
    where
      do_one :: BlockedFetch BlockCacheRequest -> IO ()
      do_one (BlockedFetch (LookupClosure cp) resp) = do
        bc <- readIORef ref
        let mrb = lookupClosure cp bc
        rb <- case mrb of
                Nothing -> do
                  rb@(RawBlock p _) <- doRequest h (RequestBlock cp)
                  atomicModifyIORef' ref (\bc' -> (addBlock rb bc', ()))
                  return rb
                Just rb -> do
                  return rb
        putSuccess resp (extractFromBlock cp rb)

