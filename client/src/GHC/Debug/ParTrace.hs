{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Functions to support the constant space traversal of a heap.
-- This module is like the Trace module but performs the tracing in
-- parellel. The speed-up is quite modest but hopefully can be improved in
-- future.
--
-- The tracing functions create a thread for each MBlock which we
-- traverse, closures are then sent to the relevant threads to be
-- dereferenced and thread-local storage is accumulated.
module GHC.Debug.ParTrace ( traceParFromM, tracePar, TraceFunctionsIO(..), ClosurePtrWithInfo(..) ) where

import           GHC.Debug.Types
import           GHC.Debug.Client.Query

import qualified Data.IntMap as IM
import Data.Array.BitArray.IO hiding (map)
import Control.Monad.Reader
import Data.Word
import GHC.Debug.Client.Monad.Simple
import Control.Concurrent.Async
import Data.IORef
import Control.Exception.Base
import Debug.Trace
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

import Data.Bits
import GHC.Int
import GHC.IO (IO(..))
import GHC.Prim

threads :: Int
threads = 64

type InChan = TChan
type OutChan = TChan

unsafeLiftIO :: IO a -> DebugM a
unsafeLiftIO = DebugM . liftIO

-- | State local to a thread, there are $threads spawned, each which deals
-- with (address `div` 8) % threads. Each thread therefore:
--
-- * Outer map, segmented by MBlock
--  * Inner map, blocks for that MBlock
--    * Inner IOBitArray, visited information for that block
data ThreadState s = ThreadState (IM.IntMap (IM.IntMap (IOBitArray Word16))) (IORef s)

newtype ThreadInfo a = ThreadInfo (InChan (ClosurePtrWithInfo a))

-- | A 'ClosurePtr' with some additional information which needs to be
-- communicated across to another thread.
data ClosurePtrWithInfo a = ClosurePtrWithInfo !a !ClosurePtr


-- | Map from Thread -> Information about the thread
type ThreadMap a = IM.IntMap (ThreadInfo a)

newtype TraceState a = TraceState { visited :: (ThreadMap a) }


getKeyTriple :: ClosurePtr -> (Int, Int, Word16)
getKeyTriple cp =
  let BlockPtr raw_bk = applyBlockMask cp
      bk = fromIntegral raw_bk `div` 8
      offset = getBlockOffset cp `div` 8
      BlockPtr raw_mbk = applyMBlockMask cp
      mbk = fromIntegral raw_mbk `div` 8
  in (mbk, bk, fromIntegral offset)

getMBlockKey :: ClosurePtr -> Int
getMBlockKey cp =
  let BlockPtr raw_bk = applyMBlockMask cp
  -- Not sure why I had to divide this by 4, but I did.
  in (fromIntegral raw_bk `div` fromIntegral mblockMask `div` 4) `mod` threads

sendToChan :: TraceState a -> ClosurePtrWithInfo a -> DebugM ()
sendToChan  ts cpi@(ClosurePtrWithInfo _ cp) = DebugM $ liftIO $ do
  let st = visited ts
      mkey = getMBlockKey cp
  case IM.lookup mkey st of
    Nothing -> error $ "Not enough chans:" ++ show mkey ++ show threads
    Just (ThreadInfo ic) -> atomically $ writeTChan ic cpi

initThread :: Monoid s =>
              Int
           -> TraceFunctionsIO a s
           -> DebugM (ThreadInfo a, STM Bool, (ClosurePtrWithInfo a -> DebugM ()) -> DebugM (Async s))
initThread n k = DebugM $ do
  e <- ask
  ic <- liftIO $ newTChanIO
  let oc = ic
  ref <- liftIO $ newIORef mempty
  worker_active <- liftIO $ newTVarIO True
  let start go = unsafeLiftIO $ async $ runSimple e $ workerThread n k worker_active ref go oc
      finished = do
        active <- not <$> readTVar worker_active
        empty  <- isEmptyTChan ic
        return (active && empty)

  return (ThreadInfo ic, finished, start)

workerThread :: forall s a . Monoid s => Int -> TraceFunctionsIO a s -> TVar Bool -> IORef s -> (ClosurePtrWithInfo a -> DebugM ()) -> OutChan (ClosurePtrWithInfo a) -> DebugM s
workerThread n k worker_active ref go oc = DebugM $ do
  d <- ask
  r <- liftIO $ newIORef (ThreadState IM.empty ref)
  liftIO $ runSimple d (loop r)
  where
    loop r = do
      mcp <- unsafeLiftIO $ try $ do
              atomically $ writeTVar worker_active False
              atomically $ do
                v <- readTChan oc
                writeTVar worker_active True
                return v
      case mcp of
        -- The thread gets blocked on readChan when the work is finished so
        -- when this happens, catch the exception and return the accumulated
        -- state for the thread. Each thread has a reference to all over
        -- threads, so the exception is only raised when ALL threads are
        -- waiting for work.
        Left AsyncCancelled -> do
          unsafeLiftIO $ readIORef ref
        Right cpi -> deref r cpi >> loop r

    deref r (ClosurePtrWithInfo a cp) = do
        m <- unsafeLiftIO $ readIORef r
        do
          (m', b) <- unsafeLiftIO $ checkVisit cp m
          unsafeLiftIO $ writeIORef r m'
          if b
            then do
              s <- visitedVal k cp a
              unsafeLiftIO $ modifyIORef' ref (s <>)
            else do
              sc <- dereferenceClosure cp
              (a', s, cont) <- closTrace k cp sc a
              unsafeLiftIO $ modifyIORef' ref (s <>)
              cont (() <$ quadtraverse (gop r a') gocd (gos r a') (goc r . ClosurePtrWithInfo a') sc)

    goc r c@(ClosurePtrWithInfo _i cp) =
      let mkey = getMBlockKey cp
      in if (mkey == n)
          then deref r c
          else go c

    -- Just do the other dereferencing in the same thread for other closure
    -- types as they are not as common.
    gos r a st = do
      st' <- dereferenceStack st
      stackTrace k st'
      () <$ traverse (goc r . ClosurePtrWithInfo a) st'

    gocd d = do
      cd <- dereferenceConDesc d
      conDescTrace k cd

    gop r a p = do
      p' <- dereferencePapPayload p
      papTrace k p'
      () <$ traverse (goc r . ClosurePtrWithInfo a) p'


handleBlockLevel :: IM.Key
                    -> Word16
                    -> IM.IntMap (IOBitArray Word16)
                    -> IO (IM.IntMap (IOBitArray Word16), Bool)

handleBlockLevel bk offset m = do
  case IM.lookup bk m of
    Nothing -> do
      na <- newArray (0, fromIntegral (blockMask `div` 8)) False
      writeArray na offset True
      return (IM.insert bk na m, False)
    Just bm -> do
      res <- readArray bm offset
      unless res (writeArray bm offset True)
      return (m, res)

checkVisit :: ClosurePtr -> ThreadState s -> IO (ThreadState s, Bool)
checkVisit cp st = do
  let (mbk, bk, offset) = getKeyTriple cp
      ThreadState v ref = st
  case IM.lookup mbk v of
    Nothing -> do
      (st', res) <- handleBlockLevel bk offset IM.empty
      return (ThreadState (IM.insert mbk st' v) ref, res)
    Just bm -> do
      (st', res) <- handleBlockLevel bk offset bm
      return (ThreadState (IM.insert mbk st' v) ref, res)





data TraceFunctionsIO a s =
      TraceFunctionsIO { papTrace :: !(GenPapPayload ClosurePtr -> DebugM ())
      , stackTrace :: !(GenStackFrames ClosurePtr -> DebugM ())
      , closTrace :: !(ClosurePtr -> SizedClosure -> a -> DebugM (a, s, DebugM () -> DebugM ()))
      , visitedVal :: !(ClosurePtr -> a -> DebugM s)
      , conDescTrace :: !(ConstrDesc -> DebugM ())
      }


-- | A generic heap traversal function which will use a small amount of
-- memory linear in the heap size. Using this function with appropiate
-- accumulation functions you should be able to traverse quite big heaps in
-- not a huge amount of memory.
--
-- The performance of this parralel version depends on how much contention
-- the functions given in 'TraceFunctionsIO' content for the handle
-- connecting for the debuggee (which is protected by an 'MVar'). With no
-- contention, and precached blocks, the workload can be very evenly
-- distributed leading to high core utilisation.
--
-- As performance depends highly on contention, snapshot mode is much more
-- amenable to parrelisation where the time taken for requests is much
-- lower.
traceParFromM :: Monoid s => TraceFunctionsIO a s -> [ClosurePtrWithInfo a] -> DebugM s
traceParFromM k cps = do
  traceM ("SPAWNING: " ++ show threads)
  (init_mblocks, work_actives, start)  <- unzip3 <$> mapM (\b -> do
                                    (ti, working, start) <- initThread b k
                                    return ((fromIntegral b, ti), working, start)) [0 .. threads - 1]
  let ts_map = IM.fromList init_mblocks
      go  = sendToChan (TraceState ts_map)
  as <- sequence (map ($ go) start )
  mapM_ go cps
  unsafeLiftIO $ waitFinish work_actives
  unsafeLiftIO $ mapM_ cancel as
  unsafeLiftIO $ mconcat <$> mapM wait as

waitFinish :: [STM Bool] -> IO ()
waitFinish working = atomically (checkDone working)
  where
    checkDone [] = return ()
    checkDone (x:xs) = do
      b <- x
      -- The variable tracks whether the thread thinks it's finished (no
      -- active work and empty chan)
      if b then checkDone xs else retry

-- | A parellel tracing function.
tracePar :: [ClosurePtr] -> DebugM ()
tracePar = traceParFromM funcs . map (ClosurePtrWithInfo ())
  where
    nop = const (return ())
    funcs = TraceFunctionsIO nop nop clos (const (const (return ()))) nop

    clos :: ClosurePtr -> SizedClosure -> ()
              -> DebugM ((), (), DebugM () -> DebugM ())
    clos _cp sc _ = do
      let itb = info (noSize sc)
      _traced <- getSourceInfo (tableId itb)
      return ((), (), id)


