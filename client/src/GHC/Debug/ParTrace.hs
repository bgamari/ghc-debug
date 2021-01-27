{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
--import Control.Concurrent.Chan.Unagi
import Control.Concurrent.Async
import Data.List
import Data.IORef
import Control.Exception.Base
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import GHC.Conc
import Debug.Trace

type InChan = TChan
type OutChan = TChan

threads :: Int
threads = 16

unsafeLiftIO :: IO a -> DebugM a
unsafeLiftIO = DebugM . liftIO

-- | State local to a thread, there are $threads spawned, each which deals
-- with (address `div` 8) % threads. Each thread therefore:
--
-- * Outer map, segmented by MBlock
--  * Inner map, blocks for that MBlock
--    * Inner IOBitArray, visited information for that block
data ThreadState s = ThreadState (IM.IntMap (IM.IntMap (IOBitArray Word16))) (IORef s)

data ThreadInfo a = ThreadInfo !(InChan (ClosurePtrWithInfo a))

-- | A 'ClosurePtr' with some additional information which needs to be
-- communicated across to another thread.
data ClosurePtrWithInfo a = ClosurePtrWithInfo !a !ClosurePtr


-- | Map from Thread -> Information about the thread
type ThreadMap a = IM.IntMap (ThreadInfo a)

data TraceState a = TraceState { visited :: !(ThreadMap a) }


getKeyTriple :: ClosurePtr -> (Int, Int, Word16)
getKeyTriple cp =
  let BlockPtr raw_bk = applyBlockMask cp
      bk = fromIntegral raw_bk `div` 8
      offset = (getBlockOffset cp) `div` 8
      BlockPtr raw_mbk = applyMBlockMask cp
      mbk = fromIntegral raw_mbk `div` 8
  in (mbk, bk, fromIntegral offset)

getMBlockKey :: ClosurePtr -> Int
getMBlockKey cp =
  let BlockPtr raw_bk = applyMBlockMask cp
  in (fromIntegral raw_bk `div` fromIntegral mblockMask) `mod` threads

sendToChan :: ThreadInfo a -> TraceState a -> ClosurePtrWithInfo a -> DebugM ()
sendToChan (ThreadInfo main_ic) ts cpi@(ClosurePtrWithInfo _ cp) = DebugM $ ask >>= \_ -> liftIO $ do
  let st = visited ts
      mkey = getMBlockKey cp
  atomically $ case IM.lookup mkey st of
    Nothing -> writeTChan main_ic cpi
    Just (ThreadInfo ic) -> writeTChan ic cpi

initThread :: Monoid s => TraceFunctionsIO a s
           -> DebugM (ThreadInfo a, STM Bool, (ClosurePtrWithInfo a -> DebugM ()) -> DebugM (Async s))
initThread k = DebugM $ do
  e <- ask
  ic <- liftIO $ atomically $ newTChan
  let oc = ic
  ref <- liftIO $ newIORef mempty
  let start go = unsafeLiftIO $ async $ runSimple e $ workerThread k ref go oc
  return (ThreadInfo ic, isEmptyTChan ic ,  start)

workerThread :: forall s a . Monoid s => TraceFunctionsIO a s -> IORef s -> (ClosurePtrWithInfo a -> DebugM ()) -> OutChan (ClosurePtrWithInfo a) -> DebugM s
workerThread k ref go oc = DebugM $ do
  d <- ask
  liftIO $ runSimple d (loop (ThreadState IM.empty ref))
  where
    loop !m = do
      mcp <- unsafeLiftIO $ try $ atomically $ readTChan oc
      case mcp of
        -- The thread gets blocked on readChan when the work is finished so
        -- when this happens, catch the exception and return the accumulated
        -- state for the thread. Each thread has a reference to all over
        -- threads, so the exception is only raised when ALL threads are
        -- waiting for work.
        Left AsyncCancelled -> unsafeLiftIO $ readIORef ref
        Right (ClosurePtrWithInfo a cp) -> do
          (m', b) <- unsafeLiftIO $ checkVisit cp m
          if b
            then do
              s <- visitedVal k cp a
              unsafeLiftIO $ modifyIORef' ref (s <>)
            else do
              sc <- dereferenceClosure cp
              (a', s, cont) <- closTrace k cp sc a
              unsafeLiftIO $ modifyIORef' ref (s <>)
              cont (() <$ quadtraverse (gop a') gocd (gos a') (go . ClosurePtrWithInfo a') sc)
          loop m'


    -- Just do the other dereferencing in the same thread for other closure
    -- types as they are not as common.
    gos a st = do
      st' <- dereferenceStack st
      stackTrace k st'
      () <$ traverse (go . ClosurePtrWithInfo a) st'

    gocd d = do
      cd <- dereferenceConDesc d
      conDescTrace k cd

    gop a p = do
      p' <- dereferencePapPayload p
      papTrace k p'
      () <$ traverse (go . ClosurePtrWithInfo a) p'


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
traceParFromM :: Monoid s => [RawBlock] -> TraceFunctionsIO a s -> [ClosurePtrWithInfo a] -> DebugM s
traceParFromM bs k cps = do
  let bs' = nub $ (map (blockMBlock . rawBlockAddr) bs)
  unsafeLiftIO $ print (("SPAWNING: ", threads))
  (init_mblocks, dones, start)  <- unzip3 <$> mapM (\b -> do
                                    (ti, done, start) <- initThread k
                                    return ((fromIntegral b, ti), done, start)) [0 .. threads - 1]
  (other_ti, done_other, start_other) <- initThread k
  let ts_map = IM.fromList init_mblocks
      go  = sendToChan other_ti (TraceState ts_map)
  as <- sequence (start_other go : map ($ go) start )
  mapM go cps
  unsafeLiftIO $ atomically $ waitFinish (done_other : dones)
  unsafeLiftIO $ mapM_ cancel as
  unsafeLiftIO $ mconcat <$> mapM wait as

waitFinish :: [STM Bool] -> STM ()
waitFinish [] = return ()
waitFinish (t:ts) = do
   b <- t
   if b then waitFinish ts
        else retry

-- | A parellel tracing function, the first argument is the list of known
-- blocks, providing an accurate list here will greatly speed up the
-- traversal.
tracePar :: [RawBlock] -> [ClosurePtr] -> DebugM ()
tracePar bs = traceParFromM bs funcs . map (ClosurePtrWithInfo ())
  where
    nop = const (return ())
    funcs = TraceFunctionsIO nop nop clos (const (const (return ()))) nop

    clos :: ClosurePtr -> SizedClosure -> ()
              -> DebugM ((), (), DebugM () -> DebugM ())
    clos _cp sc _ = do
      let itb = info (noSize sc)
      _traced <- getSourceInfo (tableId itb)
      return $ ((), (), id)


