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
-- parellel.
--
-- The tracing functions create a thread for each MBlock which we
-- traverse, closures are then sent to the relevant threads to be
-- dereferenced and thread-local storage is accumulated.
module GHC.Debug.ParTrace ( traceParFromM, tracePar, TraceFunctionsIO(..), ClosurePtrWithInfo(..), parCensus ) where

import           GHC.Debug.Types
import           GHC.Debug.Client.Query
import           GHC.Debug.Profile

import qualified Data.IntMap as IM
import Data.Array.BitArray.IO hiding (map)
import Control.Monad.Reader
import Data.Word
import GHC.Debug.Client.Monad.Simple
import Control.Concurrent.Chan.Unagi
import Control.Concurrent.Async
import Data.List
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Map as Map
import Data.Text (Text)
import Data.IORef
import Control.Exception.Base

unsafeLiftIO :: IO a -> DebugM a
unsafeLiftIO = DebugM . liftIO
-- | State local to a thread
data ThreadState s = ThreadState (IM.IntMap (IOBitArray Word16)) (IORef s)

data ThreadInfo a = ThreadInfo !(InChan (ClosurePtrWithInfo a))

data ClosurePtrWithInfo a = ClosurePtrWithInfo !a !ClosurePtr


-- Map from MBlockPtr -> Information about the thread for that pointer
type ThreadMap a = IM.IntMap (ThreadInfo a)

data TraceState a = TraceState { visited :: !(ThreadMap a) }


getKeyPair :: ClosurePtr -> (Int, Word16)
getKeyPair cp =
  let BlockPtr raw_bk = applyBlockMask cp
      bk = fromIntegral raw_bk `div` 8
      offset = (getBlockOffset cp) `div` 8
  in (bk, fromIntegral offset)

getMBlockKey :: ClosurePtr -> Int
getMBlockKey cp =
  let BlockPtr raw_bk = applyMBlockMask cp
  in fromIntegral raw_bk

sendToChan :: ThreadInfo a -> TraceState a -> ClosurePtrWithInfo a -> DebugM ()
sendToChan (ThreadInfo main_ic) ts cpi@(ClosurePtrWithInfo _ cp) = DebugM $ ask >>= \_ -> liftIO $ do
  let st = visited ts
      mkey = getMBlockKey cp
  case IM.lookup mkey st of
    Nothing -> writeChan main_ic cpi
    Just (ThreadInfo ic) -> writeChan ic cpi

initThread :: Monoid s => TraceFunctionsIO a s -> DebugM (ThreadInfo a, (ClosurePtrWithInfo a -> DebugM ()) -> DebugM (Async s))
initThread k = DebugM $ do
  e <- ask
  (ic, oc) <- liftIO $ newChan
  ref <- liftIO $ newIORef mempty
  let start go = unsafeLiftIO $ async $ runSimple e $ workerThread k ref go oc
  return (ThreadInfo ic,  start)

workerThread :: forall s a . Monoid s => TraceFunctionsIO a s -> IORef s -> (ClosurePtrWithInfo a -> DebugM ()) -> OutChan (ClosurePtrWithInfo a) -> DebugM s
workerThread k ref go oc = DebugM $ do
  d <- ask
  liftIO $ runSimple d (loop (ThreadState IM.empty ref))
  where
    loop !m = do
      mcp <- unsafeLiftIO $ try $ readChan oc
      case mcp of
        -- The thread gets blocked on readChan when the work is finished so
        -- when this happens, catch the exception and return the accumulated
        -- state for the thread. Each thread has a reference to all over
        -- threads, so the exception is only raised when ALL threads are
        -- waiting for work.
        Left BlockedIndefinitelyOnMVar -> unsafeLiftIO $ readIORef ref
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


    -- Just do the other dereferencing in the same thread
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


checkVisit :: ClosurePtr -> ThreadState s -> IO (ThreadState s, Bool)
checkVisit cp st = do
  let (bk, offset) = getKeyPair cp
      ThreadState v ref = st
  case IM.lookup bk v of
    Nothing -> do
      na <- newArray (0, fromIntegral (blockMask `div` 8)) False
      writeArray na offset True
      return (ThreadState (IM.insert bk na v) ref, False)
    Just bm -> do
      res <- readArray bm offset
      unless res (writeArray bm offset True)
      return (st, res)


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
traceParFromM :: Monoid s => [RawBlock] -> TraceFunctionsIO a s -> [ClosurePtrWithInfo a] -> DebugM s
traceParFromM bs k cps = do
  let bs' = nub $ (map (blockMBlock . rawBlockAddr) bs)
  (init_mblocks, start)  <- unzip <$> mapM (\b -> do
                                    (ti, start) <- initThread k
                                    return ((fromIntegral b, ti), start)) bs'
  (other_ti, start_other) <- initThread k
  let ts_map = IM.fromList init_mblocks
      go  = sendToChan other_ti (TraceState ts_map)
  as <- sequence (start_other go : map ($ go) start )
  mapM go cps
  unsafeLiftIO $ mconcat <$> mapM wait as

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

-- | Parallel heap census
parCensus :: [RawBlock] -> [ClosurePtr] -> DebugM (Map.Map Text CensusStats)
parCensus bs cs = DebugM $ do
  d <- ask
  MMap.getMonoidalMap <$> (liftIO $ runSimple d $ traceParFromM bs funcs (map (ClosurePtrWithInfo ()) cs))

  where
    nop = const (return ())
    funcs = TraceFunctionsIO nop nop clos  (const (const (return mempty))) nop

    clos :: ClosurePtr -> SizedClosure -> ()
              -> DebugM ((), MMap.MonoidalMap Text CensusStats, DebugM () -> DebugM ())
    clos _cp sc () = do
      d <- quadtraverse pure dereferenceConDesc pure pure sc
      let s :: Size
          s = dcSize sc
          v =  mkCS s
      return $ ((), MMap.singleton (closureToKey (noSize d)) v, id)

