{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
-- | Functions to support the constant space traversal of a heap.
module GHC.Debug.Trace ( traceFromM, TraceFunctions(..) ) where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Client.Query

import qualified Data.IntMap as IM
import Data.Array.BitArray.IO
import Control.Monad.Reader
import Data.IORef
import Data.Word

newtype VisitedSet = VisitedSet (IM.IntMap (IOBitArray Word16))

newtype TraceState = TraceState { visited :: VisitedSet }


getKeyPair :: ClosurePtr -> (Int, Word16)
getKeyPair cp =
  let BlockPtr raw_bk = applyBlockMask cp
      bk = fromIntegral raw_bk `div` 8
      offset = (getBlockOffset cp) `div` 8
  in (bk, fromIntegral offset)

checkVisit :: ClosurePtr -> IORef TraceState -> IO Bool
checkVisit cp mref = do
  st <- readIORef mref
  let VisitedSet v = visited st
      (bk, offset) = getKeyPair cp
  case IM.lookup bk v of
    Nothing -> do
      na <- newArray (0, fromIntegral (blockMask `div` 8)) False
      writeArray na offset True
      writeIORef mref (TraceState (VisitedSet (IM.insert bk na v)))
      return False
    Just bm -> do
      res <- readArray bm offset
      unless res (writeArray bm offset True)
      return res



data TraceFunctions m =
      TraceFunctions { papTrace :: GenPapPayload ClosurePtr -> m DebugM ()
      , stackTrace :: GenStackFrames ClosurePtr -> m DebugM ()
      , closTrace :: ClosurePtr -> SizedClosure -> m DebugM () -> m DebugM ()
      , visitedVal :: ClosurePtr -> (m DebugM) ()
      , conDescTrace :: ConstrDesc -> m DebugM ()
      }


type C m = (MonadTrans m, Monad (m DebugM))

-- | A generic heap traversal function which will use a small amount of
-- memory linear in the heap size. Using this function with appropiate
-- accumulation functions you should be able to traverse quite big heaps in
-- not a huge amount of memory.
traceFromM :: C m => TraceFunctions m -> [ClosurePtr] -> m DebugM ()
traceFromM k cps = do
  st <- lift (unsafeLiftIO (newIORef (TraceState (VisitedSet IM.empty))))
  runReaderT (mapM_ (traceClosureFromM k) cps) st
{-# INLINE traceFromM #-}
{-# INLINE traceClosureFromM #-}
{-# INLINE traceStackFromM #-}
{-# INLINE traceConstrDescM #-}
{-# INLINE tracePapPayloadM #-}

traceClosureFromM :: C m
                  => TraceFunctions m
                  -> ClosurePtr
                  -> ReaderT (IORef TraceState) (m DebugM) ()
traceClosureFromM k = go
  where
    go cp = do
      mref <- ask
      b <- lift $ lift $ unsafeLiftIO (checkVisit cp mref)
      if b
        then lift $ visitedVal k cp
        else do
        sc <- lift $ lift $ dereferenceClosure cp
        ReaderT $ \st -> closTrace k cp sc
         (runReaderT (() <$ quadtraverse (tracePapPayloadM k) (traceConstrDescM k) (traceStackFromM k) (traceClosureFromM k) sc) st)

traceStackFromM :: C m
                => TraceFunctions m
                -> StackCont -> ReaderT (IORef TraceState) (m DebugM) ()
traceStackFromM f = go
  where
    go st = do
      st' <- lift $ lift $ dereferenceStack st
      lift $ stackTrace f st'
      () <$ traverse (traceClosureFromM f) st'

traceConstrDescM :: (C m)
                 => TraceFunctions m -> ConstrDescCont -> ReaderT s (m DebugM) ()
traceConstrDescM f = go
  where
    go d = do
      cd <- lift $ lift $ dereferenceConDesc d
      lift $ conDescTrace f cd

tracePapPayloadM :: C m
                 => TraceFunctions m
                 -> PayloadCont
                 -> ReaderT (IORef TraceState) (m DebugM) ()
tracePapPayloadM f = go
  where
    go p = do
      p' <- lift $ lift $ dereferencePapPayload p
      lift $ papTrace f p'
      () <$ traverse (traceClosureFromM f) p'
