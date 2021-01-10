{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
-- | Functions to support the constant space traversal of a heap.
module GHC.Debug.Client.Trace where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Client

--import qualified Data.IntSet as IS
import qualified GHC.Debug.Client.IntSet as IS
import qualified Data.IntMap as IM
import Data.Array.BitArray.IO
import Control.Monad.Reader
import Data.IORef
import Debug.Trace
import Control.Monad.Identity
import Data.Word

newtype VisitedSet = VisitedSet (IM.IntMap (IOBitArray Word64))

newtype TraceState = TraceState { visited :: VisitedSet }

data TraceFunctions m =
      TraceFunctions { papTrace :: GenPapPayload ClosurePtr -> m DebugM ()
      , stackTrace :: GenStackFrames ClosurePtr -> m DebugM ()
      , closTrace :: ClosurePtr -> SizedClosure -> m DebugM () -> m DebugM ()
      , visitedVal :: ClosurePtr -> (m DebugM) ()
      , conDescTrace :: ConstrDesc -> m DebugM ()
      }


type C m = (MonadTrans m, Monad (m DebugM))

getKeyPair :: ClosurePtr -> (Int, Word64)
getKeyPair cp =
  let BlockPtr raw_bk = applyBlockMask cp
      bk = fromIntegral raw_bk `div` 8
      offset = (getBlockOffset cp) `div` 8
  in (bk, offset)

checkVisit :: ClosurePtr -> IORef TraceState -> IO Bool
checkVisit cp mref = do
  st <- readIORef mref
  let VisitedSet v = visited st
      (bk, offset) = getKeyPair cp
  case IM.lookup bk v of
    Nothing -> do
      na <- newArray (0, blockMask `div` 8) False
      writeArray na offset True
      writeIORef mref (TraceState (VisitedSet (IM.insert bk na v)))
      return False
    Just bm -> do
      res <- readArray bm offset
      unless res (writeArray bm offset True)
      return res


type SizedClosureC = DebugClosureWithSize PayloadCont ConstrDesc StackCont ClosurePtr

-- Traverse the tree from GC roots, to populate the caches
-- with everything necessary.
traceFrom :: [ClosurePtr] -> DebugM ()
traceFrom cps = runIdentityT (traceFromM funcs cps)
  where
    nop = const (return ())
    funcs = TraceFunctions nop nop clos (const (return ())) nop

    clos :: ClosurePtr -> SizedClosure -> (IdentityT DebugM) ()
              ->  (IdentityT DebugM) ()
    clos cp sc k = do
      let itb = info (noSize sc)
      lift $ request (RequestSourceInfo (tableId itb))
      k

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
    go (untagClosurePtr -> cp) = do
      mref <- ask
      b <- lift $ lift $ unsafeLiftIO (checkVisit cp mref)
      if b
        then lift $ visitedVal k cp
        else do
        sc <- lift $ lift $ dereferenceClosureFromBlock cp
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
