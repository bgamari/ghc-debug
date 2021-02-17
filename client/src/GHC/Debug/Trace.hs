{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
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
import System.IO

newtype VisitedSet = VisitedSet (IM.IntMap (IOBitArray Word16))

data TraceState = TraceState { visited :: !VisitedSet, n :: !Int }


getKeyPair :: ClosurePtr -> (Int, Word16)
getKeyPair cp =
  let BlockPtr raw_bk = applyBlockMask cp
      bk = fromIntegral raw_bk `div` 8
      offset = getBlockOffset cp `div` 8
  in (bk, fromIntegral offset)

checkVisit :: ClosurePtr -> IORef TraceState -> IO Bool
checkVisit cp mref = do
  st <- readIORef mref
  let VisitedSet v = visited st
      num_visited = n st
      (bk, offset) = getKeyPair cp
  case IM.lookup bk v of
    Nothing -> do
      na <- newArray (0, fromIntegral (blockMask `div` 8)) False
      writeArray na offset True
      writeIORef mref (TraceState (VisitedSet (IM.insert bk na v)) (num_visited + 1))
      when (num_visited `mod` 10_000 == 0) $ hPutStrLn stderr ("Traced: " ++ show num_visited)
      return False
    Just bm -> do
      res <- readArray bm offset
      unless res (writeArray bm offset True)
      return res



data TraceFunctions m =
      TraceFunctions { papTrace :: !(GenPapPayload ClosurePtr -> m DebugM ())
      , stackTrace :: !(GenStackFrames ClosurePtr -> m DebugM ())
      , closTrace :: !(ClosurePtr -> SizedClosure -> m DebugM () -> m DebugM ())
      , visitedVal :: !(ClosurePtr -> (m DebugM) ())
      , conDescTrace :: !(ConstrDesc -> m DebugM ())
      }




type C m = (MonadTrans m, Monad (m DebugM))

-- | A generic heap traversal function which will use a small amount of
-- memory linear in the heap size. Using this function with appropiate
-- accumulation functions you should be able to traverse quite big heaps in
-- not a huge amount of memory.
traceFromM :: C m => TraceFunctions m-> [ClosurePtr] -> m DebugM ()
traceFromM k cps = do
  st <- lift (unsafeLiftIO (newIORef (TraceState (VisitedSet IM.empty) 1)))
  runReaderT (mapM_ (traceClosureFromM k) cps) st
{-# INLINE traceFromM #-}
{-# INLINE traceClosureFromM #-}

traceClosureFromM :: C m
                  => TraceFunctions m
                  -> ClosurePtr
                  -> ReaderT (IORef TraceState) (m DebugM) ()
traceClosureFromM !k = go
  where
    go cp = do
      mref <- ask
      b <- lift $ lift $ unsafeLiftIO (checkVisit cp mref)
      if b
        then lift $ visitedVal k cp
        else do
        sc <- lift $ lift $ dereferenceClosure cp
        ReaderT $ \st -> closTrace k cp sc
         (runReaderT (() <$ quadtraverse gop gocd gos go sc) st)


    gos st = do
      st' <- lift $ lift $ dereferenceStack st
      lift $ stackTrace k st'
      () <$ traverse go st'

    gocd d = do
      cd <- lift $ lift $ dereferenceConDesc d
      lift $ conDescTrace k cd

    gop p = do
      p' <- lift $ lift $ dereferencePapPayload p
      lift $ papTrace k p'
      () <$ traverse go p'
