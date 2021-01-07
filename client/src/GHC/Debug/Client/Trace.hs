{-# LANGUAGE ConstraintKinds #-}
-- | Functions to support the constant space traversal of a heap.
module GHC.Debug.Client.Trace where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Client

import qualified Data.IntSet as IS
import Control.Monad.State
import Debug.Trace

data TraceState = TraceState { visited :: !(IS.IntSet) }

data TraceFunctions m =
      TraceFunctions { papTrace :: GenPapPayload ClosurePtr -> m DebugM ()
      , stackTrace :: GenStackFrames ClosurePtr -> m DebugM ()
      -- TODO: This interface is not very nice, it is something a bit like
      -- UnliftIO? The idea is that the user provided function might want
      -- to modify the context the continuation is called in.
      , closTrace :: ClosurePtr -> SizedClosure -> StateT TraceState (m DebugM) () -> StateT TraceState (m DebugM) ()
      , visitedVal :: ()
      , conDescTrace :: ConstrDesc -> m DebugM ()
      }


type C m = (MonadTrans m, Monad (m DebugM))

addVisit :: ClosurePtr -> TraceState -> TraceState
addVisit (ClosurePtr c) st = st { visited = IS.insert (fromIntegral c) (visited st) }

checkVisit :: ClosurePtr -> TraceState -> Bool
checkVisit (ClosurePtr c) st = IS.member (fromIntegral c) (visited st)

type SizedClosureC = DebugClosureWithSize PayloadCont ConstrDesc StackCont ClosurePtr

traceFromM :: C m => TraceFunctions m -> [ClosurePtr] -> m DebugM ()
traceFromM k cps = evalStateT (mapM_ (traceClosureFromM k) cps) (TraceState IS.empty)

traceClosureFromM :: C m
                  => TraceFunctions m
                  -> ClosurePtr
                  -> StateT TraceState (m DebugM) ()
traceClosureFromM k cp = do
    m <- get
    if (checkVisit cp m)
      then return (visitedVal k)
      else do
      modify (addVisit cp)
      sc <- lift $ lift $ dereferenceClosureFromBlock cp
      closTrace k cp sc
        (() <$ quadtraverse (tracePapPayloadM k) (traceConstrDescM k) (traceStackFromM k) (traceClosureFromM k) sc)

traceStackFromM :: C m
                => TraceFunctions m
                -> StackCont -> StateT TraceState (m DebugM) ()
traceStackFromM f st = do
  st' <- lift $ lift $ dereferenceStack st
  lift $ stackTrace f st'
  () <$ traverse (traceClosureFromM f) st'

traceConstrDescM :: (C m)
                 => TraceFunctions m -> ConstrDescCont -> StateT s (m DebugM) ()
traceConstrDescM f d = do
  cd <- lift $ lift $ dereferenceConDesc d
  lift $ conDescTrace f cd

tracePapPayloadM :: C m
                 => TraceFunctions m
                 -> PayloadCont
                 -> StateT TraceState (m DebugM) ()
tracePapPayloadM f p = do
  p' <- lift $ lift $ dereferencePapPayload p
  lift $ papTrace f p'
  () <$ traverse (traceClosureFromM f) p'
