{-# LANGUAGE ConstraintKinds #-}
module GHC.Debug.Client.Trace where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Client

import qualified Data.IntSet as IS
import qualified Data.Map as Map
import Control.Monad.State

data TraceState = TraceState { visited :: !(IS.IntSet) }

data TraceFunctions m a b c d = TraceFunctions { papTrace :: GenPapPayload d -> m DebugM a
                                             , stackTrace :: GenStackFrames d -> m DebugM c
                                             , closTrace :: DebugClosureWithSize a b c d -> m DebugM d
                                             , visitedVal :: d
                                             , conDescTrace :: ConstrDesc -> m DebugM b
                                             }


type C m = (MonadTrans m, Monad (m DebugM))

addVisit :: ClosurePtr -> TraceState -> TraceState
addVisit (ClosurePtr c) st = st { visited = IS.insert (fromIntegral c) (visited st) }

checkVisit :: ClosurePtr -> TraceState -> Bool
checkVisit (ClosurePtr c) st = IS.member (fromIntegral c) (visited st)

type SizedClosureC = DebugClosureWithSize PayloadCont ConstrDesc StackCont ClosurePtr

traceFromM :: C m => TraceFunctions m a b c d -> [ClosurePtr] -> m DebugM [d]
traceFromM k cps = evalStateT (mapM (traceClosureFromM k) cps) (TraceState IS.empty)

traceClosureFromM :: C m
                  => TraceFunctions m a b c d
                  -> ClosurePtr
                  -> StateT TraceState (m DebugM) d
traceClosureFromM k cp = do
    m <- get
    if (checkVisit cp m)
      then return (visitedVal k)
      else do
      modify (addVisit cp)
      sc <- lift $ lift $ dereferenceClosureFromBlock cp
      --sc' <- lift $ lift $ quadtraverse pure dereferenceConDesc pure pure sc
      sc' <- quadtraverse (tracePapPayloadM k) (traceConstrDescM k) (traceStackFromM k) (traceClosureFromM k) sc
      res <- lift $ closTrace k sc'
      return res

traceStackFromM :: C m
                => TraceFunctions m a b c d
                -> StackCont -> StateT TraceState (m DebugM) c
traceStackFromM f st = do
  st' <- lift $ lift $ dereferenceStack st
  st'' <- traverse (traceClosureFromM f) st'
  lift $ stackTrace f st''



traceConstrDescM :: (C m)
                 => TraceFunctions m a b c d -> ConstrDescCont -> StateT s (m DebugM) b
traceConstrDescM f d = do
  cd <- lift $ lift $ dereferenceConDesc d
  lift $ conDescTrace f cd

tracePapPayloadM :: C m
                 => TraceFunctions m a b c d
                 -> PayloadCont
                 -> StateT TraceState (m DebugM) a
tracePapPayloadM f p = do
  p' <- lift $ lift $ dereferencePapPayload p
  p'' <- traverse (traceClosureFromM f) p'
  lift $ papTrace f p''
