-- | Functions for computing retainers
module GHC.Debug.Client.Retainers where

import GHC.Debug.Client
import Control.Monad.State
import           GHC.Debug.Types
import           GHC.Debug.Client.Trace

import qualified Data.Set as Set
import Control.Monad.RWS

-- | From the given roots, find any path to one of the given pointers.
-- Note: This function can be quite slow!
findRetainers :: [ClosurePtr] -> [ClosurePtr] -> DebugM [[ClosurePtr]]
findRetainers rroots bads = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) [] []
  where
    bads_set = Set.fromList bads
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = ()
              , conDescTrace = const (return ())

            }
    -- Add clos
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> StateT TraceState (RWST [ClosurePtr] () [[ClosurePtr]] DebugM) ()
               -> StateT TraceState (RWST [ClosurePtr] () [[ClosurePtr]] DebugM) ()
    closAccum cp _ k
      | cp `Set.member` bads_set = do
          ctx <- ask
          lift $ modify ((cp: ctx) :)
          -- Don't call k, there might be more paths to the pointer but we
          -- probably just care about this first one.
      | otherwise = local (cp:) k

