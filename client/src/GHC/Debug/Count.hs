module GHC.Debug.Count where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Profile
import           GHC.Debug.Trace
import           GHC.Debug.ParTrace hiding (TraceFunctionsIO(..))
import GHC.Debug.ParTrace (TraceFunctionsIO(TraceFunctionsIO))
import Control.Monad.State


parCount :: [ClosurePtr] -> DebugM CensusStats
parCount = traceParFromM funcs . map (ClosurePtrWithInfo ())
  where
    nop = const (return ())
    funcs = TraceFunctionsIO nop nop nop clos (const (const (return mempty))) nop

    clos :: ClosurePtr -> SizedClosure -> ()
              -> DebugM ((), CensusStats, DebugM () -> DebugM ())
    clos _cp sc _ = do
      return ((), mkCS (dcSize sc), id)

-- | Simple statistics about a heap, total objects, size and maximum object
-- size
count :: [ClosurePtr] -> DebugM CensusStats
count cps = snd <$> runStateT (traceFromM funcs cps) (CS 0 0 0)
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , srtTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }

    closAccum  :: ClosurePtr
               -> SizedClosure
               ->  (StateT CensusStats DebugM) ()
               ->  (StateT CensusStats DebugM) ()
    closAccum _cp s k = do
      modify' (go s)
      k

    go :: SizedClosure -> CensusStats -> CensusStats
    go sc cs = mkCS (dcSize sc) <> cs
