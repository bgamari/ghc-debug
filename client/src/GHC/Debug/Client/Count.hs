module GHC.Debug.Client.Count where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Client.Profile
import           GHC.Debug.Client.Trace
import Control.Monad.State

-- | General function for performing a heap census in constant memory
count :: [ClosurePtr] -> DebugM CensusStats
count cps = snd <$> runStateT (traceFromM funcs cps) (CS 0 0 0)
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }
    -- Add cos
    closAccum  :: ClosurePtr
               -> SizedClosure
               ->  (StateT CensusStats DebugM) ()
               ->  (StateT CensusStats DebugM) ()
    closAccum cp s k = do
      modify' (go s)
      k

    go :: SizedClosure -> CensusStats -> CensusStats
    go sc cs = mkCS (dcSize sc) <> cs
