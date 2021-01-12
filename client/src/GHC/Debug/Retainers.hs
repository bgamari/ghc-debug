-- | Functions for computing retainers
module GHC.Debug.Retainers where

import GHC.Debug.Client
import Control.Monad.State
import GHC.Debug.Trace

import qualified Data.Set as Set
import Control.Monad.RWS

addOne :: [ClosurePtr] -> (Maybe Int, [[ClosurePtr]]) -> (Maybe Int, [[ClosurePtr]])
addOne _ (Just 0, cp) = (Just 0, cp)
addOne cp (n, cps)    = (subtract 1 <$> n, cp:cps)

-- | From the given roots, find any path to one of the given pointers.
-- Note: This function can be quite slow!
findRetainers :: Maybe Int -> [ClosurePtr] -> [ClosurePtr] -> DebugM [[ClosurePtr]]
findRetainers limit rroots bads = (\(_, r, _) -> snd r) <$> runRWST (traceFromM funcs rroots) [] (limit, [])
  where
    bads_set = Set.fromList bads
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }
    -- Add clos
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> RWST [ClosurePtr] () (Maybe Int, [[ClosurePtr]]) DebugM ()
               -> RWST [ClosurePtr] () (Maybe Int, [[ClosurePtr]]) DebugM ()
    closAccum cp _ k
      | cp `Set.member` bads_set = do
          ctx <- ask
          modify' (addOne (cp: ctx))
          -- Don't call k, there might be more paths to the pointer but we
          -- probably just care about this first one.
      | otherwise = do
          (lim, _) <- get
          case lim of
            Just 0 -> return ()
            _ -> local (cp:) k

