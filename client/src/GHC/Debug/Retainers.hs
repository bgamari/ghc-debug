-- | Functions for computing retainers
module GHC.Debug.Retainers(findRetainersOf, findRetainers) where

import GHC.Debug.Client
import Control.Monad.State
import GHC.Debug.Trace

import qualified Data.Set as Set
import Control.Monad.RWS

addOne :: [ClosurePtr] -> (Maybe Int, [[ClosurePtr]]) -> (Maybe Int, [[ClosurePtr]])
addOne _ (Just 0, cp) = (Just 0, cp)
addOne cp (n, cps)    = (subtract 1 <$> n, cp:cps)

findRetainersOf :: Maybe Int
                -> [ClosurePtr]
                -> [ClosurePtr]
                -> DebugM [[ClosurePtr]]
findRetainersOf limit cps bads = findRetainers limit cps (\cp _ -> return (cp `Set.member` bad_set))
  where
    bad_set = Set.fromList bads

-- | From the given roots, find any path to one of the given pointers.
-- Note: This function can be quite slow! The first argument is a limit to
-- how many paths to find. You should normally set this to a small number
-- such as 10.
findRetainers :: Maybe Int -> [ClosurePtr] -> (ClosurePtr -> SizedClosure -> DebugM Bool) -> DebugM [[ClosurePtr]]
findRetainers limit rroots p = (\(_, r, _) -> snd r) <$> runRWST (traceFromM funcs rroots) [] (limit, [])
  where
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
    closAccum cp sc k = do
      b <- lift $ p cp sc
      if b
        then do
          ctx <- ask
          modify' (addOne (cp: ctx))
          -- Don't call k, there might be more paths to the pointer but we
          -- probably just care about this first one.
        else do
          (lim, _) <- get
          case lim of
            Just 0 -> return ()
            _ -> local (cp:) k

