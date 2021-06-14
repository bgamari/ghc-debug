{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
-- | Attempt to find duplicate objects on the heap. The analysis is not
-- exact but attempts to find closures which are identical, and could be
-- shared.
module GHC.Debug.ObjectEquiv(objectEquiv, objectEquivAnalysis, printObjectEquiv, EquivMap) where

import GHC.Debug.Client.Monad
import GHC.Debug.Client
import GHC.Debug.Trace
import GHC.Debug.Profile
import GHC.Debug.Types.Graph (ppClosure)
import GHC.Debug.Types(ClosurePtr(..))

import Control.Monad.State
import Data.List (sortBy)
import Data.Ord
import Debug.Trace
import qualified Data.OrdPSQ as PS
import qualified Data.IntMap.Strict as IM
import Data.List.NonEmpty(NonEmpty(..))

type CensusByObjectEquiv = IM.IntMap CensusStats

-- | How big to allow the priority queue to grow to
limit :: Int
limit = 100_000
-- | How many times an object must appear per 100 000 closures to be
-- "interesting" and kept for the future.
of_interest :: Int
of_interest = 1000

-- Pick a representative ClosurePtr for each object
type EquivMap = PS.OrdPSQ PtrClosure -- Object
                           Int
                           ClosurePtr -- Representative of equivalence class

type Equiv2Map = IM.IntMap -- Pointer
                  ClosurePtr -- Representative of equivalence class

data ObjectEquivState = ObjectEquivState  {
                            emap   :: !EquivMap
                          , emap2 :: !Equiv2Map
                          , _census :: !CensusByObjectEquiv
                          }
-- Don't need to add identity mapping in emap2 because lookup failure is
-- the identity anyway.
addEquiv :: ClosurePtr -> PtrClosure -> ObjectEquivState -> ObjectEquivState
addEquiv cp pc (trimMap -> o) =
                  let (res, new_m) = PS.alter g pc (emap o)
                      new_emap2 = case res of
                                     Left _ -> emap2 o
                                     -- Only remap objects which have a hit
                                     -- in the cache
                                     Right new_cp -> addNewMap cp new_cp (emap2 o)
                  in ( o { emap = new_m
                      , emap2 = new_emap2 })
  where
    g Nothing = (Left cp, Just (0, cp))
    g (Just (p, v)) = (Right v, Just (p + 1, v))

addNewMap :: ClosurePtr -> ClosurePtr -> Equiv2Map -> Equiv2Map
addNewMap (ClosurePtr cp) equiv_cp o = IM.insert (fromIntegral cp) equiv_cp o

-- Trim down map to keep objects which have only been seen 100 times or
-- more within the last 10 000 seen objects
trimMap :: ObjectEquivState -> ObjectEquivState
trimMap o = if checkSize o > limit
              then let new_o = o { emap = snd $ PS.atMostView of_interest (emap o) }
                   in traceShow (checkSize new_o) new_o
                   -- TODO: Here would be good to also keep everything
                   -- which is referenced by the kept closures, otherwise
                   -- you end up with duplicates in the map

              else o

-- | O(1) due to psqueues implementation
checkSize :: ObjectEquivState -> Int
checkSize (ObjectEquivState e1 _ _) = PS.size e1

type PtrClosure = DebugClosureWithSize PapPayload ConstrDesc StackFrames ClosurePtr

-- | General function for performing a heap census in constant memory
censusObjectEquiv :: [ClosurePtr] -> DebugM ObjectEquivState
censusObjectEquiv cps = snd <$> runStateT (traceFromM funcs cps) (ObjectEquivState PS.empty IM.empty IM.empty)
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
               -> (StateT ObjectEquivState DebugM) ()
               -> (StateT ObjectEquivState DebugM) ()
    closAccum cp s k = do
      -- Step 0: Check to see whether there is already an equivalence class
      -- for this cp
      -- Step 1: Decode a bit more of the object, so we can see all the
      -- pointers.
      s' <- lift $ quadtraverse dereferencePapPayload dereferenceConDesc dereferenceStack pure s
      -- Step 2: Replace all the pointers in the closure by things they are
      -- equivalent to we have already seen.
      s''  <- quadtraverse (traverse rep_c) pure (traverse rep_c) rep_c s'
      -- Step 3: Have we seen a closure like this one before?
      modify' (addEquiv cp s'')

{-
        -- Yes, we have seen something of identical shape
        -- 1. Update equivalence maps to map this closure into the right
        -- equivalence class
        Just new_cp -> do
          n <- checkSize <$> lift get
          traceShowM n
          lift $ modify' (addNewMap cp new_cp)
          return new_cp
          --
        -- No, never seen something like this before
        -- Add the mapping to emap
        Nothing -> do
          lift $ modify' (addNewEquiv cp s'')
          return cp
          -}

      -- Step 4: Update the census, now we know the equivalence class of
      -- the object
      --lift $ modify' (go new_cp s'')
      -- Step 5: Call the continuation to carry on with the analysis
      k

    rep_c cp@(ClosurePtr k) = do
      m <- gets emap2
      case IM.lookup (fromIntegral k) m of
        -- There is an equivalence class already
        Just cp' -> return cp'
        -- No equivalence class yet
        Nothing -> return cp

printObjectEquiv :: EquivMap -> IO ()
printObjectEquiv c = do
  let cmp (_, b,_) = b
      res = sortBy (flip (comparing cmp)) (PS.toList c)
      showLine (k, p, v) =
        concat [show v, ":", show p,":", ppClosure "" (\_ -> show) 0 (noSize k)]
  mapM_ (putStrLn . showLine) res
--  writeFile "profile/profile_out.txt" (unlines $ "key, total, count, max, avg" : (map showLine res))

objectEquivAnalysis :: DebugM (EquivMap, HeapGraph Size)
objectEquivAnalysis = do
  precacheBlocks
  rs <- gcRoots
  traceWrite (length rs)
  r1 <- emap <$> censusObjectEquiv rs
  let elems = snd $ PS.atMostView of_interest r1
      cmp (_, b,_) = b
      cps = map (\(_, _, cp) -> cp) (sortBy (flip (comparing cmp)) (PS.toList elems))
  -- Use this code if we are returning ClosurePtr not SourceInformation
  r2 <- case cps of
    [] -> error "None"
    (c:cs) -> multiBuildHeapGraph (Just 10) (c :| cs)
  return (r1, r2)

objectEquiv :: Debuggee -> IO ()
objectEquiv = runAnalysis objectEquivAnalysis $ \(rmap, hg) -> do
                                                    printObjectEquiv rmap
                                                    putStrLn $ ppHeapGraph show hg
