{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
module GHC.Debug.Client.ObjectEquiv(objectEquiv) where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Client
import           GHC.Debug.Client.Trace
import           GHC.Debug.Client.Profile
import           GHC.Debug.Types.Graph

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.RWS
import Data.List
import Data.Ord
import Control.Concurrent
import Eventlog.Types
import Eventlog.Data
import Eventlog.Total
import Eventlog.HtmlTemplate
import Eventlog.Args (defaultArgs)
import Data.Text (pack, Text, unpack)
import Data.Semigroup
import qualified Data.Text as T
import Debug.Trace
import qualified Data.OrdPSQ as PS
import qualified Data.IntMap.Strict as IM
import Data.List.NonEmpty(NonEmpty(..))

derefFuncM c = do
  c <- dereferenceClosureFromBlock c
  quadtraverse dereferencePapPayload dereferenceConDesc dereferenceStack pure c

type CensusByObjectEquiv = IM.IntMap CensusStats

limit = 100_000
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
                          , census :: !CensusByObjectEquiv
                          }
-- Don't need to add identity mapping in emap2 because lookup failure is
-- the identity anyway.
addEquiv :: ClosurePtr -> PtrClosure -> ObjectEquivState -> ObjectEquivState
addEquiv cp pc (trimMap -> o) =
                  let (res, new_m) = (PS.alter g pc (emap o))
                      new_emap2 = case res of
                                     Left _ -> emap2 o
                                     -- Only remap objects which have a hit
                                     -- in the cache
                                     Right new_cp -> addNewMap cp new_cp (emap2 o)
                  in ( o { emap = new_m
                      , emap2 = new_emap2 })
  where
    g Nothing = (Left cp, Just (0, cp))
    g (Just (p, v)) = (Right v, Just ((p + 1), v))

addNewMap :: ClosurePtr -> ClosurePtr -> Equiv2Map -> Equiv2Map
addNewMap (ClosurePtr cp) equiv_cp o = IM.insert (fromIntegral cp) equiv_cp o

-- Trim down map to keep objects which have only been seen 100 times or
-- more within the last 10 000 seen objects
trimMap :: ObjectEquivState -> ObjectEquivState
trimMap o = if checkSize o > limit
              then let new_o = o { emap = snd $ PS.atMostView of_interest (emap o) }
                   in traceShow (checkSize new_o) new_o

              else o

checkSize :: ObjectEquivState -> Int
checkSize (ObjectEquivState e1 e2 _) = PS.size e1

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
      res = reverse (sortBy (comparing cmp) (PS.toList c))
      showLine (k, p, v) =
        concat [show v, ":", show p,":", ppClosure "" (\_ -> show) 0 (noSize k)]
  mapM_ (putStrLn . showLine) res
--  writeFile "profile/profile_out.txt" (unlines $ "key, total, count, max, avg" : (map showLine res))


objectEquiv :: Debuggee -> IO ()
objectEquiv e = do
  pause e
  r <- runTrace e $ do
        precacheBlocks
        rs <- request RequestRoots
        traceWrite (length rs)
        r <- censusObjectEquiv rs
        return r
  resume e
  let elems = (snd $ PS.atMostView of_interest (emap r))
      cmp (_, b,_) = b
      cps = map (\(_, _, cp) -> cp) (reverse (sortBy (comparing cmp) (PS.toList elems)))
  -- Use this code if we are returning ClosurePtr not SourceInformation
  (hg, _) <- run e $ case cps of
    [] -> error "None"
    (c:cs) -> multiBuildHeapGraph derefFuncM (Just 10) (c :| cs)
--  let cs = map (flip GHC.Debug.Types.Graph.lookupHeapGraph hg) top10
--  mapM print (zip top10 cs)
  putStrLn $ ppHeapGraph show hg
  (printObjectEquiv elems)
  return ()
