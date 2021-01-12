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
{- | Functions for performing whole heap census in the style of the normal
- heap profiling -}
module GHC.Debug.Profile( profile
                        , censusClosureType
                        , census2LevelClosureType
                        , closureCensusBy
                        , CensusByClosureType
                        , printCensusByClosureType
                        , CensusStats(..)
                        , mkCS
                        , Count(..)
                        , closureToKey ) where

import GHC.Debug.Client.Monad
import GHC.Debug.Client
import GHC.Debug.Trace

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


newtype Count = Count Int
                deriving (Semigroup, Monoid, Num) via Sum Int
                deriving (Show, Ord, Eq)

data CensusStats = CS { n :: !Count, cssize :: !Size, csmax :: !(Max Size) } deriving Show

mkCS :: Size -> CensusStats
mkCS i = CS (Count 1) i (Max i)

instance Semigroup CensusStats where
  (CS a b c) <> (CS a1 b1 c1) = CS (a <> a1) (b <> b1) (c <> c1)

type CensusByClosureType = Map.Map Text CensusStats

-- | Perform a heap census in the same style as the -hT profile.
censusClosureType :: [ClosurePtr] -> DebugM CensusByClosureType
censusClosureType = closureCensusBy go
  where
    go :: ClosurePtr -> DebugClosureWithSize a ConstrDesc b c
       -> Maybe (Text, CensusStats)
    go _ d =
      let s :: Size
          s = dcSize d
          v =  CS (Count 1) s (Max s)
      in Just (closureToKey (noSize d), v)



closureToKey :: DebugClosure a ConstrDesc c d -> Text
closureToKey d =
  case d of
     ConstrClosure { constrDesc = ConstrDesc a b c }
       -> pack a <> ":" <> pack b <> ":" <> pack c
     _ -> pack (show (tipe (decodedTable (info d))))


-- | General function for performing a heap census in constant memory
closureCensusBy :: forall k v . (Semigroup v, Ord k)
                => (ClosurePtr -> SizedClosureC -> Maybe (k, v))
                -> [ClosurePtr] -> DebugM (Map.Map k v)
closureCensusBy f cps = snd <$> runStateT (traceFromM funcs cps) Map.empty
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
               ->  (StateT (Map.Map k v) DebugM) ()
               ->  (StateT (Map.Map k v) DebugM) ()
    closAccum cp s k = do
      s' <- lift $ quadtraverse pure dereferenceConDesc pure pure s
      modify' (go cp s')
      k

    go :: ClosurePtr -> SizedClosureC -> Map.Map k v -> Map.Map k v
    go cp d = case f cp d of
                Just (k, v) -> Map.insertWith (<>) k v
                Nothing -> id

-- | Perform a 2-level census where the keys are the type of the closure
-- in addition to the type of ptrs of the closure. This can be used to
-- distinguish between lists of different type for example.
census2LevelClosureType :: [ClosurePtr] -> DebugM CensusByClosureType
census2LevelClosureType cps = snd <$> runStateT (traceFromM funcs cps) Map.empty
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
               -> (StateT CensusByClosureType DebugM) ()
               -> (StateT CensusByClosureType DebugM) ()
    closAccum _ s k = do
      s' <- lift $ quadtraverse dereferencePapPayload dereferenceConDesc dereferenceStack pure s
      pts <- lift $ mapM dereferenceClosure (allClosures (noSize s'))
      pts' <- lift $ mapM (quadtraverse pure dereferenceConDesc pure pure) pts


      modify' (go s' pts')
      k

    go d args =
      let k = closureToKey (noSize d)
          kargs = map (closureToKey . noSize) args
          final_k :: Text
          final_k = k <> "[" <> T.intercalate "," kargs <> "]"
      in Map.insertWith (<>) final_k (mkCS (dcSize d))


printCensusByClosureType :: CensusByClosureType -> IO ()
printCensusByClosureType c = do
  let res = reverse (sortBy (comparing (cssize . snd)) (Map.toList c))
      showLine (k, (CS (Count n) (Size s) (Max (Size mn)))) =
        concat [unpack k, ":", show s,":", show n, ":", show mn,":", show @Double (fromIntegral s / fromIntegral n)]
  writeFile "profile/profile_out.txt" (unlines $ "key, total, count, max, avg" : (map showLine res))


-- | Peform a profile at the given interval (in seconds), the result will
-- be rendered after each iteration using @eventlog2html@.
profile :: Int -> Debuggee -> IO ()
profile interval e = loop [(0, Map.empty)] 0
  where
    loop :: [(Int, CensusByClosureType)] -> Int -> IO ()
    loop ss i = do
      threadDelay (interval * 1_000_000)
      pause e
      r <- runTrace e $ do
        precacheBlocks
        rs <- gcRoots
        traceWrite (length rs)
        r <- census2LevelClosureType rs
        return r
      resume e
      printCensusByClosureType r
      let new_data = (((i + 1) * interval, r) : ss)
      renderProfile new_data
      loop new_data (i + 1)


mkFrame :: (Int, CensusByClosureType) -> Frame
mkFrame (t, m) = Frame (fromIntegral t / 10e6) (Map.foldrWithKey (\k v r -> mkSample k v : r) [] m)

mkSample :: Text -> CensusStats -> Sample
mkSample k (CS _ (Size v) _) =
  Sample (Bucket k) (fromIntegral v)

mkProfData :: [(Int, CensusByClosureType)] -> ProfData
mkProfData raw_fs =
  let fs = map mkFrame raw_fs
      (counts, totals) = total fs
      -- Heap profiles don't contain any other information than the simple bucket name
      binfo = Map.mapWithKey (\(Bucket k) (t,s,g) -> BucketInfo k Nothing t s g) totals
  -- Heap profiles do not support traces
      header = Header "ghc-debug" "" (Just HeapProfBreakdownClosureType) "" "" "" counts Nothing
  in (ProfData header binfo mempty fs [] mempty)

renderProfile :: [(Int, CensusByClosureType)] -> IO ()
renderProfile ss = do
  let pd = mkProfData ss
  as <- defaultArgs "unused"
  (header, data_json, descs, closure_descs) <- generateJsonData as pd
  let html = templateString header data_json descs closure_descs as
  writeFile ("profile/ht.html") html
  return ()


