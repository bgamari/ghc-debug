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
module GHC.Debug.Client.Profile where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Client
import           GHC.Debug.Client.Trace

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
type CensusByMBlock = Map.Map Text CensusStats

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



closureToKey :: GHC.Debug.Types.DebugClosure a ConstrDesc c d -> Text
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
              , visitedVal = ()
              , conDescTrace = const (return ())

            }
    -- Add cos
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> StateT TraceState (StateT (Map.Map k v) DebugM) ()
               -> StateT TraceState (StateT (Map.Map k v) DebugM) ()
    closAccum cp s k = do
      s' <- lift $ lift $ quadtraverse pure dereferenceConDesc pure pure s
      lift $ modify (go cp s')
      k

    go :: ClosurePtr -> SizedClosureC -> Map.Map k v -> Map.Map k v
    go cp d = case f cp d of
                Just (k, v) -> Map.insertWith (<>) k v
                Nothing -> id

-- | General function for performing a heap census in constant memory
census2LevelClosureType :: [ClosurePtr] -> DebugM CensusByClosureType
census2LevelClosureType cps = snd <$> runStateT (traceFromM funcs cps) Map.empty
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = ()
              , conDescTrace = const (return ())

            }
    -- Add cos
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> StateT TraceState (StateT CensusByClosureType DebugM) ()
               -> StateT TraceState (StateT CensusByClosureType DebugM) ()
    closAccum _ s k = do
      s' <- lift $ lift $ quadtraverse dereferencePapPayload dereferenceConDesc dereferenceStack pure s
      pts <- lift $ lift $ mapM dereferenceClosureFromBlock (allClosures (noSize s'))
      pts' <- lift $ lift $ mapM (quadtraverse pure dereferenceConDesc pure pure) pts


      lift $ modify' (go s' pts')
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


profile :: Int -> DebugEnv DebugM -> IO ()
profile interval e = loop [(0, Map.empty)] 0
  where
    loop :: [(Int, CensusByClosureType)] -> Int -> IO ()
    loop ss i = do
      threadDelay interval
      run e $ request RequestPause
      r <- runTrace e $ do
        precacheBlocks
        rs <- request RequestRoots
        traceWrite (length rs)
        r <- census2LevelClosureType rs
        return r
      run e $ request RequestResume
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


