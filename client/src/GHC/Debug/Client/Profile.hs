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
module GHC.Debug.Client.Profile where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Client
import           GHC.Debug.Client.Trace

import qualified Data.IntSet as IS
import qualified Data.Map as Map
import Control.Monad.State
import Data.Monoid
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



newtype Count = Count Int
                deriving (Semigroup, Monoid, Num) via Sum Int

data CensusStats = CS { n :: Count, cssize :: Size, csmax :: Max Size }

instance Semigroup CensusStats where
  (CS a b c) <> (CS a1 b1 c1) = CS (a <> a1) (b <> b1) (c <> c1)

type CensusByClosureType = Map.Map Text CensusStats

censusClosureType :: [ClosurePtr] -> DebugM CensusByClosureType
censusClosureType cps = snd <$> runStateT (traceFromM funcs cps) Map.empty
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = ()
              , conDescTrace = \x -> return x

            }
    -- Add cos
    closAccum  :: DebugClosureWithSize () ConstrDesc () () -> StateT CensusByClosureType DebugM ()
    closAccum s = do
      modify (go s)

    go :: DebugClosureWithSize () ConstrDesc () () -> CensusByClosureType -> CensusByClosureType
    go d =
      let s :: Size
          s = dcSize d
          v =  CS (Count 1) s (Max s)
      in case lookupStgInfoTableWithPtr (noSize d) of
           itbl ->
              let k :: Text
                  k = case (noSize d) of
                        ConstrClosure { constrDesc = ConstrDesc a b c }
                          -> pack a <> ":" <> pack b <> ":" <> pack c
                        _ -> pack (show (tipe (decodedTable itbl)))
              in Map.insertWith (<>) k v


printCensusByClosureType :: CensusByClosureType -> IO ()
printCensusByClosureType c = do
  let res = reverse (sortBy (comparing (cssize . snd)) (Map.toList c))
      showLine (k, (CS (Count n) (Size s) (Max (Size mn)))) =
        concat [unpack k, ":", show s,":", show n, ":", show mn,":", show @Double (fromIntegral s / fromIntegral n)]
  mapM_ (putStrLn . showLine) res
  --writeFile "profile_out" (unlines (map showLine res))

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
        censusClosureType rs
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
      n = maximum (map fst ss)
  writeFile ("profile/ht-" ++ show n ++ ".html") html

  return ()


