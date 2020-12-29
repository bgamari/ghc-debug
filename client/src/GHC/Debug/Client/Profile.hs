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

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
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
import Data.Text (pack)


data TraceState s = TraceState { visited :: !(IS.IntSet), user :: !s }

addVisit :: ClosurePtr -> TraceState s -> TraceState s
addVisit (ClosurePtr c) st = st { visited = IS.insert (fromIntegral c) (visited st) }

checkVisit :: ClosurePtr -> TraceState s -> Bool
checkVisit (ClosurePtr c) st = IS.member (fromIntegral c) (visited st)

updUser :: (st -> st) -> TraceState st -> TraceState st
updUser f st = st { user = (f (user st)) }

-- Traverse the tree from GC roots, to populate the caches
-- with everything necessary.
traceFromM :: (SizedClosure -> st -> st) -> st -> [ClosurePtr] -> DebugM st
traceFromM k i cps = user <$> execStateT (mapM_ (traceClosureFromM k) cps) (TraceState IS.empty i)

traceClosureFromM :: (SizedClosure -> st -> st) -> ClosurePtr -> StateT (TraceState st) DebugM ()
traceClosureFromM k cp = do
    m <- get
    unless (checkVisit cp m) $ do
      modify (addVisit cp)
      sc <- lift $ dereferenceClosureFromBlock cp
      modify (updUser (k sc))
      tritraverse traceConstrDescM traceStackFromM (traceClosureFromM k) sc
      case lookupStgInfoTableWithPtr (noSize sc) of
        Nothing -> return Nothing
        Just infoTableWithptr -> lift $ request (RequestSourceInfo (tableId infoTableWithptr))
      return ()

traceStackFromM :: StackCont -> StateT s DebugM ()
traceStackFromM st = lift $ () <$ dereferenceStack st

traceConstrDescM :: ConstrDescCont -> StateT s DebugM ()
traceConstrDescM d = lift $ () <$ dereferenceConDesc d

newtype Count = Count Int
                deriving (Semigroup, Monoid, Num) via Sum Int

data CensusStats = CS { n :: Count, cssize :: Size  }

instance Semigroup CensusStats where
  (CS a b) <> (CS a1 b1) = CS (a <> a1) (b <> b1)

type CensusByClosureType = IM.IntMap CensusStats

censusClosureType :: [ClosurePtr] -> DebugM CensusByClosureType
censusClosureType = traceFromM go IM.empty
  where
    go :: SizedClosure -> CensusByClosureType -> CensusByClosureType
    go d =
      let s :: Size
          s = dcSize d
          v =  CS (Count 1) s
      in case lookupStgInfoTableWithPtr (noSize d) of
           Just itbl ->
              let k :: ClosureType
                  k = tipe (decodedTable itbl)
              in IM.insertWith (<>) (fromEnum k) v
           Nothing -> id


printCensusByClosureType :: CensusByClosureType -> IO ()
printCensusByClosureType c =
  let res = reverse (sortBy (comparing (cssize . snd)) (IM.toList c))
      showLine (k, (CS (Count n) (Size s))) =
        concat [show @ClosureType (toEnum k), ":", show s,":", show n,":", show @Double (fromIntegral s / fromIntegral n)]
  in mapM_ (putStrLn . showLine) res

profile :: Int -> DebugEnv DebugM -> IO ()
profile interval e = loop [(0, IM.empty)] 0
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
      printCensusByClosureType r
      run e $ request RequestResume
      let new_data = (((i + 1) * interval, r) : ss)
      renderProfile new_data
      loop new_data (i + 1)

mkFrame :: (Int, CensusByClosureType) -> Frame
mkFrame (t, m) = Frame (fromIntegral t / 10e6) (IM.foldrWithKey (\k v r -> mkSample k v : r) [] m)

mkSample :: Int -> CensusStats -> Sample
mkSample k (CS _ (Size v)) =
  Sample (Bucket (pack $ show @ClosureType (toEnum k))) (fromIntegral v)


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


