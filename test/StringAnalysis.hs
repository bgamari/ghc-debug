{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import GHC.Debug.Client
import GHC.Debug.Retainers
import GHC.Debug.Fragmentation
import GHC.Debug.Profile
import GHC.Debug.Dominators
import GHC.Debug.Snapshot
import GHC.Debug.Count
import GHC.Debug.TypePointsFrom
import GHC.Debug.Types.Ptr
import GHC.Debug.GML
import GHC.Debug.Types.Graph (heapGraphSize, traverseHeapGraph, ppClosure)
--import GHC.Debug.Types.Closures
import GHC.Debug.Trace
import GHC.Debug.ParTrace(tracePar)
import GHC.Debug.ObjectEquiv
import Control.Monad.RWS
import Control.Monad.Writer
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Foldable as F

import Control.Monad
import Debug.Trace
import Control.Exception
import Control.Concurrent
import Data.Bitraversable
import Data.Monoid
import Control.Applicative
import GHC.Debug.Client.Monad

import Data.List.Extra (trim)
import System.Process
import Data.Tree
import Data.Maybe
import qualified Data.Map as Map
import Data.Ord
import Data.List (sort, sortBy, (\\))
import qualified Data.Set as S
import Data.List.NonEmpty(NonEmpty(..), fromList)
import Data.Semigroup
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Extra
import Data.Char
import Data.Bitraversable
import Control.Monad
--main = withDebuggeeConnect "/tmp/ghc-debug" (\e -> pp e)

main = snapshotRun "/home/matt/.local/share/ghc-debug/debuggee/snapshots/hls-mercury-7" string_prog

-- Find all the strings and then print out how many duplicates there are
string_prog e = do
  pause e
  res <- runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    res <- stringAnalysis rs
--    res <- arrWordsAnalysis rs
    return res

  printResult (Map.map (\s -> Count (S.size s)) res)
  printResult (Map.mapWithKey (\k s -> Count (fromIntegral (length k) * (S.size s))) res)
  let anal n = do
        let cools = fromJust (Map.lookup n res)
        print cools
        stacks <- run e $ do
          roots <- gcRoots
          rets <- findRetainersOf (Just (S.size cools)) Nothing roots (S.toList cools)
          rets' <- traverse (\c -> (show (head c),) <$> (addLocationToStack' c)) rets
          return rets'
        displayRetainerStack' stacks

--  anal "ome/matt/mercury"
--  anal "/hie.yaml"
  anal "/nix/store/cjv9qdysvas6say6s09sb5i016w11gj2-ghc-9.2.2-doc/share/doc/ghc/html/libraries/base-4.16.1.0/src/GHC-Base.html"

-- | Find the parents of Bin nodes
stringAnalysis :: [ClosurePtr] -> DebugM (Map.Map _ _)
stringAnalysis rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) () (Map.empty)
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , srtTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }

    getKey :: InfoTablePtr -> DebugM String
    getKey itblp = do
      loc <- getSourceInfo itblp
      itbl <- dereferenceInfoTable itblp
      case loc of
        Nothing -> getKeyFallback itblp itbl
        Just s -> return $ show (tipe itbl) ++ ":" ++ renderSourceInfo s

    renderSourceInfo :: SourceInformation -> String
    renderSourceInfo s = (infoName s ++ ":" ++ infoType s ++ ":" ++ infoPosition s)

    getKeyFallback itbp itbl = do
      case tipe itbl of
        t | CONSTR <= t && t <= CONSTR_NOCAF   -> do
          ConstrDesc a b c <- dereferenceConDesc itbp
          return $ a ++ ":" ++ b ++ ":" ++ c
        _ -> return $ show (tipe itbl)
    -- First time we have visited a closure
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> (RWST () () (Map.Map _ _) DebugM) ()
               -> (RWST () () (Map.Map _ _) DebugM) ()
    closAccum cp sc k = do
      case noSize sc of
        ConstrClosure _ info ps ds cd -> do
          cd' <- lift $ dereferenceConDesc cd
          case cd' of
            ConstrDesc _ _ cd | cd /= ":" -> do
              process sc
            _ -> return ()
        other  -> process sc
      k
      where
        process sc = do
          s' <- lift $ hextraverse pure dereferenceSRT dereferencePapPayload dereferenceConDesc (bitraverse dereferenceSRT pure <=< dereferenceStack) pure sc
          pts <- lift $ mapM dereferenceClosure (allClosures (noSize s'))
          pts' <- lift $ mapM addConstrDesc pts
          checked <- lift $ filterM (\(_, c) -> check_bin (noSize c)) (zip (allClosures (noSize s')) pts')
          case checked of
            -- No children are Bin, don't care
            [] ->  return ()
            -- Has bin children, record the closure
            xs  -> do
              loc <- lift $ getKey (tableId $ info $ noSize sc)
              ds <- lift $ mapM (decodeString . fst) xs
              let f :: Map.Map _ _ -> Map.Map _ _
                  f = (\m1 -> foldr (\(x, d) m -> Map.insertWith (<>) d (S.singleton x) m) m1 (zip (map fst xs) ds))
              modify' f

        process_2 cp = do
          cp' <- dereferenceClosure cp
--          traceShowM cp'
          case noSize cp' of
            (ConstrClosure _ _ _ _ cd) -> do
              (ConstrDesc _ _ cn) <- dereferenceConDesc cd
              return (cn == "C#")
            _ -> return False


        check_char (ConstrClosure _ _ _ _ (ConstrDesc _ _ "C#")) = True
        check_char _ = False

        check_bin (ConstrClosure _ _ (p:_) _ (ConstrDesc _ _ ":")) = process_2 p
        check_bin _ = return False

decodeString :: ClosurePtr -> DebugM String
decodeString cp = do
  cp' <- dereferenceClosure cp
  case noSize cp' of
    (ConstrClosure _ _ [p,ps] _ cd) -> do
      cp' <- dereferenceClosure p
      case noSize cp' of
        (ConstrClosure _ _ _ [w] _) -> do
          (chr (fromIntegral w):) <$> decodeString ps
        _ -> return []
    _ -> return []


printResult :: Show a => Map.Map a Count -> IO [a]
printResult m = do
  putStrLn $ "TOTAL: " ++ show total
  mapM_ show_line top10
  return (map fst top10)
  where
    show_line (k, Count v) = T.putStrLn (T.pack (show k) <> ": " <> T.pack (show v))
    top10 = take 1000 $ reverse (sortBy (comparing snd) (Map.toList m))
    total = F.fold (Map.elems m)

-- | Find how many ArrWords there are
arrWordsAnalysis :: [ClosurePtr] -> DebugM (Map.Map _ _)
arrWordsAnalysis rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) () (Map.empty)
  where
    funcs = justClosures closAccum

    -- First time we have visited a closure
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> (RWST () () (Map.Map _ _) DebugM) ()
               -> (RWST () () (Map.Map _ _) DebugM) ()
    closAccum cp sc k = do
          case (noSize sc) of
            ArrWordsClosure _ _ _ p ->  do
              modify' (Map.insertWith (<>) (arrWordsBS p) (S.singleton cp))
              k
            _ -> k
