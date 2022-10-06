{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module GHC.Debug.Strings ( stringProgram, arrWordsProgram
                         , arrWordsAnalysis, stringAnalysis) where

import GHC.Debug.Client
import GHC.Debug.Types.Ptr
import GHC.Debug.Trace
import GHC.Debug.Profile.Types
import Control.Monad.RWS
import qualified Data.Foldable as F

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Map as Map
import qualified Data.Set as S
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS (length)
import Data.Char
import Data.Ord
import Data.List

-- | Find all the strings and then print out how many duplicates there are
stringProgram :: Debuggee -> IO ()
--arrWordsProgram :: Debuggee -> IO ()
stringProgram = programX length stringAnalysis
arrWordsProgram = programX (fromIntegral . BS.length) arrWordsAnalysis

programX :: Show a => (a -> Int) -> ([ClosurePtr] -> DebugM (Map.Map a (S.Set b))) -> Debuggee -> IO ()
programX sizeOf anal e = do
  pause e
  res <- runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    res <- anal rs
    return res
  printResult (Map.map (\s -> Count (S.size s)) res)
  printResult (Map.mapWithKey (\k s -> Count (fromIntegral (sizeOf k) * (S.size s))) res)
  return ()

  {-
  let anal n = do
        let cools = fromJust (Map.lookup n res)
        print cools
        stacks <- run e $ do
          roots <- gcRoots
          rets <- findRetainersOf (Just (S.size cools)) roots (S.toList cools)
          rets' <- traverse (\c -> (show (head c),) <$> (addLocationToStack' c)) rets
          return rets'
        displayRetainerStack' stacks
        -}

-- | Find the parents of Bin nodes
stringAnalysis :: [ClosurePtr] -> DebugM (Map.Map String (S.Set ClosurePtr))
stringAnalysis rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) False (Map.empty)
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }

    -- First time we have visited a closure
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> (RWST Bool () (Map.Map String (S.Set ClosurePtr)) DebugM) ()
               -> (RWST Bool () (Map.Map String (S.Set ClosurePtr)) DebugM) ()
    closAccum cp sc k = do
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          cd' <- lift $ dereferenceConDesc cd
          case cd' of
            ConstrDesc _ _ cd2 | cd2 == ":" -> do
              process cp sc k
            _ -> local (const False) k
        _  -> local (const False) k
      where
        process :: ClosurePtr -> SizedClosure
                -> (RWST Bool () (Map.Map String (S.Set ClosurePtr)) DebugM) ()
                -> (RWST Bool () (Map.Map String (S.Set ClosurePtr)) DebugM) ()
        process cp clos k = do
          clos' <- lift $ quadtraverse pure dereferenceConDesc return return (noSize clos)
          checked <- lift $ check_bin clos'
          if checked
            then do
              parent_is_cons <- ask
              if parent_is_cons
                then local (const True) k
                else do
                  ds <- lift $ decodeString cp
                  modify' (Map.insertWith (<>) ds (S.singleton cp))
                  local (const True) k
            else local (const False) k

        process_2 cp = do
          cp' <- dereferenceClosure cp
          case noSize cp' of
            (ConstrClosure _ _ _ cd) -> do
              (ConstrDesc _ _ cn) <- dereferenceConDesc cd
              return (cn == "C#")
            _ -> return False

        check_bin (ConstrClosure _ [h,_] _ (ConstrDesc _ _ ":")) = process_2 h
        check_bin _ = return False

decodeString :: ClosurePtr -> DebugM String
decodeString cp = do
  cp' <- dereferenceClosure cp
  case noSize cp' of
    (ConstrClosure _ [p,ps] _ _) -> do
      cp'' <- dereferenceClosure p
      case noSize cp'' of
        (ConstrClosure _ _ [w] _) -> do
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

-- | Find how many distinct ArrWords there are
arrWordsAnalysis :: [ClosurePtr] -> DebugM (Map.Map ByteString (S.Set ClosurePtr))
arrWordsAnalysis rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) () (Map.empty)
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }

    -- First time we have visited a closure
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> (RWST () () (Map.Map ByteString (S.Set ClosurePtr)) DebugM) ()
               -> (RWST () () (Map.Map ByteString (S.Set ClosurePtr)) DebugM) ()
    closAccum cp sc k = do
          case (noSize sc) of
            ArrWordsClosure _ _ p ->  do
              modify' (Map.insertWith (<>) (arrWordsBS p) (S.singleton cp))
              k
            _ -> k
