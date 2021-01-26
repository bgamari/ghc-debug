{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Functions for analysing memory fragmentation
module GHC.Debug.Fragmentation (summariseBlocks
                                     , censusByMBlock
                                      , printMBlockCensus
                                      , censusByBlock
                                      , printBlockCensus
                                      , censusPinnedBlocks
                                      , PinnedCensusStats(..)

                                      , findBadPtrs
                                      ) where

import GHC.Debug.Profile
import GHC.Debug.Client
import GHC.Debug.Types
--import GHC.Debug.Client.Monad

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Ord
import Data.Word

-- | Print a summary of the given raw blocks
-- This is useful to see how many MBlocks and how many pinned blocks there
-- are.
summariseBlocks :: [RawBlock] -> IO ()
summariseBlocks bs = do
  putStrLn ("TOTAL BLOCKS: " ++ show (length bs))
  putStrLn ("PINNED BLOCKS: " ++ show (length $ filter isPinnedBlock bs))
  putStrLn ("MBLOCK: " ++ show n_mblocks)
  where
    n_mblocks :: Int
    n_mblocks = length (nub (map (blockMBlock . rawBlockAddr) bs))

-- | Perform a heap census by which MBlock each closure lives in
censusByMBlock :: [ClosurePtr] -> DebugM (Map.Map BlockPtr CensusStats)
censusByMBlock = closureCensusBy go
  where
    go cp d =
      let s :: Size
          s = dcSize d
          v =  mkCS s

          k :: BlockPtr
          k = applyMBlockMask cp
      in if heapAlloced cp
           then return $ Just (k, v)
           -- Ignore static things
           else return $ Nothing

-- | Perform a census based on which block each closure resides in.
censusByBlock :: [ClosurePtr] -> DebugM (Map.Map BlockPtr CensusStats)
censusByBlock = closureCensusBy go
  where
    go cp d =
      let s :: Size
          s = dcSize d
          v =  mkCS s

          k = applyBlockMask cp
      in if heapAlloced cp
           then return $ Just (k, v)
           -- Ignore static things
           else return $ Nothing

newtype PinnedCensusStats =
          PinnedCensusStats (CensusStats, [(ClosurePtr, SizedClosure)])
          deriving (Semigroup)

-- | Only census the given (pinned) blocks
censusPinnedBlocks :: [RawBlock]
                   -> [ClosurePtr]
                   -> DebugM (Map.Map BlockPtr PinnedCensusStats)
censusPinnedBlocks bs = closureCensusBy go
  where
    pbs = Set.fromList (map rawBlockAddr (filter isPinnedBlock bs))
    go :: ClosurePtr -> SizedClosure
          -> DebugM (Maybe (BlockPtr, PinnedCensusStats))
    go cp d =
      let v :: CensusStats
          v = mkCS (dcSize d)

          bp = applyBlockMask cp

      in return $ if bp `Set.member` pbs
           then Just (bp, PinnedCensusStats (v, [(cp, d)]))
           -- Ignore static things
           else Nothing


-- | Given a pinned block census, find the ARR_WORDS objects which are in the
-- blocks which are < 10 % utilised. The return list is sorted by how many
-- times each distinct ARR_WORDS appears on the heap.
findBadPtrs :: Map.Map k PinnedCensusStats
            -> [((Count, [ClosurePtr]), String)]
findBadPtrs mb_census  =
      let fragged_blocks = Map.filter (\(PinnedCensusStats ((CS _ (Size s) _), _)) -> fromIntegral s / fromIntegral blockMaxSize <= (0.1 :: Double))  mb_census
          all_arr_words :: [(String, (Count, [ClosurePtr]))]
          all_arr_words = concatMap (\(PinnedCensusStats (_, i)) -> map (\(c,d) -> (displayArrWords d, (Count 1, [c]))) i) (Map.elems fragged_blocks)
          swap (a, b) = (b, a)
          dups = map swap (reverse $ sortBy (comparing snd) (Map.toList (Map.fromListWith (<>) all_arr_words)))
      in dups

displayArrWords :: SizedClosure -> String
displayArrWords d =
    case noSize d of
      ArrWordsClosure { arrWords } -> show (arrWordsBS arrWords)
      _ -> error "Not ARR_WORDS"

printMBlockCensus, printBlockCensus ::  Map.Map BlockPtr CensusStats -> IO ()
printMBlockCensus = printXBlockCensus mblockMaxSize
-- | Print out a block census
printBlockCensus = printXBlockCensus blockMaxSize

-- | Print either a MBlock or Block census as a histogram
printXBlockCensus :: Word64 -> Map.Map BlockPtr CensusStats -> IO ()
printXBlockCensus maxSize m =
  mapM_ (putStrLn . displayLine) (bin 0 (map calcPercentage (sortBy (comparing (cssize . snd)) (Map.toList m))))
  where
    calcPercentage (k, (CS _ (Size tot) _)) =
      (k, (fromIntegral tot/ fromIntegral maxSize) * 100 :: Double)

    displayLine (l, h, n) = show l ++ "%-" ++ show h ++ "%: " ++ show n

    bin _ [] = []
    bin k xs = case now of
                 [] -> bin (k + 10) later
                 _ -> (k, k+10, length now) : bin (k + 10) later
      where
        (now, later) = span ((<= k + 10) . snd) xs

