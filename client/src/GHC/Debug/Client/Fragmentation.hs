{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Functions for analysing memory fragmentation
module GHC.Debug.Client.Fragmentation (summariseBlocks
                                      , censusByMBlock
                                      , outMBlockCensus
                                      , censusByBlock
                                      , outBlockCensus
                                      , censusPinnedBlocks

                                      , findBadPtrs
                                      , displayArrWords
                                      ) where

import GHC.Debug.Client.Profile
import GHC.Debug.Types
import GHC.Debug.Client.Monad

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Ord
import Data.Text (pack, Text)
import Data.Semigroup
import Data.Word

-- | Print a summary of the given raw blocks
-- This is useful to see how many MBlocks and how many pinned blocks there
-- are.
summariseBlocks :: [RawBlock] -> IO ()
summariseBlocks bs = do
  print ("MBLOCK:", length mblocks)
  print ("PINNED:", (length $ filter isPinnedBlock bs))
  print ("TOTAL:", length bs)
  where
    mblocks = map (\mbs@(g:_) -> (g, length mbs)) (group (sort (map fst bs')))
    bs' = map go bs
    go b = (blockMBlock (rawBlockAddr b), isPinnedBlock b)

censusByMBlock :: [ClosurePtr] -> DebugM (Map.Map BlockPtr CensusStats)
censusByMBlock = closureCensusBy go
  where
    go cp d =
      let s :: Size
          s = dcSize d
          v =  CS (Count 1) s (Max s)

          k :: BlockPtr
          k = applyMBlockMask cp
      in if heapAlloced cp
           then Just (k, v)
           -- Ignore static things
           else Nothing

censusByBlock :: [ClosurePtr] -> DebugM CensusByClosureType
censusByBlock = closureCensusBy go
  where
    go cp d =
      let s :: Size
          s = dcSize d
          v =  mkCS s

          k :: Text
          k = pack (show (applyBlockMask cp))
      in if heapAlloced cp
           then Just (k, v)
           -- Ignore static things
           else Nothing

-- Only census the given (pinned) blocks
censusPinnedBlocks :: [RawBlock]
                   -> [ClosurePtr]
                   -> DebugM (Map.Map BlockPtr (CensusStats, [(ClosurePtr, DebugClosureWithSize () () () ())]))
censusPinnedBlocks bs = closureCensusBy go
  where
    pbs = Set.fromList (map rawBlockAddr (filter isPinnedBlock bs))
    go :: forall a b c string . ClosurePtr
          -> DebugClosureWithSize a string b c
          -> Maybe (BlockPtr, (CensusStats, [(ClosurePtr, DebugClosureWithSize () () () ())]))
    go cp d =
      let s :: Size
          s = dcSize d
          v =  mkCS s
          bp = applyBlockMask cp

          f _ = ()
          neut :: DebugClosureWithSize a string b c -> DebugClosureWithSize () () () ()
          neut = quadmap f f f f
      in if heapAlloced cp && bp `Set.member` pbs
           then Just (bp, (v, [(cp, neut d)]))
           -- Ignore static things
           else Nothing


-- | Given a pinned block census, find the ARR_WORDS objects which are in the
-- blocks which are < 10 % utilised. The return list is sorted by how many
-- times each distinct ARR_WORDS appears on the heap.
findBadPtrs :: Map.Map k (CensusStats, [(ClosurePtr, DebugClosureWithSize () () () ())])
            -> [((Count, [ClosurePtr]), String)]
findBadPtrs mb_census  =
      let fragged_blocks = Map.filter (\((CS _ (Size s) _), _) -> fromIntegral s / fromIntegral blockMaxSize <= (0.1 :: Double))  mb_census
          all_arr_words :: [(String, (Count, [ClosurePtr]))]
          all_arr_words = concatMap (\(_, i) -> map (\(c,d) -> (displayArrWords d, (Count 1, [c]))) i) (Map.elems fragged_blocks)
          swap (a, b) = (b, a)
          dups = map swap (reverse $ sortBy (comparing snd) (Map.toList (Map.fromListWith (<>) all_arr_words)))
      in dups

displayArrWords :: DebugClosureWithSize () () () () -> String
displayArrWords d =
    case noSize d of
      ArrWordsClosure { arrWords } -> show (arrWordsBS arrWords)
      _ -> error "Not ARR_WORDS"

outMBlockCensus, outBlockCensus ::  Map.Map BlockPtr CensusStats -> IO ()
outMBlockCensus = printBlockCensus mblockMaxSize
-- | Print out a block census
outBlockCensus = printBlockCensus blockMaxSize

-- | Print either a MBlock or Block census as a histogram
printBlockCensus :: Word64 -> Map.Map BlockPtr CensusStats -> IO ()
printBlockCensus maxSize m =
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

