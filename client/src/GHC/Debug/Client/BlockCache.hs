{-# LANGUAGE ViewPatterns #-}
-- The BlockCache stores the currently fetched blocks
-- and is consulted first to avoid requesting too much
-- from the debuggee. The BlockCache can either be populated
-- via a call to RequestBlocks or on demand on a cache miss.

module GHC.Debug.Client.BlockCache where

import GHC.Debug.Types.Ptr
import GHC.Debug.Types
import Data.IntervalMap.Strict as I
import GHC.Word
import Data.Maybe
import System.Endian

data BlockCache = BlockCache (IntervalMap Word64 RawBlock)

emptyBlockCache :: BlockCache
emptyBlockCache = BlockCache I.empty

addBlock :: RawBlock -> BlockCache -> BlockCache
addBlock rb@(RawBlock (BlockPtr (fromBE64 -> bp)) _) (BlockCache bc) = BlockCache (I.insert (IntervalCO bp (bp + fromIntegral s)) rb bc)
  where
    s = rawBlockSize rb

addBlocks :: [RawBlock] -> BlockCache -> BlockCache
addBlocks bc bs = Prelude.foldr addBlock bs bc

lookupClosure :: ClosurePtr -> BlockCache -> Maybe RawBlock
lookupClosure (ClosurePtr (fromBE64 -> cp)) (BlockCache b) =
  snd <$> listToMaybe (toAscList (I.containing b cp))

bcSize :: BlockCache -> Int
bcSize (BlockCache b) = I.size b


