{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Data types for representing different pointers and raw information
-- All pointers are stored in little-endian to make arithmetic easier.
--
-- We have to send and recieve the pointers in big endian though. This
-- conversion is dealt with in the Binary instance for ClosurePtr and
-- then the other pointers are derived from this instance using DerivingVia
module GHC.Debug.Types.Ptr( -- * InfoTables
                            InfoTablePtr(..)
                          , readInfoTablePtr
                          , RawInfoTable(..)
                          -- UntaggedClosurePtr constructor not exported so
                          -- we can maintain the invariant that all
                          -- ClosurePtr are untagged
                          -- * Closures
                          , ClosurePtr(..,ClosurePtr)
                          , mkClosurePtr
                          , readClosurePtr
                          , RawClosure(..)
                          , rawClosureSize
                          , getInfoTblPtr
                          -- * Operations on 'ClosurePtr'
                          , applyBlockMask
                          , applyMBlockMask
                          , subtractBlockPtr
                          , heapAlloced

                          , getBlockOffset
                          -- * Blocks
                          , BlockPtr(..)
                          , RawBlock(..)
                          , isLargeBlock
                          , isPinnedBlock
                          , rawBlockAddr
                          , extractFromBlock
                          , blockMBlock
                          , rawBlockSize
                          -- * Stacks
                          , StackPtr(..)
                          , RawStack(..)

                          , subtractStackPtr
                          , calculateStackLen
                          , addStackPtr
                          , rawStackSize
                          , printStack
                          -- * Bitmaps
                          , PtrBitmap(..)
                          , traversePtrBitmap
                          -- * Constants
                          , blockMask
                          , mblockMask
                          , mblockMaxSize
                          , blockMaxSize
                          , CCSPtr(..)
                          , CCPtr(..)
                          , RetainerSetPtr(..)

                          -- * Other utility
                          , arrWordsBS
                          , prettyPrint
                          , printBS
                          )  where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Hashable
import Data.Word

import GHC.Debug.Utils

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import System.Endian

import Numeric (showHex, readHex)
import Data.Coerce
import Data.Bits
import GHC.Stack
import Control.Applicative
import qualified Data.Array.Unboxed as A
import Control.Monad
import qualified Data.Foldable as F

prettyPrint :: BS.ByteString -> String
prettyPrint = concatMap (flip showHex "") . BS.unpack

newtype InfoTablePtr = InfoTablePtr Word64
                     deriving (Eq, Ord)
                     deriving newtype (Hashable)
                     deriving (Show, Binary) via ClosurePtr

readInfoTablePtr :: String -> Maybe InfoTablePtr
readInfoTablePtr ('0':'x':s) = case readHex s of
    [(res, "")] -> Just (InfoTablePtr res)
    _ -> Nothing
readInfoTablePtr _ = Nothing

newtype RetainerSetPtr = RetainerSetPtr Word64
                   deriving (Eq, Ord)
                   deriving newtype (Hashable)
                   deriving (Show, Binary) via ClosurePtr

newtype CCSPtr = CCSPtr Word64
                   deriving (Eq, Ord)
                   deriving newtype (Hashable)
                   deriving (Show, Binary) via ClosurePtr

newtype CCPtr = CCPtr Word64
                   deriving (Eq, Ord)
                   deriving newtype (Hashable)
                   deriving (Show, Binary) via ClosurePtr

-- Invariant, ClosurePtrs are *always* untagged, we take some care to
-- untag them when making a ClosurePtr so we don't have to do it on every
-- call to decodeClosure
newtype ClosurePtr = UntaggedClosurePtr Word64
                   deriving (Eq)
                   deriving newtype (Hashable)

pattern ClosurePtr :: Word64 -> ClosurePtr
pattern ClosurePtr p <- UntaggedClosurePtr p

{-# COMPLETE ClosurePtr #-}

mkClosurePtr :: Word64 -> ClosurePtr
mkClosurePtr = untagClosurePtr . UntaggedClosurePtr

readClosurePtr :: String -> Maybe ClosurePtr
readClosurePtr ('0':'x':s) = case readHex s of
                               [(res, "")] -> Just (mkClosurePtr res)
                               _ -> Nothing
readClosurePtr _ = Nothing

instance Binary ClosurePtr where
  put (ClosurePtr p) = putWord64be (toBE64 p)
  get = mkClosurePtr . fromBE64 <$> getWord64be

instance Ord ClosurePtr where
  (ClosurePtr x) `compare` (ClosurePtr y) = x `compare` y

instance Show ClosurePtr where
  show (ClosurePtr 0) = "null"
  show (ClosurePtr p) =  "0x" ++ showHex p ""


newtype StackPtr = StackPtr Word64
                   deriving (Eq, Ord)
                   deriving newtype (Hashable)
                   deriving (Show, Binary) via ClosurePtr

newtype StringPtr = StringPtr Word64
  deriving Show via StackPtr


subtractBlockPtr :: ClosurePtr -> BlockPtr -> Word64
subtractBlockPtr cp bp = subtractStackPtr (coerce cp) (coerce bp)

subtractStackPtr :: StackPtr -> ClosurePtr -> Word64
subtractStackPtr (StackPtr c) (ClosurePtr c2) =
  c - c2

addStackPtr :: StackPtr -> Word64 -> StackPtr
addStackPtr (StackPtr c) o = StackPtr (c + o)

rawClosureSize :: RawClosure -> Int
rawClosureSize (RawClosure s) = BS.length s

calculateStackLen :: Word32 -> Word64 -> ClosurePtr -> StackPtr -> Word64
calculateStackLen siz offset (ClosurePtr p) (StackPtr sp) =
  (p  -- Pointer to start of StgStack closure
    + offset       -- Offset to end of closure
    + (fromIntegral siz * 8) -- Stack_Size (in words)
    )
    - sp -- Minus current Sp

printBS :: HasCallStack => BS.ByteString -> String
-- Not technically all ClosurePtr but good for the show instance
printBS bs = show (runGet_ (many (get @ClosurePtr)) (BSL.fromStrict bs))

printStack :: RawStack -> String
printStack (RawStack s) = printBS s

arrWordsBS :: [Word] -> BSL.ByteString
arrWordsBS = runPut . mapM_ putWordhost

-- | Check if the ClosurePtr is block allocated or not
-- TODO: MP: These numbers are hard-coded from what
-- mblock_address_space.begin and mblock_address_space.end were when
-- I inspected them in gdb. I don't know if they are always the same of
-- should be queried from the debuggee
heapAlloced :: ClosurePtr -> Bool
heapAlloced (ClosurePtr w) = (w >= 0x4200000000 && w <= 0x14200000000)

newtype RawInfoTable = RawInfoTable BS.ByteString
                     deriving (Eq, Ord, Show)
                     deriving newtype (Binary)

newtype RawClosure = RawClosure BS.ByteString
                   deriving (Eq, Ord, Show)

getRawClosure :: Get RawClosure
getRawClosure = do
  len <- getWord32be
  RawClosure <$!> getByteString (fromIntegral len)

putRawClosure :: RawClosure -> Put
putRawClosure (RawClosure rc) = do
  let n = BS.length rc
  putWord32be (fromIntegral n)
  putByteString rc

instance Binary RawClosure where
  get = getRawClosure
  put = putRawClosure

newtype RawStack = RawStack BS.ByteString
                   deriving (Eq, Ord, Show)

newtype RawPayload = RawPayload BS.ByteString
                   deriving (Eq, Ord, Show)

rawStackSize :: RawStack -> Int
rawStackSize (RawStack bs) = BS.length bs


newtype BlockPtr = BlockPtr Word64
                   deriving (Eq, Ord)
                   deriving newtype (Hashable)
                   deriving (Binary, Show) via StackPtr

blockMBlock :: BlockPtr -> Word64
blockMBlock (BlockPtr p) = p .&. (complement mblockMask)

applyMBlockMask :: ClosurePtr -> BlockPtr
applyMBlockMask (ClosurePtr p) = BlockPtr (p .&. complement mblockMask)

applyBlockMask :: ClosurePtr -> BlockPtr
applyBlockMask (ClosurePtr p) = BlockPtr (p .&. complement blockMask)

getBlockOffset :: ClosurePtr -> Word64
getBlockOffset (ClosurePtr p) = p .&. blockMask

mblockMaxSize, blockMaxSize :: Word64
mblockMaxSize = mblockMask + 1
blockMaxSize = blockMask + 1

mblockMask :: Word64
mblockMask = 0b11111111111111111111 -- 20 bits

blockMask :: Word64
blockMask = 0b111111111111 -- 12 bits

isPinnedBlock :: RawBlock -> Bool
isPinnedBlock (RawBlock _ flags _) = (flags .&. 0b100) /= 0

isLargeBlock :: RawBlock -> Bool
isLargeBlock (RawBlock _ flags _) = (flags .&. 0b10) /= 0

data RawBlock = RawBlock BlockPtr Word16 BS.ByteString
                    deriving (Show)

-- flags, Ptr, size then raw block
getBlock :: Get RawBlock
getBlock = do
  bflags <- getWord16le
  bptr <- get
  len <- getInt32be
  rb <- getByteString (fromIntegral len)
  return (RawBlock bptr bflags rb)

putBlock :: RawBlock -> Put
putBlock (RawBlock bptr bflags rb) = do
  putWord16le bflags
  put bptr
  putInt32be (fromIntegral $ BS.length rb)
  putByteString rb

instance Binary RawBlock where
  get = getBlock
  put = putBlock

rawBlockSize :: RawBlock -> Int
rawBlockSize (RawBlock _ _ bs) = BS.length bs

rawBlockAddr :: RawBlock -> BlockPtr
rawBlockAddr (RawBlock addr _ _) = addr

-- | Invariant: ClosurePtr is within the range of the block
-- The 'RawClosure' this returns is actually the tail of the whole block,
-- this is fine because the memory for each block is only allocated once
-- due to how BS.drop is implemented via pointer arithmetic.
extractFromBlock :: ClosurePtr
                -> RawBlock
                -> RawClosure
extractFromBlock cp (RawBlock bp _ b) =
--  Calling closureSize doesn't work as the info table addresses are bogus
--  clos_size_w <- withForeignPtr fp' (\p -> return $ closureSize (ptrToBox p))
--  let clos_size = clos_size_w * 8
    --traceShow (fp, offset, cp, bp,o, l)
    --traceShow ("FP", fp `plusForeignPtr` offset)
    RawClosure (BS.drop offset b)
    where
      offset = fromIntegral (subtractBlockPtr cp bp)

tAG_MASK :: Word64
tAG_MASK = 0b111

untagClosurePtr :: ClosurePtr -> ClosurePtr
untagClosurePtr (ClosurePtr w) = UntaggedClosurePtr (w .&. complement tAG_MASK)

getInfoTblPtr :: HasCallStack => RawClosure -> InfoTablePtr
getInfoTblPtr (RawClosure bs) = runGet_ (isolate 8 get) (BSL.fromStrict bs)

-- | A bitmap that records whether each field of a stack frame is a pointer.
newtype PtrBitmap = PtrBitmap (A.Array Int Bool) deriving (Show)

traversePtrBitmap :: Monad m => (Bool -> m a) -> PtrBitmap -> m [a]
traversePtrBitmap f (PtrBitmap arr) = mapM f (A.elems arr)

getPtrBitmap :: Get PtrBitmap
getPtrBitmap = do
  len <- getWord32be
  bits <- replicateM (fromIntegral len) getWord8
  let arr = A.listArray (0, fromIntegral len-1) (map (==1) bits)
  return $ PtrBitmap arr

putPtrBitmap :: PtrBitmap -> Put
putPtrBitmap (PtrBitmap pbm) = do
  let n = F.length pbm
  putWord32be (fromIntegral n)
  F.traverse_ (\b -> if b then putWord8 1 else putWord8 0) pbm

instance Binary PtrBitmap where
  get = getPtrBitmap
  put = putPtrBitmap
