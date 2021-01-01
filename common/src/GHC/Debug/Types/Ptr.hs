{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}

-- | Data types for representing different pointers and raw information
-- All pointers are stored in little-endian to make arithmetic easier.
--
-- We have to send and recieve the pointers in big endiant though. This
-- conversion is dealt with in the Binary instance for ClosurePtr and
-- then the other pointers are derived from this instance using DerivingVia
module GHC.Debug.Types.Ptr where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Hashable
import Data.Word

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import System.Endian

import Numeric (showHex)
import Data.Coerce
import Data.Bits
import GHC.Stack
import Control.Applicative

prettyPrint :: BS.ByteString -> String
prettyPrint = concatMap (flip showHex "") . BS.unpack

-- TODO: Fetch this from debuggee
tablesNextToCode :: Bool
tablesNextToCode = True

-- TODO: Fetch this from debuggee
profiling :: Bool
profiling = False

newtype InfoTablePtr = InfoTablePtr Word64
                     deriving (Eq, Ord)
                     deriving newtype (Hashable)
                     deriving (Show, Binary) via ClosurePtr

newtype ClosurePtr = ClosurePtr Word64
                   deriving (Eq)
                   deriving newtype (Hashable)

instance Binary ClosurePtr where
  put (ClosurePtr p) = putWord64be (toBE64 p)
  get = ClosurePtr . fromBE64 <$> getWord64be

instance Ord ClosurePtr where
  (ClosurePtr x) `compare` (ClosurePtr y) = x `compare` y

instance Show ClosurePtr where
  show (ClosurePtr 0) = "null"
  show (ClosurePtr p) =  "0x" ++ showHex p ""

data StackCont = StackCont StackPtr -- Address of start of frames
                           RawStack -- The raw frames
                           deriving Show

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

calculateStackLen :: Word32 -> ClosurePtr -> StackPtr -> Word64
calculateStackLen siz (ClosurePtr p) (StackPtr sp) =
  (p  -- Pointer to start of StgStack closure
    + 24       -- Offset to end of closure
    + (fromIntegral siz * 8) -- Stack_Size (in words)
    )
    - sp -- Minus current Sp

getRawStack :: (Word32, StackPtr) -> ClosurePtr -> RawClosure -> RawStack
getRawStack (siz, sp) c (RawClosure s) =
  let -- Offset from start of RawClosure to stack frames
      k = fromIntegral (subtractStackPtr sp c)
      -- The size of the stack frames
      len = calculateStackLen siz c sp
      raw_s = (BS.take (fromIntegral len) (BS.drop k s))
  in RawStack raw_s

printBS :: BS.ByteString -> String
-- Not technically all ClosurePtr but good for the show instance
printBS bs = show (runGet (many (get @ClosurePtr)) (BSL.fromStrict bs))

printStack :: RawStack -> String
printStack (RawStack s) = printBS s

-- | Check if the ClosurePtr is block allocated or not
-- TODO: MP: These numbers are hard-coded from what
-- mblock_address_space.begin and mblock_address_space.end were when
-- I inspected them in gdb. I don't know if they are always the same of
-- should be queried from the debuggee
ptrInBlock :: ClosurePtr -> Bool
ptrInBlock (ClosurePtr w) = (w >= 0x4200000000 && w <= 0x14200000000)

newtype RawInfoTable = RawInfoTable BS.ByteString
                     deriving (Eq, Ord, Show)
                     deriving newtype (Binary)

newtype RawClosure = RawClosure BS.ByteString
                   deriving (Eq, Ord, Show)
                   deriving newtype (Binary)

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

data RawBlock = RawBlock BlockPtr Word16 BS.ByteString
                    deriving (Show)

rawBlockSize :: RawBlock -> Int
rawBlockSize (RawBlock _ _ bs) = BS.length bs

rawBlockAddr :: RawBlock -> BlockPtr
rawBlockAddr (RawBlock addr _ _) = addr

tAG_MASK :: Word64
tAG_MASK = 0b111

untagClosurePtr :: ClosurePtr -> ClosurePtr
untagClosurePtr (ClosurePtr w) = ClosurePtr (w .&. complement tAG_MASK)

getInfoTblPtr :: HasCallStack => RawClosure -> InfoTablePtr
getInfoTblPtr (RawClosure bs) = runGet (isolate 8 get) (BSL.fromStrict bs)

-- A value, but with a different value used for testing equality.
data PayloadWithKey k a = PayloadWithKey k a deriving Show


instance Eq k => Eq (PayloadWithKey k a) where
  (PayloadWithKey k1 _) == (PayloadWithKey k2 _) = k1 == k2

instance Hashable k => Hashable (PayloadWithKey k a) where
  hashWithSalt s (PayloadWithKey k _) = hashWithSalt s k
