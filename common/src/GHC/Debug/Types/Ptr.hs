{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BinaryLiterals #-}

module GHC.Debug.Types.Ptr where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Hashable
import Data.Word

import Data.Binary
import Data.Binary.Get
import System.Endian

import Numeric (showHex)
import Data.Coerce
import Data.Bits

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
                     deriving newtype (Binary, Hashable)

newtype ShowPtr = ShowPtr Word64

instance Show ShowPtr where
  show (ShowPtr w) = "0x" ++ showHex w ""

instance Show InfoTablePtr where
  show (InfoTablePtr p) =  "0x" ++ showHex (fromBE64 p) ""

newtype ClosurePtr = ClosurePtr Word64
                   deriving (Eq)
                   deriving newtype (Binary, Hashable)

instance Ord ClosurePtr where
  (ClosurePtr x) `compare` (ClosurePtr y) = fromBE64 x `compare` fromBE64 y

instance Show ClosurePtr where
  show (ClosurePtr p) =  "0x" ++ showHex (fromBE64 p) ""

data StackCont = StackCont StackPtr deriving Show

newtype StackPtr = StackPtr Word64
                   deriving (Eq, Ord)
                   deriving newtype (Binary, Hashable)

instance Show StackPtr where
  show (StackPtr p) =  "0x" ++ showHex (fromBE64 p) ""

newtype StringPtr = StringPtr Word64
  deriving Show via StackPtr


subtractBlockPtr :: ClosurePtr -> BlockPtr -> Word64
subtractBlockPtr cp bp = subtractStackPtr (coerce cp) (coerce bp)

subtractStackPtr :: StackPtr -> ClosurePtr -> Word64
subtractStackPtr (StackPtr c) (ClosurePtr c2) =
  (fromBE64 c) - (fromBE64 c2)

addStackPtr :: StackPtr -> Word64 -> StackPtr
addStackPtr (StackPtr c) o = StackPtr (toBE64 (fromBE64 c + o))

rawClosureSize :: RawClosure -> Int
rawClosureSize (RawClosure s) = BS.length s

{-
getRawStack :: StackPtr -> ClosurePtr -> RawClosure -> RawStack
getRawStack sp c (RawClosure s) =
  let k = fromIntegral (subtractStackPtr sp c)
  in RawStack (BS.drop k s)/
  -}

-- | Check if the ClosurePtr is block allocated or not
-- TODO: MP: These numbers are hard-coded from what
-- mblock_address_space.begin and mblock_address_space.end were when
-- I inspected them in gdb. I don't know if they are always the same of
-- should be queried from the debuggee
ptrInBlock :: ClosurePtr -> Bool
ptrInBlock (ClosurePtr (fromBE64 -> w)) = (w >= 0x4200000000 && w <= 0x14200000000)

newtype RawInfoTable = RawInfoTable BS.ByteString
                     deriving (Eq, Ord, Show)
                     deriving newtype (Binary)

newtype RawClosure = RawClosure BS.ByteString
                   deriving (Eq, Ord, Show)
                   deriving newtype (Binary)

newtype RawStack = RawStack BS.ByteString
                   deriving (Eq, Ord, Show)


newtype BlockPtr = BlockPtr Word64
                   deriving (Eq, Ord)
                   deriving newtype (Binary, Hashable)
                   deriving Show via StackPtr

data RawBlock = RawBlock BlockPtr Word16 BS.ByteString
                    deriving (Show)

rawBlockSize :: RawBlock -> Int
rawBlockSize (RawBlock _ _ bs) = BS.length bs

rawBlockAddr :: RawBlock -> BlockPtr
rawBlockAddr (RawBlock addr _ _) = addr

tAG_MASK :: Word64
tAG_MASK = 0b111

untagClosurePtr :: ClosurePtr -> ClosurePtr
untagClosurePtr (ClosurePtr (fromBE64 -> w)) = ClosurePtr (toBE64 (w .&. complement tAG_MASK))

getInfoTblPtr :: RawClosure -> InfoTablePtr
getInfoTblPtr (RawClosure bs) = InfoTablePtr (runGet getWord64be (BSL.take 8 (BSL.fromStrict bs)))

-- A value, but with a different value used for testing equality.
data PayloadWithKey k a = PayloadWithKey k a deriving Show


instance Eq k => Eq (PayloadWithKey k a) where
  (PayloadWithKey k1 _) == (PayloadWithKey k2 _) = k1 == k2

instance Hashable k => Hashable (PayloadWithKey k a) where
  hashWithSalt s (PayloadWithKey k _) = hashWithSalt s k
