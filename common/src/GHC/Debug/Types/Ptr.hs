{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Debug.Types.Ptr where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Hashable
import Data.Word

import Data.Binary
import Data.Binary.Get
import System.Endian
import Debug.Trace

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
                   deriving (Eq, Ord)
                   deriving newtype (Binary, Hashable)

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

rawClosureSize :: RawClosure -> Int
rawClosureSize (RawClosure s) = BS.length s

{-
getRawStack :: StackPtr -> ClosurePtr -> RawClosure -> RawStack
getRawStack sp c (RawClosure s) =
  let k = fromIntegral (subtractStackPtr sp c)
  in RawStack (BS.drop k s)/
  -}

-- | Check if the ClosurePtr is block allocated or not
ptrInBlock :: ClosurePtr -> Bool
ptrInBlock (ClosurePtr (fromBE64 -> w)) = (w >= 283467841536 && w <= 1382979469312)

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

data RawBlock = RawBlock BlockPtr BS.ByteString
                    deriving (Show)

rawBlockSize :: RawBlock -> Int
rawBlockSize (RawBlock _ bs) = BS.length bs

tAG_MASK = 1

untagClosurePtr :: ClosurePtr -> ClosurePtr
untagClosurePtr (ClosurePtr (fromBE64 -> w)) = ClosurePtr (toBE64 (w .&. complement tAG_MASK))

getInfoTblPtr :: RawClosure -> InfoTablePtr
getInfoTblPtr (RawClosure bs) = InfoTablePtr (runGet getWord64be (BSL.take 8 (BSL.fromStrict bs)))

