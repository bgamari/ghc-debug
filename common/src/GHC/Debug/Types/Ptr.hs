{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module GHC.Debug.Types.Ptr where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Hashable
import Data.Word
import System.IO
import System.IO.Unsafe

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import System.Endian

import Debug.Trace

import Numeric (showHex)


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

data StackCont = StackCont RawStack deriving Show

newtype StackPtr = StackPtr Word64

instance Show StackPtr where
  show (StackPtr p) =  "0x" ++ showHex (fromBE64 p) ""



subtractStackPtr :: StackPtr -> ClosurePtr -> Word64
subtractStackPtr (StackPtr c) (ClosurePtr c2) =
  (fromBE64 c) - (fromBE64 c2)

rawClosureSize :: RawClosure -> Int
rawClosureSize (RawClosure s) = BS.length s

getRawStack :: StackPtr -> ClosurePtr -> RawClosure -> RawStack
getRawStack sp c (RawClosure s) =
  let k = fromIntegral (subtractStackPtr sp c)
  in RawStack (BS.drop k s)


newtype RawInfoTable = RawInfoTable BS.ByteString
                     deriving (Eq, Ord, Show)
                     deriving newtype (Binary)

newtype RawClosure = RawClosure BS.ByteString
                   deriving (Eq, Ord, Show)
                   deriving newtype (Binary)

newtype RawStack = RawStack BS.ByteString
                   deriving (Eq, Ord, Show)


getInfoTblPtr :: RawClosure -> InfoTablePtr
getInfoTblPtr (RawClosure bs) = InfoTablePtr (runGet getWord64be (BSL.take 8 (BSL.fromStrict bs)))


