module GHC.Debug.Decode.Stack
  ( FieldValue(..)
  , decodeStack
  ) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Applicative

import Data.Binary.Get as B

import GHC.Debug.Types

data FieldValue = Ptr !ClosurePtr
                | NonPtr !Word64

decodeStack :: (InfoTablePtr -> PtrBitmap) -> BS.ByteString -> [(InfoTablePtr, [FieldValue])]
decodeStack getBitmap closure = B.runGet (getStack getBitmap) (BSL.fromStrict closure)

getStack :: (InfoTablePtr -> PtrBitmap) -> Get [(InfoTablePtr, [FieldValue])]
getStack getBitmap = many $ do
    itblPtr <- getInfoTablePtr
    let bitmap = getBitmap itblPtr
    fields <- traversePtrBitmap decodeField bitmap
    return (itblPtr, fields)
  where
    decodeField True  = Ptr . ClosurePtr <$> getWord
    decodeField False = NonPtr <$> getWord

getInfoTablePtr :: Get InfoTablePtr
getInfoTablePtr = InfoTablePtr <$> getWord64le -- TODO word size

getWord :: Get Word64
getWord = getWord64le -- TODO word size
