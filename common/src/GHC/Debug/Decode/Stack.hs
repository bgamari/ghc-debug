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

getStack :: (InfoTablePtr -> InfoTable) -> (InfoTablePtr -> PtrBitmap) -> Get [(InfoTablePtr, [FieldValue])]
getStack getInfoTable getBitmap = many $ do
    itblPtr <- peek getInfoTablePtr
    let itbl = getInfoTable itblPtr
    case tipe itbl of
      RET_BCO -> do
        -- TODO: In the case of a RET_BCO frame we must decode the frame as a BCO
        error "getStack: RET_BCO"
      _ -> do
        -- In all other cases we request the pointer bitmap from the debuggee
        -- and decode as appropriate.
        _itblPtr <- getInfoTablePtr
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
