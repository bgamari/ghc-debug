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
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.InfoTable.Types
import System.Endian


decodeStack :: RawStack
            -> StgInfoTable
            -> PtrBitmap
            -> Stack
decodeStack (RawStack closure) itbl bitmap =
  B.runGet (getStack bitmap itbl) (BSL.fromStrict closure)

getStack :: PtrBitmap
         -> StgInfoTable
         -> Get Stack
getStack bitmap itbl = do
    case tipe itbl of
      RET_BCO -> do
        -- TODO: In the case of a RET_BCO frame we must decode the frame as a BCO
        error "getStack: RET_BCO"
      _ -> do
        -- In all other cases we request the pointer bitmap from the debuggee
        -- and decode as appropriate.
        _itblPtr <- getInfoTablePtr
        fields <- traversePtrBitmap decodeField bitmap
        return (DebugStackFrame itbl fields)
  where
    decodeField True  = SPtr . ClosurePtr . toBE64 <$> getWord
    decodeField False = SNonPtr <$> getWord

getInfoTablePtr :: Get InfoTablePtr
getInfoTablePtr = InfoTablePtr <$> getWord64le -- TODO word size

getWord :: Get Word64
getWord = getWord64le -- TODO word size
