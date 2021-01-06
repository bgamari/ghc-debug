module GHC.Debug.Decode.Stack
  ( decodeStack
  ) where

import Data.Word
import qualified Data.ByteString as BS

import Data.Binary.Get as B

import GHC.Debug.Types
import GHC.Exts.Heap.ClosureTypes
import Control.Monad

import Data.Coerce
import Debug.Trace

decodeStack :: Monad m
            => (RawClosure -> m StgInfoTableWithPtr)
            -> (Word32 -> m PtrBitmap)
            -> RawStack
            -> m StackFrames
decodeStack decodeInfoTable getBitmap rs = do
  GenStackFrames <$> get_frames 0 rs
  where
    get_frames sp raw@(RawStack c) = do
      st_it <- decodeInfoTable (coerce raw)
      bm <- getBitmap sp
      let res = B.runGetIncremental (getFrame bm st_it) `pushChunk` c
      case res of
        Fail _rem _offset err -> error err
        Partial _inp -> error "Not enough input"
        Done more offset v
          | BS.null more -> return []
          | otherwise -> (v:) <$> get_frames (sp + (fromIntegral offset)) (RawStack more)

getFrame :: PtrBitmap
         -> StgInfoTableWithPtr
         -> Get (DebugStackFrame ClosurePtr)
getFrame st_bitmap itbl =
    case tipe (decodedTable itbl) of
      RET_BCO ->
        -- TODO: In the case of a RET_BCO frame we must decode the frame as a BCO
        error "getStack: RET_BCO"
      ty -> do
        -- In all other cases we request the pointer bitmap from the debuggee
        -- and decode as appropriate.
        --traceShowM (headerSize ty, ty, st_bitmap, itbl)
        _itblPtr <- replicateM (headerSize ty) getWord64le
        fields <- traversePtrBitmap decodeField st_bitmap
        return (DebugStackFrame itbl fields)
  where
    decodeField True  = SPtr . ClosurePtr <$> getWord
    decodeField False = SNonPtr <$> getWord

    headerSize RET_FUN = 3
    headerSize RET_BCO = 2
    headerSize _ = 1

getInfoTablePtr :: Get InfoTablePtr
getInfoTablePtr = InfoTablePtr <$> getWord64le -- TODO word size

getWord :: Get Word64
getWord = getWord64le -- TODO word size
