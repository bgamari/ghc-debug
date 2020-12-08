module GHC.Debug.Decode.Stack
  ( decodeStack
  ) where

import Data.Word
import qualified Data.ByteString as BS

import Data.Binary.Get as B

import GHC.Debug.Types
import GHC.Exts.Heap.ClosureTypes
import System.Endian

import Data.Coerce

decodeStack :: Monad m
            => (RawClosure -> m StgInfoTableWithPtr)
            -> (RawClosure -> m PtrBitmap)
            -> RawStack
            -> m StackFrames
decodeStack decodeInfoTable getBitmap rs = do
  GenStackFrames <$> get_frames rs
  where
    get_frames raw@(RawStack c) = do
      st_it <- decodeInfoTable (coerce rs)
      bm <- getBitmap (coerce raw)
      let res = B.runGetIncremental (getFrame bm st_it) `pushChunk` c
      case res of
        Fail _rem _offset err -> error err
        Partial _inp -> error "Not enough input"
        Done more _offset v
          | BS.null more -> return []
          | otherwise -> (v:) <$> get_frames (RawStack  more)

getFrame :: PtrBitmap
         -> StgInfoTableWithPtr
         -> Get (DebugStackFrame ClosurePtr)
getFrame st_bitmap itbl =
    case tipe (decodedTable itbl) of
      RET_BCO ->
        -- TODO: In the case of a RET_BCO frame we must decode the frame as a BCO
        error "getStack: RET_BCO"
      _ -> do
        -- In all other cases we request the pointer bitmap from the debuggee
        -- and decode as appropriate.
        _itblPtr <- getInfoTablePtr
        fields <- traversePtrBitmap decodeField st_bitmap
        return (DebugStackFrame itbl fields)
  where
    decodeField True  = SPtr . ClosurePtr . toBE64 <$> getWord
    decodeField False = SNonPtr <$> getWord

getInfoTablePtr :: Get InfoTablePtr
getInfoTablePtr = InfoTablePtr <$> getWord64le -- TODO word size

getWord :: Get Word64
getWord = getWord64le -- TODO word size
