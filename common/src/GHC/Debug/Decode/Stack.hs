module GHC.Debug.Decode.Stack
  ( decodeStack
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

import Data.Coerce

import GHC.Debug.Decode

decodeStack :: Monad m
            => (RawClosure -> m (RawInfoTable, RawClosure))
            -> (RawClosure -> m PtrBitmap)
            -> RawStack
            -> m Stack
decodeStack getInfoTable getBitmap rs = do
  frames <- get_frames rs
  return (Stack 0 0 0 frames)
  where
    get_frames rs@(RawStack c) = do
      (itbl, _) <- getInfoTable (coerce rs)
      bm <- getBitmap (coerce rs)
      let st_it = StgInfoTableWithPtr (getInfoTblPtr (coerce rs)) (decodeInfoTable itbl)
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
getFrame bitmap itbl =
    case tipe (decodedTable itbl) of
      RET_BCO ->
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
