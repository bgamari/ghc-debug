{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
#if !MIN_VERSION_ghc_heap(8,7,0)
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module GHC.Debug.Decode (decodeClosure, decodeInfoTable) where

import GHC.Ptr (Ptr(..), plusPtr, castPtr)
import GHC.Exts -- (Addr#, unsafeCoerce#, Any, Word#, ByteArray#)
import GHC.Int
import Data.Coerce
import Data.Bifunctor
import GHC.Word
import GHC.IO.Unsafe
import Foreign.Storable

import qualified Data.ByteString.Internal as BSI

import qualified GHC.Exts.Heap as GHC
import GHC.Exts.Heap hiding (Closure)
import qualified GHC.Exts.Heap.InfoTable as Itbl
import qualified GHC.Exts.Heap.InfoTableProf as ItblProf

import GHC.Debug.Types
import GHC.Debug.Decode.Convert
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.ForeignPtr       (withForeignPtr)
import System.Endian

foreign import prim "unpackClosureWordszh" unpackClosureWords# ::
              Any -> (# Addr#, ByteArray#, ByteArray# #)

getClosureRaw :: Box -> IO (GenClosure Word)
getClosureRaw (Box a) = do
  let (# infoTablePtr, datArr, pointers #) = unpackClosureWords# a
  let nelems_ptrs = (I# (sizeofByteArray# pointers)) `div` 8
      end_ptrs = fromIntegral nelems_ptrs - 1
      rawPtrs = [W# (indexWordArray# pointers i) | I# i <- [0.. end_ptrs] ]
  getClosureDataFromHeapRep True datArr (Ptr infoTablePtr) rawPtrs


-- | Allocate a bytestring directly into memory and return a pointer to the
-- allocated buffer
allocate :: BSI.ByteString -> (Ptr a -> IO a) -> IO a
allocate (BSI.PS fp o l) action =
 allocaBytes l $ \buf ->
   withForeignPtr fp $ \p -> do
     BSI.memcpy buf (p `plusPtr` o) (fromIntegral l)
     action (castPtr buf)

data NotABox = NotABox Addr#

#if !MIN_VERSION_ghc_heap(8,7,0)
deriving instance Functor GenClosure
#endif

ptrToBox :: Ptr a -> Box
ptrToBox (Ptr p) = unsafeCoerce# (NotABox p)

boxToRawAddress :: Box -> Word64
boxToRawAddress (Box x) = (toBE64 (W64# (aToWord# x)))

-- This is a datatype that has the same layout as Ptr, so that by
-- unsafeCoerce'ing, we obtain the Addr of the wrapped value
data Ptr' a = Ptr' a

aToWord# :: Any -> Word#
aToWord# a = case Ptr' a of mb@(Ptr' _) -> case unsafeCoerce# mb :: Word of W# addr -> addr

decodeClosure :: RawInfoTable -> (ClosurePtr, RawClosure) -> IO Closure
decodeClosure (RawInfoTable itbl) (ptr, rc@(RawClosure clos)) = do
    allocate itbl $ \itblPtr -> do
      allocate clos $ \closPtr -> do
        let ptr_to_itbl_ptr :: Ptr (Ptr StgInfoTable)
            ptr_to_itbl_ptr = castPtr closPtr
        -- The pointer is to the end of the info table (not the start)
        -- Info table is two words long which is why we subtract 16 from
        -- the pointer
        poke ptr_to_itbl_ptr (fixTNTC itblPtr)
        -- You should be able to print these addresses in gdb
        -- and observe the memory layout is identical to the debugee
        -- process
        -- Printing this return value can lead to segfaults because the
        -- pointer for constrDesc won't point to a string after being
        -- decoded.
        r <- getClosureRaw (ptrToBox closPtr)
        -- print ("DECODED", r)
        return $ trimap (const ptr) stackCont  ClosurePtr . convertClosure
          $ fmap (\(W# w) -> toBE64 (W64# w)) r
  where
    stackCont :: Word64 -> StackCont
    stackCont sp =  StackCont (StackPtr sp)
      --(getRawStack (StackPtr sp) ptr rc)


fixTNTC :: Ptr a -> Ptr StgInfoTable
fixTNTC ptr
  | tablesNextToCode = castPtr $ ptr  `plusPtr` itblSize'
  | otherwise        = castPtr $ ptr
  where
    itblSize'
      | profiling  = ItblProf.itblSize
      | otherwise  = Itbl.itblSize

decodeInfoTable :: RawInfoTable -> StgInfoTable
decodeInfoTable (RawInfoTable itbl) = unsafePerformIO $ do
  allocate itbl $ \itblPtr -> do
    peekItbl itblPtr
