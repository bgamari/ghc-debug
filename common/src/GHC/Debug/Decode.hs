{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
#if !MIN_VERSION_ghc_heap(8,7,0)
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module GHC.Debug.Decode (decodeClosure) where

import GHC.Ptr (Ptr(..), plusPtr, castPtr)
import GHC.Exts (Addr#, unsafeCoerce#, Any, Word#)
import GHC.Word
import GHC.IO.Unsafe
import Foreign.Storable

import qualified Data.ByteString.Internal as BSI

import GHC.Exts.Heap
import qualified GHC.Exts.Heap.InfoTable as Itbl
import qualified GHC.Exts.Heap.InfoTableProf as ItblProf

import GHC.Debug.Types
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.ForeignPtr       (withForeignPtr)
import System.Endian

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

boxToClosurePtr :: Box -> ClosurePtr
boxToClosurePtr (Box x) = ClosurePtr (toBE64 (W64# (aToWord# x)))

-- This is a datatype that has the same layout as Ptr, so that by
-- unsafeCoerce'ing, we obtain the Addr of the wrapped value
data Ptr' a = Ptr' a

aToWord# :: Any -> Word#
aToWord# a = case Ptr' a of mb@(Ptr' _) -> case unsafeCoerce# mb :: Word of W# addr -> addr

decodeClosure :: RawInfoTable -> RawClosure -> GenClosure ClosurePtr
decodeClosure (RawInfoTable itbl) (RawClosure clos) = unsafePerformIO $ do
    allocate itbl $ \itblPtr -> do
      allocate clos $ \closPtr -> do
        let ptr_to_itbl_ptr :: Ptr (Ptr StgInfoTable)
            ptr_to_itbl_ptr = castPtr closPtr
        -- The pointer is to the end of the info table (not the start)
        -- Info table is two words long which is why we subtract 16 from
        -- the pointer
        print itblSize
        poke ptr_to_itbl_ptr (fixTNTC itblPtr)
        -- You should be able to print these addresses in gdb
        -- and observe the memory layout is identical to the debugee
        -- process
        print ("Closure", closPtr)
        print ("itbl", itblPtr)
        r <- getBoxedClosureData (ptrToBox closPtr)
        print ("Decoded", r)
        return $ fmap boxToClosurePtr r
  where
    fixTNTC :: Ptr a -> Ptr StgInfoTable
    fixTNTC ptr
      | tablesNextToCode = castPtr $ ptr  `plusPtr` itblSize'
      | otherwise        = castPtr $ ptr

    itblSize'
      | profiling  = ItblProf.itblSize
      | otherwise  = Itbl.itblSize

