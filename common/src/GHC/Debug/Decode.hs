{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
#if !MIN_VERSION_ghc_heap(8,7,0)
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module GHC.Debug.Decode (decodeClosure) where

import GHC.Ptr (Ptr(..), plusPtr, castPtr, minusPtr)
import GHC.Exts (Addr#, unsafeCoerce#)
import GHC.IO.Unsafe
import Foreign.Storable
import Foreign.C.Types (CChar)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BSI

import GHC.Exts.Heap
import GHC.Exts.Heap.InfoTable.Types
import qualified GHC.Exts.Heap.InfoTable as Itbl
import qualified GHC.Exts.Heap.InfoTableProf as ItblProf

import GHC.Debug.Types
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.Marshal.Array    (allocaArray)
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr, touchForeignPtr)

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
boxToClosurePtr (Box x) = ClosurePtr (unsafeCoerce# x)

decodeClosure :: RawInfoTable -> RawClosure -> GenClosure ClosurePtr
decodeClosure (RawInfoTable itbl) (RawClosure clos) = unsafePerformIO $ do
    allocate itbl $ \itblPtr -> do
      allocate clos $ \closPtr -> do
        let ptr_to_itbl_ptr :: Ptr (Ptr StgInfoTable)
            ptr_to_itbl_ptr = castPtr closPtr
        -- The pointer is to the end of the info table (not the start)
        -- Info table is two words long which is why we subtract 16 from
        -- the pointer
        poke ptr_to_itbl_ptr (castPtr (itblPtr `plusPtr` 16))
        -- You should be able to print these addresses in gdb
        -- and observe the memory layout is identical to the debugee
        -- process
        print ("Closure", closPtr)
        print ("itbl", itblPtr)
        fmap boxToClosurePtr <$> getBoxedClosureData (ptrToBox closPtr)
  where
    fixTNTC :: Ptr a -> Ptr StgInfoTable
    fixTNTC ptr
      | tablesNextToCode = castPtr $ ptr  `plusPtr` itblSize
      | otherwise        = castPtr $ ptr

    itblSize
      | profiling  = ItblProf.itblSize
      | otherwise  = Itbl.itblSize

