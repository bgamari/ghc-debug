{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
#if !MIN_VERSION_ghc_heap(8,7,0)
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module GHC.Debug.Decode (decodeClosure) where

import GHC.Ptr (Ptr(..), plusPtr, castPtr)
import GHC.Exts (Addr#, unsafeCoerce#)
import GHC.IO.Unsafe
import Foreign.Storable
import Foreign.C.Types (CChar)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import GHC.Exts.Heap
import GHC.Exts.Heap.InfoTable.Types
import qualified GHC.Exts.Heap.InfoTable as Itbl
import qualified GHC.Exts.Heap.InfoTableProf as ItblProf

import GHC.Debug.Types

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
    BS.unsafeUseAsCString itbl $ \itblPtr -> do
      BS.unsafeUseAsCString clos $ \closPtr -> do
        let ptr_to_itbl_ptr :: Ptr (Ptr StgInfoTable)
            ptr_to_itbl_ptr = castPtr closPtr
        poke ptr_to_itbl_ptr (fixTNTC itblPtr)
        fmap boxToClosurePtr <$> getBoxedClosureData (ptrToBox closPtr)
  where
    fixTNTC :: Ptr a -> Ptr StgInfoTable
    fixTNTC ptr
      | tablesNextToCode = castPtr $ ptr `plusPtr` itblSize
      | otherwise        = castPtr $ ptr

    itblSize
      | profiling  = ItblProf.itblSize
      | otherwise  = Itbl.itblSize

