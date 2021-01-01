{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE BangPatterns #-}
#if !MIN_VERSION_ghc_heap(8,7,0)
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

module GHC.Debug.Decode (decodeClosure, decodeClosureWithSize, decodeInfoTable, extractFromBlock) where

import GHC.Ptr (plusPtr, castPtr)
import GHC.Exts hiding (closureSize#) -- (Addr#, unsafeCoerce#, Any, Word#, ByteArray#)
import GHC.Word
import GHC.IO.Unsafe
import Foreign.Storable

import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Short.Internal (ShortByteString(..), toShort)
import qualified Data.ByteString.Lazy as BSL

import GHC.Exts.Heap hiding (Closure, closureSize#)
import qualified GHC.Exts.Heap.InfoTable as Itbl
import qualified GHC.Exts.Heap.InfoTableProf as ItblProf

import GHC.Debug.Types.Ptr
import GHC.Debug.Types.Closures
import GHC.Debug.Decode.Convert
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.ForeignPtr       (withForeignPtr)
import GHC.ForeignPtr
import System.Endian
import Debug.Trace
import Data.Binary.Get as B
import Control.Monad
import Data.Void

import qualified Data.ByteString as B

foreign import prim "unpackClosureInfozh" unpackClosureInfo# ::
              Addr# -> (# Addr# #)

foreign import prim "unpackClosureDatzh" unpackClosureDat# ::
              Addr# -> (# ByteArray# #)

foreign import prim "unpackClosurePtrzh" unpackClosurePtr# ::
              Addr# -> (# ByteArray# #)

foreign import prim "closureSizezh" closureSize# ::
              Addr# -> (# Word# #)

unpackClosureWords# c =
  let
    !(# itbl #) = unpackClosureInfo# c
    !(# dat #)  = unpackClosureDat# c
    !(# ptrs #) = unpackClosurePtr# c
  in (# itbl, dat, ptrs #)


data AllocStrategy = AllocByPtr | AllocByCopy

allocStrategy :: AllocStrategy
allocStrategy = AllocByCopy

getClosureRaw :: StgInfoTable -> Ptr a -> BSI.ByteString -> IO (GenClosure Word, Size)
getClosureRaw itb (Ptr closurePtr) datString = do
  let !(# pointers #) = unpackClosurePtr# closurePtr
      !(# raw_size_wh #) = closureSize# closurePtr
      raw_size = fromIntegral (W# raw_size_wh) * 8
  -- Not strictly necessary to take the size of the raw string but its
  -- a good sanity check. In particular it helps with stack decoding.
  let !(SBS datArr) = (toShort (B.take raw_size datString))
  let nelems_ptrs = (I# (sizeofByteArray# pointers)) `div` 8
      end_ptrs = fromIntegral nelems_ptrs - 1
      rawPtrs = [W# (indexWordArray# pointers i) | I# i <- [0.. end_ptrs] ]
  gen_closure <- getClosureDataFromHeapRepPrim (return ("", "", ""))
                                               (\_ -> return Nothing) itb datArr  rawPtrs
  return (gen_closure, Size raw_size)

-- | Allow access directly to the chunk of memory used by a bytestring
allocate :: BSI.ByteString -> (Ptr a -> IO a) -> IO a
allocate = case allocStrategy of
            AllocByPtr  -> allocateByPtr
            AllocByCopy -> allocateByCopy

-- MP: It was thought that allocateByPtr would be quite a bit faster but
-- this turns out to not be true on some simple benchmarks. In future we
-- might try to remove all copying from the pipeline.


-- | Allocate a bytestring directly into memory and return a pointer to the
-- allocated buffer
allocateByCopy :: BSI.ByteString -> (Ptr a -> IO a) -> IO a
allocateByCopy (BSI.PS fp o l) action =
 allocaBytes l $ \buf ->
   withForeignPtr fp $ \p -> do
     --print (fp, o, l)
     BSI.memcpy buf (p `plusPtr` o) (fromIntegral l)
     action (castPtr buf)

-- allocate' will not work to allocate a closure pointer unless the offset
-- is a multiple of word size, so the closure is word-aligned.
-- This is currently ensured by using `BS.copy` when the RawClosure is
-- constructed.
allocateByPtr :: BSI.ByteString -> (Ptr a -> IO a) -> IO a
-- DEBUG: Check for alignment
--allocate' (BSI.PS fp o l) _ | o `mod` 8 /= 0 = error (show ("NOT ALIGNED", fp, o, l))
allocateByPtr (BSI.PS fp o _l) action =
  withForeignPtr (fp `plusForeignPtr` o) $ \p -> do
    --print (fp, p, o, l)
    action (castPtr p)

#if !MIN_VERSION_ghc_heap(8,7,0)
deriving instance Functor GenClosure
#endif

decodeClosureWithSize :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeClosureWithSize itb (ptr, rc) = decodeClosure itb (ptr, rc)

decodePAPClosure :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodePAPClosure (info, _) (cp, RawClosure rc) = flip runGet (BSL.fromStrict rc) $ do
  _itbl <- getWord64le
  arity <- getWord32le
  nargs <- getWord32le
  fun_ptr <- getWord64le
  payload <- replicateM (fromIntegral nargs) getWord64le
  let funp = (ClosurePtr (toBE64 fun_ptr))
      cont = PayloadCont funp payload
  return $ DCS (Size ((3 + fromIntegral nargs) * 8)) (GHC.Debug.Types.Closures.PAPClosure info arity nargs funp cont)

decodeAPClosure :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeAPClosure (info, _) (cp, RawClosure rc) = flip runGet (BSL.fromStrict rc) $ do
  _itbl <- getWord64le
  arity <- getWord32le
  nargs <- getWord32le
  fun_ptr <- getWord64le
  payload <- replicateM (fromIntegral nargs) getWord64le
  let funp = (ClosurePtr (toBE64 fun_ptr))
      cont = PayloadCont funp payload
  return $ DCS (Size ((3 + fromIntegral nargs) * 8)) (GHC.Debug.Types.Closures.APClosure info arity nargs funp cont)


decodeTVarClosure :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeTVarClosure (info, _) (cp, RawClosure rc) = flip runGet (BSL.fromStrict rc) $ do
  _itbl <- getWord64le
  ptr <- getClosurePtr
  watch_queue <- getClosurePtr
  updates <- getInt64le
  return $ DCS 4 (TVarClosure info ptr watch_queue (fromIntegral updates))

getClosurePtr :: Get ClosurePtr
getClosurePtr = ClosurePtr . toBE64 <$> getWord64le

getWord :: Get Word64
getWord = getWord64le

decodeMutPrim :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeMutPrim (info, _) (cp, RawClosure rc) = flip runGet (BSL.fromStrict rc) $ do
  _itbl <- getWord64le
  let kptrs = fromIntegral (ptrs (decodedTable info))
      kdat = fromIntegral (nptrs (decodedTable info))
  pts <- replicateM kptrs getClosurePtr
  dat <- replicateM kdat (fromIntegral <$> getWord64le)
  return $ DCS (Size (1 + kptrs + kdat)) (MutPrimClosure info pts dat)

decodeTrecChunk :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeTrecChunk (info, _) (cp, RawClosure rc) = flip runGet (BSL.fromStrict rc) $ do
  _itbl <- getWord64le
  prev <- getClosurePtr
  next_idx <- getWord64le
  chunks <- replicateM (fromIntegral next_idx) getChunk
  return $ DCS (3 + (16 * 4)) (TRecChunkClosure info prev (fromIntegral next_idx) chunks)

  where
    getChunk = do
      TRecEntry <$> getClosurePtr
                <*> getClosurePtr
                <*> getClosurePtr
                <*> (fromIntegral <$> getInt64le) -- TODO: num_updates field is wrong
                                                  -- Not sure how it should
                                                  -- be decoded

decodeBlockingQueue :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeBlockingQueue (info, _) (_, RawClosure rc) = flip runGet (BSL.fromStrict rc) $ do
  _itbl <- getWord
  q <- getClosurePtr
  bh <- getClosurePtr
  tso <- getClosurePtr
  bh_q <- getClosurePtr
  return $ DCS 5 (GHC.Debug.Types.Closures.BlockingQueueClosure info q bh tso bh_q)

-- It is just far simpler to directly decode the stack here rather than use
-- the existing logic in ghc-heap......
decodeStack :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeStack (info, _) (cp, RawClosure rc) =
  let res = flip runGetOrFail (BSL.fromStrict rc) $ do
              _itbl <- getWord
              st_size <- getWord32le
              st_dirty <- getWord8
              st_marking <- getWord8
              -- Up to now, 14 bytes are read, skip 2 to get to 16/start of
              -- sp field
              skip 2
              st_sp <- StackPtr . toBE64 <$> getWord
              let k = fromIntegral (subtractStackPtr st_sp cp)
                        -- -24 for the bytes already read
                        - 24
                  len = calculateStackLen st_size cp st_sp
              -- Skip to start of stack frames
              skip k
              -- Read the raw frames, we can't decode them yet because we
              -- need to query the debuggee for the bitmaps
              raw_stack <- RawStack <$> getByteString (fromIntegral len)
              return (GHC.Debug.Types.Closures.StackClosure info st_size st_dirty st_marking (StackCont st_sp raw_stack))
  in case res of
       Left err -> error (show err)
       Right (_rem, o, v) ->
        -- Offset is is in bytes, size is in words
        (DCS (Size (fromIntegral (o `div` 8))) v)



decodeAPStack :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeAPStack = undefined




decodeClosure :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeClosure i@(itb, _) c
  | (StgInfoTable { tipe = PAP }) <- decodedTable itb = decodePAPClosure i c
  | (StgInfoTable { tipe = AP }) <- decodedTable itb = decodeAPClosure i c
  | (StgInfoTable { tipe = TVAR }) <- decodedTable itb = decodeTVarClosure i c
  | (StgInfoTable { tipe = MUT_PRIM }) <- decodedTable itb = decodeMutPrim i c
  | (StgInfoTable { tipe = TREC_CHUNK }) <- decodedTable itb = decodeTrecChunk i c
  | (StgInfoTable { tipe = BLOCKING_QUEUE }) <- decodedTable itb = decodeBlockingQueue i c
  | (StgInfoTable { tipe = STACK }) <- decodedTable itb = decodeStack i c
decodeClosure (itb, RawInfoTable rit) (ptr, rc@(RawClosure clos)) = unsafePerformIO $ do
    allocate rit $ \itblPtr -> do
      allocate clos $ \closPtr -> do
        let ptr_to_itbl_ptr :: Ptr (Ptr StgInfoTable)
            ptr_to_itbl_ptr = castPtr closPtr
        -- The pointer is to the end of the info table (not the start)
        -- Info table is two words long which is why we subtract 16 from
        -- the pointer
        --print (itblPtr, closPtr)
        -- Save the old value of itbl_ptr so we can put it back if we're in
        -- the no copying mode (allocateByPtr)
        old_itbl <- peek ptr_to_itbl_ptr
        poke ptr_to_itbl_ptr (fixTNTC itblPtr)
        -- You should be able to print these addresses in gdb
        -- and observe the memory layout is identical to the debugee
        -- process
        -- Printing this return value can lead to segfaults because the
        -- pointer for constrDesc won't point to a string after being
        -- decoded.
        --print (tipe (decodedTable itb), ptr, closPtr, itblPtr)
        (!r, !s) <- getClosureRaw (decodedTable itb) closPtr clos
        -- Mutate back the ByteArray as if we attempt to use it again then
        -- the itbl pointer will point somewhere into our address space
        -- rather than the debuggee address space
        --
        poke ptr_to_itbl_ptr old_itbl
        return $ DCS s . quadmap absurd
                        (\itb' -> PayloadWithKey itb' ptr)
                        stackCont
                        ClosurePtr . convertClosure itb
          $ fmap (\(W# w) -> toBE64 (W64# w)) r
  where
    stackCont :: (Word32, StackPtr) -> StackCont
    stackCont (n,sp) = StackCont sp (getRawStack (n,sp) ptr rc)



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



-- | Invariant: ClosurePtr is within the range of the block
-- The 'RawClosure' this returns is actually the tail of the whole block,
-- this is fine because the memory for each block is only allocated once
-- due to how BS.drop is implemented via pointer arithmetic.
extractFromBlock :: ClosurePtr
                -> RawBlock
                -> RawClosure
extractFromBlock cp (RawBlock bp _ b) =
--  Calling closureSize doesn't work as the info table addresses are bogus
--  clos_size_w <- withForeignPtr fp' (\p -> return $ closureSize (ptrToBox p))
--  let clos_size = clos_size_w * 8
    --traceShow (fp, offset, cp, bp,o, l)
    --traceShow ("FP", fp `plusForeignPtr` offset)
    RawClosure (B.drop offset b)
    where
      offset = fromIntegral (subtractBlockPtr (untagClosurePtr cp) bp)




