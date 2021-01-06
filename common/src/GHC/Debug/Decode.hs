{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE BangPatterns #-}
-- | Low-level functions for decoding a closure representation from the raw
-- bytes
module GHC.Debug.Decode (decodeClosure, decodeClosureWithSize, decodeInfoTable, extractFromBlock) where

import GHC.Ptr (plusPtr, castPtr)
import GHC.Exts hiding (closureSize#) -- (Addr#, unsafeCoerce#, Any, Word#, ByteArray#)
import GHC.Word
import GHC.IO.Unsafe
import Foreign.Storable

import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Short.Internal (ShortByteString(..), toShort)
import qualified Data.ByteString.Lazy as BSL

import GHC.Exts.Heap hiding (Closure)
import qualified GHC.Exts.Heap.InfoTable as Itbl
import qualified GHC.Exts.Heap.InfoTableProf as ItblProf

import GHC.Debug.Types.Ptr
import GHC.Debug.Types.Closures
import GHC.Debug.Decode.Convert
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.ForeignPtr       (withForeignPtr)
import GHC.ForeignPtr
import Data.Binary.Get as B
import Control.Monad
import Data.Void
import Control.DeepSeq

import qualified Data.ByteString as B

foreign import prim "unpackClosurePtrzh" unpackClosurePtr# ::
              Addr# -> (# ByteArray# #)

foreign import prim "closureSizezh" closureSize# ::
              Addr# -> (# Word# #)

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
      rawPtrs = force [W# (indexWordArray# pointers i) | I# i <- [0.. end_ptrs] ]
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

decodeClosureWithSize :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeClosureWithSize itb (ptr, rc) = decodeClosure itb (ptr, rc)

skipClosureHeader :: Get ()
skipClosureHeader
  | profiling = () <$ skip 24
  | otherwise = () <$ skip 8

decodePAPClosure :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodePAPClosure (infot, _) (_, rc) = decodeFromBS rc $ do
  _itbl <- skipClosureHeader
  carity <- getWord32le
  nargs <- getWord32le
  funp <- getClosurePtr
  cpayload <- replicateM (fromIntegral nargs) getWord64le
  let cont = PayloadCont funp cpayload
  return $ (GHC.Debug.Types.Closures.PAPClosure infot carity nargs funp cont)

decodeAPClosure :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeAPClosure (infot, _) (_, rc) = decodeFromBS rc $ do
  _itbl <- skipClosureHeader
  carity <- getWord32le
  nargs <- getWord32le
  fun_ptr <- getWord64le
  cpayload <- replicateM (fromIntegral nargs) getWord64le
  let funp = (ClosurePtr fun_ptr)
      cont = PayloadCont funp cpayload
  return $ (GHC.Debug.Types.Closures.APClosure infot carity nargs funp cont)


decodeTVarClosure :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeTVarClosure (infot, _) (_, rc) = decodeFromBS rc $ do
  _itbl <- skipClosureHeader
  ptr <- getClosurePtr
  watch_queue <- getClosurePtr
  updates <- getInt64le
  return $ (TVarClosure infot ptr watch_queue (fromIntegral updates))

getClosurePtr :: Get ClosurePtr
getClosurePtr = ClosurePtr <$> getWord64le

getWord :: Get Word64
getWord = getWord64le

decodeMutPrim :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeMutPrim (infot, _) (_, rc) = decodeFromBS rc $ do
  _itbl <- skipClosureHeader
  let kptrs = fromIntegral (ptrs (decodedTable infot))
      kdat = fromIntegral (nptrs (decodedTable infot))
  pts <- replicateM kptrs getClosurePtr
  dat <- replicateM kdat (fromIntegral <$> getWord64le)
  return $ (MutPrimClosure infot pts dat)

decodeTrecChunk :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeTrecChunk (infot, _) (_, rc) = decodeFromBS rc $ do
  _itbl <- skipClosureHeader
  prev <- getClosurePtr
  clos_next_idx <- getWord64le
  chunks <- replicateM (fromIntegral clos_next_idx) getChunk
  return $ (TRecChunkClosure infot prev (fromIntegral clos_next_idx) chunks)

  where
    getChunk = do
      TRecEntry <$> getClosurePtr
                <*> getClosurePtr
                <*> getClosurePtr
                <*> (fromIntegral <$> getInt64le) -- TODO: num_updates field is wrong
                                                  -- Not sure how it should
                                                  -- be decoded

decodeBlockingQueue :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeBlockingQueue (infot, _) (_, rc) = decodeFromBS rc $ do
  _itbl <- skipClosureHeader
  q <- getClosurePtr
  bh <- getClosurePtr
  tso <- getClosurePtr
  bh_q <- getClosurePtr
  return $ (GHC.Debug.Types.Closures.BlockingQueueClosure infot q bh tso bh_q)

-- It is just far simpler to directly decode the stack here rather than use
-- the existing logic in ghc-heap......
decodeStack :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeStack (infot, _) (cp, rc) = decodeFromBS rc $ do
   _itbl <- skipClosureHeader
   st_size <- getWord32le
   st_dirty <- getWord8
   st_marking <- getWord8
   -- Up to now, 14 bytes are read, skip 2 to get to 16/start of
   -- sp field
   skip 2
   st_sp <- StackPtr <$> getWord
   stackHeaderSize <- bytesRead
   let k = fromIntegral (subtractStackPtr st_sp cp)
             -- -stackHeaderSize for the bytes already read
             - fromIntegral stackHeaderSize
       len = calculateStackLen st_size (fromIntegral stackHeaderSize) cp st_sp
   -- Skip to start of stack frames
   skip k
   -- Read the raw frames, we can't decode them yet because we
   -- need to query the debuggee for the bitmaps
   raw_stack <- RawStack <$> getByteString (fromIntegral len)
   return (GHC.Debug.Types.Closures.StackClosure
            infot
            st_size
            st_dirty
            st_marking
            (StackCont st_sp raw_stack))

decodeFromBS :: RawClosure -> Get (DebugClosure pap string s b)
                           -> DebugClosureWithExtra Size pap string s b
decodeFromBS (RawClosure rc) parser =
  case runGetOrFail parser (BSL.fromStrict rc) of
    Left err -> error (show err)
    Right (_rem, o, v) ->
      let !s = fromIntegral o
      in DCS (Size s) v

decodeAPStack :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeAPStack (infot, _) (cp, rc) = decodeFromBS rc $ do
  _itbl <- skipClosureHeader
  st_size <- getWord
  fun_closure <- getClosurePtr
  k <- bytesRead
  let sp = addStackPtr (coerce cp) (fromIntegral k)
  clos_payload <- RawStack <$> getByteString (fromIntegral st_size)
  return $ GHC.Debug.Types.Closures.APStackClosure
              infot
              (fromIntegral st_size)
              fun_closure
              (StackCont sp clos_payload)

decodeClosure :: (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeClosure i@(itb, _) c
  | (StgInfoTable { tipe = PAP }) <- decodedTable itb = decodePAPClosure i c
  | (StgInfoTable { tipe = AP }) <- decodedTable itb = decodeAPClosure i c
  | (StgInfoTable { tipe = TVAR }) <- decodedTable itb = decodeTVarClosure i c
  | (StgInfoTable { tipe = MUT_PRIM }) <- decodedTable itb = decodeMutPrim i c
  | (StgInfoTable { tipe = TREC_CHUNK }) <- decodedTable itb = decodeTrecChunk i c
  | (StgInfoTable { tipe = BLOCKING_QUEUE }) <- decodedTable itb = decodeBlockingQueue i c
  | (StgInfoTable { tipe = STACK }) <- decodedTable itb = decodeStack i c
  | (StgInfoTable { tipe = AP_STACK }) <- decodedTable itb = decodeAPStack i c
decodeClosure (itb, RawInfoTable rit) (_, (RawClosure clos)) = unsafePerformIO $ do
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
                        id
                        absurd
                        ClosurePtr . convertClosure itb
          $ fmap (\(W# w) -> (W64# w)) r
  where
--    stackCont :: (Word32, StackPtr) -> StackCont
--    stackCont (n,sp) = StackCont sp (getRawStack (n,sp) ptr rc)


fixTNTC :: Ptr a -> Ptr StgInfoTable
fixTNTC ptr
  | tablesNextToCode = castPtr $ ptr  `plusPtr` realItblSize
  | otherwise        = castPtr $ ptr

realItblSize :: Int
realItblSize
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

