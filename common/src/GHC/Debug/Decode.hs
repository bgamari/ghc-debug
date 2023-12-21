{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Low-level functions for decoding a closure representation from the raw
-- bytes
module GHC.Debug.Decode ( decodeClosure
                        , decodeInfoTable
                        ) where

import GHC.Ptr (plusPtr, castPtr)
import GHC.Exts hiding (closureSize#) -- (Addr#, unsafeCoerce#, Any, Word#, ByteArray#)
import GHC.Word
import GHC.IO.Unsafe
import Foreign.Storable

import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Short.Internal (ShortByteString(..), toShort)
import qualified Data.ByteString.Lazy as BSL

import GHC.Exts.Heap (GenClosure)
import GHC.Exts.Heap hiding (GenClosure(..), Closure)
import qualified GHC.Exts.Heap.InfoTable as Itbl
import qualified GHC.Exts.Heap.InfoTableProf as ItblProf

import GHC.Debug.Types.Ptr
import GHC.Debug.Types.Version
import GHC.Debug.Types.Closures
import GHC.Debug.Decode.Convert
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.ForeignPtr       (withForeignPtr)
import Data.Binary.Get as B
import Data.Binary
import Control.Monad
import Data.Void
import Control.DeepSeq
import GHC.Exts.Heap.FFIClosures
import Foreign.Marshal.Utils (copyBytes)
import Data.Functor

import qualified Data.ByteString as B

foreign import prim "unpackClosurePtrzh" unpackClosurePtr# ::
              Addr# -> (# ByteArray# #)

foreign import prim "closureSizezh" closureSize# ::
              Addr# -> (# Word# #)

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
allocate = allocateByCopy


-- | Allocate a bytestring directly into memory and return a pointer to the
-- allocated buffer
allocateByCopy :: BSI.ByteString -> (Ptr a -> IO a) -> IO a
allocateByCopy (BSI.PS fp o l) action =
 allocaBytes l $ \buf ->
   withForeignPtr fp $ \p -> do
     --print (fp, o, l)
     copyBytes buf (p `plusPtr` o) (fromIntegral l)
     action (castPtr buf)

skipClosureHeader :: Version -> Get (Maybe ProfHeaderWithPtr)
skipClosureHeader ver = do
  () <$ skip (8 * 1)
  getProfHeader ver

getProfHeader :: Version -> Get (Maybe ProfHeaderWithPtr)
getProfHeader Version{..}
  | v_profiling = do
      ccs <- get
      header <- getWord64le
      pure $ Just $ ProfHeader ccs header
  | otherwise = pure Nothing

decodePAPClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodePAPClosure ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  carity <- getWord32le
  nargs <- getWord32le
  funp <- getClosurePtr
  cpayload <- replicateM (fromIntegral nargs) getWord64le
  let cont = PayloadCont funp cpayload
  return $ (GHC.Debug.Types.Closures.PAPClosure infot prof carity nargs funp cont)

decodeAPClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeAPClosure ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  _smp_header <- getWord64le
  -- _itbl <- skipClosureHeader
  carity <- getWord32le
  nargs <- getWord32le
  funp <- getClosurePtr
  cpayload <- replicateM (fromIntegral nargs) getWord64le
  let cont = PayloadCont funp cpayload
  return $ (GHC.Debug.Types.Closures.APClosure infot prof carity nargs funp cont)


decodeTVarClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeTVarClosure ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  ptr <- getClosurePtr
  watch_queue <- getClosurePtr
  updates <- getInt64le
  return $ (TVarClosure infot prof ptr watch_queue (fromIntegral updates))

getClosurePtr :: Get ClosurePtr
getClosurePtr = get

getWord :: Get Word64
getWord = getWord64le

decodeMutPrim :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeMutPrim ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  let kptrs = fromIntegral (ptrs (decodedTable infot))
      kdat = fromIntegral (nptrs (decodedTable infot))
  pts <- replicateM kptrs getClosurePtr
  dat <- replicateM kdat (fromIntegral <$> getWord64le)
  return $ (MutPrimClosure infot prof pts dat)

decodeTrecChunk :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeTrecChunk ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  prev <- getClosurePtr
  clos_next_idx <- getWord64le
  chunks <- replicateM (fromIntegral clos_next_idx) getChunk
  return $ (TRecChunkClosure infot prof prev (fromIntegral clos_next_idx) chunks)
  where
    getChunk = do
      TRecEntry <$> getClosurePtr
                <*> getClosurePtr
                <*> getClosurePtr
                <*> (fromIntegral <$> getInt64le) -- TODO: num_updates field is wrong
                                                  -- Not sure how it should
                                                  -- be decoded

decodeBlockingQueue :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeBlockingQueue ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  q <- getClosurePtr
  bh <- getClosurePtr
  tso <- getClosurePtr
  bh_q <- getClosurePtr
  return $ (GHC.Debug.Types.Closures.BlockingQueueClosure infot prof q bh tso bh_q)

-- It is just far simpler to directly decode the stack here rather than use
-- the existing logic in ghc-heap......
decodeStack :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeStack ver (infot, _) (cp, rc) = decodeFromBS rc $ do
   prof <- skipClosureHeader ver
   st_size <- getWord32le
   st_dirty <- getWord8
   st_marking <- getWord8
   -- Up to now, 14 bytes are read, skip 2 to get to 16/start of
   -- sp field
   skip 2
   st_sp <- StackPtr <$> getWord
   stackHeaderSize <- bytesRead
   let stack_offset = fromIntegral (subtractStackPtr st_sp cp)
             -- -stackHeaderSize for the bytes already read
             - fromIntegral stackHeaderSize
       len = calculateStackLen st_size (fromIntegral stackHeaderSize) cp st_sp
   -- Skip to start of stack frames
   skip stack_offset
   -- Read the raw frames, we can't decode them yet because we
   -- need to query the debuggee for the bitmaps
   raw_stack <- RawStack <$> getByteString (fromIntegral len)
   return (GHC.Debug.Types.Closures.StackClosure
            infot
            prof
            st_size
            st_dirty
            st_marking
            (StackCont st_sp raw_stack))

decodeFromBS :: RawClosure -> Get (DebugClosure ccs srt pap string s b)
                           -> DebugClosureWithExtra Size ccs srt pap string s b
decodeFromBS (RawClosure rc) parser =
  case runGetOrFail parser (BSL.fromStrict rc) of
    Left err -> error ("DEC:" ++ show err ++ printBS rc)
    Right (_rem, o, v) ->
      let !s = fromIntegral o
      in DCS (Size s) v

decodeAPStack :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeAPStack ver (infot, _) (ClosurePtr cp, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  _smp_header <- getWord64le
  st_size <- getWord
  fun_closure <- getClosurePtr
  k <- bytesRead
  let sp = addStackPtr (StackPtr cp) (fromIntegral k)
  clos_payload <- RawStack <$> getByteString (fromIntegral st_size)
  return $ GHC.Debug.Types.Closures.APStackClosure
              infot
              prof
              (fromIntegral st_size)
              fun_closure
              (StackCont sp clos_payload)

decodeStandardLayout :: Version
                     -> Get ()
                     -> (Maybe ProfHeaderWithPtr -> [ClosurePtr] -> [Word] -> Closure)
                     -> (StgInfoTableWithPtr, RawInfoTable)
                     -> (ClosurePtr, RawClosure)
                     -> SizedClosure
decodeStandardLayout ver extra k (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  -- For the THUNK header
  extra
  pts <- replicateM (fromIntegral (ptrs (decodedTable infot))) getClosurePtr
  cwords <- replicateM (fromIntegral (nptrs (decodedTable infot))) getWord
  return $ k prof pts (map fromIntegral cwords)

decodeArrWords :: Version -> (StgInfoTableWithPtr, b)
               -> (a, RawClosure) -> SizedClosure
decodeArrWords ver  (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  bytes <- getWord64le
  payload <- replicateM (fromIntegral $ bytes `ceilIntDiv` 8) getWord
  return $ GHC.Debug.Types.Closures.ArrWordsClosure infot prof (fromIntegral bytes) (map fromIntegral payload)

-- | Compute @ceiling (a/b)@.
ceilIntDiv :: Integral a => a -> a -> a
ceilIntDiv a b = (a + b - 1) `div` b

tsoVersionChanged :: Version -> Bool
tsoVersionChanged (Version maj min _ _) = (maj > 905) || (maj == 905 && min >= 20220925)

weakNotNull :: Version -> Bool
weakNotNull (Version maj min _ _) = (maj > 904) || (maj == 904 && min >= 2)

decodeTSO :: Version
          -> (StgInfoTableWithPtr, RawInfoTable)
          -> (a, RawClosure)
          -> SizedClosure
decodeTSO ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  link <- getClosurePtr
  global_link <- getClosurePtr
  tsoStack <- getClosurePtr
  what_next <- parseWhatNext <$> getWord16le
  why_blocked <- parseWhyBlocked <$> getWord16le
  flags <- parseTsoFlags <$> getWord32le
  _block_info <- getClosurePtr
  threadId <- getWord64le
  saved_errno <- getWord32le
  dirty       <- getWord32le

  _bound       <- getClosurePtr
  _cap         <- getClosurePtr
  trec           <- getClosurePtr
  threadLabel <-
    if tsoVersionChanged ver
      then do
        thread_label <- getClosurePtr
        return $ if thread_label == mkClosurePtr 0 then Nothing else Just thread_label
      else return Nothing
  blocked_exceptions <- getClosurePtr
  bq             <- getClosurePtr
  alloc_limit    <- getInt64le
  tot_stack_size <- getWord32le
  let res :: Closure = (GHC.Debug.Types.Closures.TSOClosure
            { info = infot
            , profHeader = prof
            , _link = link
            , prof = Nothing
            , .. })
  return res

decodeClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeClosure ver i@(itb, _) c
  -- MP: It was far easier to implement the decoding of these closures in
  -- ghc-heap using binary rather than patching GHC and going through that
  -- dance. I think in the future it's better to do this for all the
  -- closures... it's simpler and probably much faster.
  = case tipe (decodedTable itb) of
      ARR_WORDS -> decodeArrWords ver i c
      PAP -> decodePAPClosure ver i c
      AP -> decodeAPClosure ver i c
      TVAR -> decodeTVarClosure ver i c
      MUT_PRIM -> decodeMutPrim ver i c
      TREC_CHUNK -> decodeTrecChunk ver i c
      BLOCKING_QUEUE -> decodeBlockingQueue ver i c
      TSO -> decodeTSO ver i c
      STACK -> decodeStack ver i c
      AP_STACK -> decodeAPStack ver i c
      THUNK_STATIC -> decodeStandardLayout ver (return ()) (\ph -> ThunkClosure itb ph (tableId itb)) i c
      THUNK_SELECTOR -> decodeThunkSelector ver i c
      BCO -> decodeBCO ver i c
      IND        -> decodeIndirectee ver IndClosure i c
      IND_STATIC -> decodeIndirectee ver IndClosure i c
      BLACKHOLE  -> decodeIndirectee ver BlackholeClosure i c
      MVAR_CLEAN -> decodeMVar ver i c
      MVAR_DIRTY -> decodeMVar ver i c
      MUT_VAR_CLEAN -> decodeMutVar ver i c
      MUT_VAR_DIRTY -> decodeMutVar ver i c
      WEAK -> decodeWeakClosure ver i c
      ty
        | CONSTR <= ty && ty <= CONSTR_0_2 ->
            decodeStandardLayout ver (return ()) (\ph pts ws -> ConstrClosure itb ph pts ws (tableId itb)) i c
        | CONSTR <= ty && ty <= CONSTR_NOCAF ->
            decodeStandardLayout ver (return ()) (\ph pts ws -> ConstrClosure itb ph pts ws (tableId itb)) i c
        | FUN <= ty && ty <= FUN_STATIC ->
            decodeStandardLayout ver (return ()) (\ph -> FunClosure itb ph (tableId itb)) i c
        | THUNK <= ty && ty <= THUNK_0_2 ->
            decodeStandardLayout ver (() <$ getWord) (\ph -> ThunkClosure itb ph (tableId itb)) i c
        | MUT_ARR_PTRS_CLEAN <= ty && ty <= MUT_ARR_PTRS_FROZEN_CLEAN ->
            decodeMutArr ver i c
        | SMALL_MUT_ARR_PTRS_CLEAN <= ty && ty <= SMALL_MUT_ARR_PTRS_FROZEN_CLEAN ->
            decodeSmallMutArr ver i c
        | otherwise -> decodeWithLibrary ver i c

decodeWeakClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeWeakClosure ver (infot, _) (_, rc) = decodeFromBS rc $ do
  profHeader <- skipClosureHeader ver
  cfinalizers <- getClosurePtr
  key <- getClosurePtr
  value <- getClosurePtr
  finalizer <- getClosurePtr
  mlink <- do
    p@(ClosurePtr w) <- getClosurePtr
    pure $ if w == 0 then Nothing else Just p
  pure $ WeakClosure infot profHeader cfinalizers key value finalizer mlink

decodeMVar :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeMVar ver (infot, _) (_, rc) = decodeFromBS rc $ do
  profHeader <- skipClosureHeader ver
  hd <- getClosurePtr
  tl <- getClosurePtr
  val <- getClosurePtr
  pure $ MVarClosure infot profHeader hd tl val

decodeMutVar :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeMutVar ver (infot, _) (_, rc) = decodeFromBS rc $ do
  profHeader <- skipClosureHeader ver
  val <- getClosurePtr
  pure $ MutVarClosure infot profHeader val

decodeMutArr :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeMutArr ver (infot, _) (_, rc) = decodeFromBS rc $ do
  profHeader <- skipClosureHeader ver
  nptrs <- getWord64le
  size <- getWord64le
  payload <- replicateM (fromIntegral nptrs) getClosurePtr
  pure $ MutArrClosure infot profHeader (fromIntegral nptrs) (fromIntegral size) payload

decodeSmallMutArr :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeSmallMutArr ver (infot, _) (_, rc) = decodeFromBS rc $ do
  profHeader <- skipClosureHeader ver
  nptrs <- getWord64le
  payload <- replicateM (fromIntegral nptrs) getClosurePtr
  pure $ SmallMutArrClosure infot profHeader (fromIntegral nptrs) payload

decodeIndirectee :: Version
                 -> (StgInfoTableWithPtr -> Maybe ProfHeaderWithPtr -> ClosurePtr -> Closure)
                 -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeIndirectee ver mk (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  ind <- getClosurePtr
  pure $ mk infot prof ind

decodeBCO :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeBCO ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  instrs <- getClosurePtr
  literals <- getClosurePtr
  bcoptrs <- getClosurePtr
  arity <- getWord32le
  size <- getWord32le
  bitmap <- replicateM (fromIntegral size) (fromIntegral <$> getWord64le) -- TODO getWord?
  pure (BCOClosure infot prof instrs literals bcoptrs arity size bitmap)


decodeThunkSelector :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeThunkSelector ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- skipClosureHeader ver
  (() <$ getWord)
  selectee <- getClosurePtr
  pure (SelectorClosure infot prof selectee)

decodeWithLibrary :: Version -> (StgInfoTableWithPtr, RawInfoTable)
                      -> (a, RawClosure)
                      -> SizedClosure
decodeWithLibrary ver (itb, RawInfoTable rit) (_, (RawClosure clos)) = unsafePerformIO $ do
    allocate rit $ \itblPtr -> do
      allocate clos $ \closPtr -> do
        let ptr_to_itbl_ptr :: Ptr (Ptr StgInfoTable)
            ptr_to_itbl_ptr = castPtr closPtr
        -- The pointer is to the end of the info table (not the start)
        -- Info table is two words long which is why we subtract 16 from
        -- the pointer
        --print (itblPtr, closPtr)
        poke ptr_to_itbl_ptr (fixTNTC ver itblPtr)
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
        return $ DCS s . quinmap id id absurd
                        id
                        absurd
                        mkClosurePtr . convertClosure itb
          $ fmap (fromIntegral @Word @Word64) r


fixTNTC :: Version -> Ptr a -> Ptr StgInfoTable
fixTNTC ver ptr
  | v_tntc ver  = castPtr $ ptr  `plusPtr` realItblSize ver
  | otherwise        = castPtr $ ptr

realItblSize :: Version -> Int
realItblSize Version{..}
  | v_profiling  = ItblProf.itblSize
  | otherwise  = Itbl.itblSize

decodeInfoTable :: Version -> RawInfoTable -> StgInfoTable
decodeInfoTable Version{..} (RawInfoTable itbl) =
  case runGetOrFail itParser (BSL.fromStrict itbl) of
    Left err -> error ("DEC:" ++ show err ++ printBS itbl)
    Right (_rem, o, v) ->
      let !s = fromIntegral o
      in v
  where
    itParser = do
      entry <- case v_tntc of
        True -> pure Nothing
        False -> do
          getWord64le -- todo return funptr
          pure Nothing
      when v_profiling $ do
        () <$ getWord64le
        () <$ getWord64le
      ptrs <- getWord32le
      nptrs <- getWord32le
      tipe <- getWord32le
      srtlen <- getWord32le
      return $
        StgInfoTable
        { entry = entry
        , ptrs = ptrs
        , nptrs = nptrs
        , tipe = toEnum (fromIntegral tipe)
        , srtlen = srtlen
        , code = Nothing
        }

