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

import GHC.Debug.Types.Ptr
import GHC.Debug.Types.Version
import GHC.Debug.Types.Closures
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.ForeignPtr       (withForeignPtr)
import Data.Binary.Get as B
import Data.Binary
import Control.Monad
import Data.Void
import Control.DeepSeq
import Foreign.Marshal.Utils (copyBytes)
import Data.Functor
import Data.Bits

import qualified Data.ByteString as B

foreign import prim "unpackClosurePtrzh" unpackClosurePtr# ::
              Addr# -> (# ByteArray# #)

foreign import prim "closureSizezh" closureSize# ::
              Addr# -> (# Word# #)

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

decodeClosureHeader :: Version -> Get (Maybe ProfHeaderWithPtr)
decodeClosureHeader ver = do
  () <$ skip (8 * 1)
  getProfHeader ver

getProfHeader :: Version -> Get (Maybe ProfHeaderWithPtr)
getProfHeader ver =
  case v_profiling ver of
    Nothing -> pure Nothing
    Just mode -> do
      ccs <- get
      header <- getWord64le
      pure $ Just $ ProfHeader ccs (decodeHeader mode header)

decodeHeader :: ProfilingMode -> Word64 -> ProfHeaderWord
decodeHeader mode hp = case mode of
  NoProfiling -> OtherHeader hp
  OtherProfiling -> OtherHeader hp
  -- TODO handle 32 bit
  RetainerProfiling -> RetainerHeader (testBit hp 0) (RetainerSetPtr $ clearBit hp 0)
  LDVProfiling -> LDVWord (testBit hp 60) (fromIntegral ((hp .&. _LDV_CREATE_MASK) `shiftR` _LDV_SHIFT)) (fromIntegral (hp .&. _LDV_LAST_MASK))
  EraProfiling -> EraWord hp

_LDV_CREATE_MASK, _LDV_LAST_MASK :: Word64
_LDV_CREATE_MASK = 0x0FFFFFFFC0000000
_LDV_LAST_MASK = 0x000000003FFFFFFF
_LDV_SHIFT :: Int
_LDV_SHIFT = 30

decodePAPClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodePAPClosure ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- decodeClosureHeader ver
  carity <- getWord32le
  nargs <- getWord32le
  funp <- getClosurePtr
  cpayload <- replicateM (fromIntegral nargs) getWord64le
  let cont = PayloadCont funp cpayload
  return $ (GHC.Debug.Types.Closures.PAPClosure infot prof carity nargs funp cont)

decodeAPClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeAPClosure ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- decodeClosureHeader ver
  _smp_header <- getWord64le
  -- _itbl <- decodeClosureHeader
  carity <- getWord32le
  nargs <- getWord32le
  funp <- getClosurePtr
  cpayload <- replicateM (fromIntegral nargs) getWord64le
  let cont = PayloadCont funp cpayload
  return $ (GHC.Debug.Types.Closures.APClosure infot prof carity nargs funp cont)


decodeTVarClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeTVarClosure ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- decodeClosureHeader ver
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
  prof <- decodeClosureHeader ver
  let kptrs = fromIntegral (ptrs (decodedTable infot))
      kdat = fromIntegral (nptrs (decodedTable infot))
  pts <- replicateM kptrs getClosurePtr
  dat <- replicateM kdat (fromIntegral <$> getWord64le)
  return $ (MutPrimClosure infot prof pts dat)

decodeTrecChunk :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeTrecChunk ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- decodeClosureHeader ver
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
  prof <- decodeClosureHeader ver
  q <- getClosurePtr
  bh <- getClosurePtr
  tso <- getClosurePtr
  bh_q <- getClosurePtr
  return $ (GHC.Debug.Types.Closures.BlockingQueueClosure infot prof q bh tso bh_q)

-- It is just far simpler to directly decode the stack here rather than use
-- the existing logic in ghc-heap......
decodeStack :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeStack ver (infot, _) (cp, rc) = decodeFromBS rc $ do
   prof <- decodeClosureHeader ver
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
  prof <- decodeClosureHeader ver
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
  prof <- decodeClosureHeader ver
  -- For the THUNK header
  extra
  pts <- replicateM (fromIntegral (ptrs (decodedTable infot))) getClosurePtr
  cwords <- replicateM (fromIntegral (nptrs (decodedTable infot))) getWord
  return $ k prof pts (map fromIntegral cwords)

decodeArrWords :: Version -> (StgInfoTableWithPtr, b)
               -> (a, RawClosure) -> SizedClosure
decodeArrWords ver  (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- decodeClosureHeader ver
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
  prof <- decodeClosureHeader ver
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

parseWhatNext :: Word16 -> WhatNext
parseWhatNext i = case i of
  1 -> ThreadRunGHC
  2 -> ThreadInterpret
  3 -> ThreadKilled
  4 -> ThreadComplete
  _ -> WhatNextUnknownValue i

parseWhyBlocked :: Word16 -> WhyBlocked
parseWhyBlocked i = case i of
  0  -> NotBlocked
  1  -> BlockedOnMVar
  14 -> BlockedOnMVarRead
  2  -> BlockedOnBlackHole
  3  -> BlockedOnRead
  4  -> BlockedOnWrite
  5  -> BlockedOnDelay
  6  -> BlockedOnSTM
  7  -> BlockedOnDoProc
  10 -> BlockedOnCCall
  11 -> BlockedOnCCall_Interruptible
  12 -> BlockedOnMsgThrowTo
  13 -> ThreadMigrating
  _  -> WhyBlockedUnknownValue i

parseTsoFlags :: Word32 -> [TsoFlags]
parseTsoFlags w =
  go [ (TsoLocked             , 1)
     , (TsoBlockx             , 2)
     , (TsoInterruptible      , 3)
     , (TsoStoppedOnBreakpoint, 4)
     , (TsoMarked             , 5)
     , (TsoSqueezed           , 6)
     , (TsoAllocLimit         , 7)
     ]
  where
    go xs = [flag | (flag, i) <- xs, testBit w i]

decodeClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeClosure ver i@(itb, _) c
  -- MP: It was far easier to implement the decoding of these closures in
  -- ghc-heap using binary rather than patching GHC and going through that
  -- dance.
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
        | otherwise -> error $ "unhandled closure type" ++ show ty

decodeWeakClosure :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeWeakClosure ver (infot, _) (_, rc) = decodeFromBS rc $ do
  profHeader <- decodeClosureHeader ver
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
  profHeader <- decodeClosureHeader ver
  hd <- getClosurePtr
  tl <- getClosurePtr
  val <- getClosurePtr
  pure $ MVarClosure infot profHeader hd tl val

decodeMutVar :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeMutVar ver (infot, _) (_, rc) = decodeFromBS rc $ do
  profHeader <- decodeClosureHeader ver
  val <- getClosurePtr
  pure $ MutVarClosure infot profHeader val

decodeMutArr :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeMutArr ver (infot, _) (_, rc) = decodeFromBS rc $ do
  profHeader <- decodeClosureHeader ver
  nptrs <- getWord64le
  size <- getWord64le
  payload <- replicateM (fromIntegral nptrs) getClosurePtr
  pure $ MutArrClosure infot profHeader (fromIntegral nptrs) (fromIntegral size) payload

decodeSmallMutArr :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeSmallMutArr ver (infot, _) (_, rc) = decodeFromBS rc $ do
  profHeader <- decodeClosureHeader ver
  nptrs <- getWord64le
  payload <- replicateM (fromIntegral nptrs) getClosurePtr
  pure $ SmallMutArrClosure infot profHeader (fromIntegral nptrs) payload

decodeIndirectee :: Version
                 -> (StgInfoTableWithPtr -> Maybe ProfHeaderWithPtr -> ClosurePtr -> Closure)
                 -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) -> SizedClosure
decodeIndirectee ver mk (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- decodeClosureHeader ver
  ind <- getClosurePtr
  pure $ mk infot prof ind

decodeBCO :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeBCO ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- decodeClosureHeader ver
  instrs <- getClosurePtr
  literals <- getClosurePtr
  bcoptrs <- getClosurePtr
  arity <- getWord32le
  size <- getWord32le
  bitmap <- replicateM (fromIntegral size) (fromIntegral <$> getWord64le) -- TODO getWord?
  pure (BCOClosure infot prof instrs literals bcoptrs arity size bitmap)


decodeThunkSelector :: Version -> (StgInfoTableWithPtr, RawInfoTable) -> (ClosurePtr, RawClosure) ->  SizedClosure
decodeThunkSelector ver (infot, _) (_, rc) = decodeFromBS rc $ do
  prof <- decodeClosureHeader ver
  (() <$ getWord)
  selectee <- getClosurePtr
  pure (SelectorClosure infot prof selectee)

decodeInfoTable :: Version -> RawInfoTable -> StgInfoTable
decodeInfoTable ver@Version{..} (RawInfoTable itbl) =
  case runGetOrFail itParser (BSL.fromStrict itbl) of
    Left err -> error ("DEC:" ++ show err ++ printBS itbl)
    Right (_rem, o, v) ->
      let !s = fromIntegral o
      in v
  where
    itParser = do
      _entry <- case v_tntc of
        True -> pure Nothing
        False -> do
          getWord64le -- todo return funptr
          pure Nothing
      when (isProfiledRTS ver) $ do
        () <$ getWord64le
        () <$ getWord64le
      ptrs <- getWord32le
      nptrs <- getWord32le
      tipe <- getWord32le
      srtlen <- getWord32le
      return $
        StgInfoTable
        { ptrs = ptrs
        , nptrs = nptrs
        , tipe = decodeInfoTableType tipe
        , srtlen = srtlen
        }

decodeInfoTableType :: Word32 -> ClosureType
decodeInfoTableType i = case i of
  0 -> INVALID_OBJECT
  1 -> CONSTR
  2 -> CONSTR_1_0
  3 -> CONSTR_0_1
  4 -> CONSTR_2_0
  5 -> CONSTR_1_1
  6 -> CONSTR_0_2
  7 -> CONSTR_NOCAF
  8 -> FUN
  9 -> FUN_1_0
  10 -> FUN_0_1
  11 -> FUN_2_0
  12 -> FUN_1_1
  13 -> FUN_0_2
  14 -> FUN_STATIC
  15 -> THUNK
  16 -> THUNK_1_0
  17 -> THUNK_0_1
  18 -> THUNK_2_0
  19 -> THUNK_1_1
  20 -> THUNK_0_2
  21 -> THUNK_STATIC
  22 -> THUNK_SELECTOR
  23 -> BCO
  24 -> AP
  25 -> PAP
  26 -> AP_STACK
  27 -> IND
  28 -> IND_STATIC
  29 -> RET_BCO
  30 -> RET_SMALL
  31 -> RET_BIG
  32 -> RET_FUN
  33 -> UPDATE_FRAME
  34 -> CATCH_FRAME
  35 -> UNDERFLOW_FRAME
  36 -> STOP_FRAME
  37 -> BLOCKING_QUEUE
  38 -> BLACKHOLE
  39 -> MVAR_CLEAN
  40 -> MVAR_DIRTY
  41 -> TVAR
  42 -> ARR_WORDS
  43 -> MUT_ARR_PTRS_CLEAN
  44 -> MUT_ARR_PTRS_DIRTY
  45 -> MUT_ARR_PTRS_FROZEN_DIRTY
  46 -> MUT_ARR_PTRS_FROZEN_CLEAN
  47 -> MUT_VAR_CLEAN
  48 -> MUT_VAR_DIRTY
  49 -> WEAK
  50 -> PRIM
  51 -> MUT_PRIM
  52 -> TSO
  53 -> STACK
  54 -> TREC_CHUNK
  55 -> ATOMICALLY_FRAME
  56 -> CATCH_RETRY_FRAME
  57 -> CATCH_STM_FRAME
  58 -> WHITEHOLE
  59 -> SMALL_MUT_ARR_PTRS_CLEAN
  60 -> SMALL_MUT_ARR_PTRS_DIRTY
  61 -> SMALL_MUT_ARR_PTRS_FROZEN_DIRTY
  62 -> SMALL_MUT_ARR_PTRS_FROZEN_CLEAN
  63 -> COMPACT_NFDATA
  64 -> CONTINUATION
  65 -> N_CLOSURE_TYPES

