{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{- | The Haskell representation of a heap closure, the 'DebugClosure' type
- is quite similar to the one found in the @ghc-heap@ package but with some
- more type parameters and other changes..
-}
module GHC.Debug.Types.Closures (
    -- * Closure Representation
      Closure
    , SizedClosure
    , SizedClosureC
    , SizedClosureP
    , DebugClosure(..)
    , TRecEntry(..)
    -- * Wrappers
    , DebugClosureWithSize
    , DebugClosureWithExtra(..)
    , Size(..)
    , InclusiveSize(..)
    , RetainerSize(..)
    , noSize
    , dcSize
    , allClosures
    -- * Info Table Representation
    , StgInfoTable(..)
    , ClosureType(..)
    , WhatNext(..)
    , WhyBlocked(..)
    , TsoFlags(..)
    , StgInfoTableWithPtr(..)
    -- * Stack Frame Representation
    , DebugStackFrame(..)
    , FieldValue(..)
    , GenStackFrames(..)
    , StackFrames
    , StackCont(..)
    -- * PAP payload representation
    , GenPapPayload(..)
    , PapPayload
    , PayloadCont(..)
    -- * Constructor Description Representation
    , ConstrDesc(..)
    , ConstrDescCont
    , parseConstrDesc
    -- * SRT field representation
    , GenSrtPayload(..)
    , SrtPayload
    , SrtCont
    , ProfHeader(..)
    , ProfHeaderWord(..)
    , ProfHeaderWithPtr
    , CCSPayload
    , GenCCSPayload(..)
    , CCPayload(..)

    -- * Traversing functions
    , Hextraversable(..)
    , hexmap
    ) where

import Prelude -- See note [Why do we import Prelude here?]
-- TODO: Support profiling
--import qualified GHC.Exts.Heap.InfoTableProf as ItblProf
-- import GHC.Exts.Heap.InfoTable
-- import qualified GHC.Exts.Heap as GHC
-- import GHC.Exts.Heap.ProfInfo.Types as ProfTypes


import Data.Functor.Identity
import Data.Int
import Data.Word
import GHC.Exts
import GHC.Generics
import GHC.Debug.Types.Ptr
import Data.List (intercalate)
import Data.Char

import Control.Applicative
import Data.Monoid
import Data.Bitraversable
import Data.Bifunctor
import Data.Bifoldable

------------------------------------------------------------------------
-- GHC Heap


data ClosureType
    = INVALID_OBJECT
    | CONSTR
    | CONSTR_1_0
    | CONSTR_0_1
    | CONSTR_2_0
    | CONSTR_1_1
    | CONSTR_0_2
    | CONSTR_NOCAF
    | FUN
    | FUN_1_0
    | FUN_0_1
    | FUN_2_0
    | FUN_1_1
    | FUN_0_2
    | FUN_STATIC
    | THUNK
    | THUNK_1_0
    | THUNK_0_1
    | THUNK_2_0
    | THUNK_1_1
    | THUNK_0_2
    | THUNK_STATIC
    | THUNK_SELECTOR
    | BCO
    | AP
    | PAP
    | AP_STACK
    | IND
    | IND_STATIC
    | RET_BCO
    | RET_SMALL
    | RET_BIG
    | RET_FUN
    | UPDATE_FRAME
    | CATCH_FRAME
    | UNDERFLOW_FRAME
    | STOP_FRAME
    | BLOCKING_QUEUE
    | BLACKHOLE
    | MVAR_CLEAN
    | MVAR_DIRTY
    | TVAR
    | ARR_WORDS
    | MUT_ARR_PTRS_CLEAN
    | MUT_ARR_PTRS_DIRTY
    | MUT_ARR_PTRS_FROZEN_DIRTY
    | MUT_ARR_PTRS_FROZEN_CLEAN
    | MUT_VAR_CLEAN
    | MUT_VAR_DIRTY
    | WEAK
    | PRIM
    | MUT_PRIM
    | TSO
    | STACK
    | TREC_CHUNK
    | ATOMICALLY_FRAME
    | CATCH_RETRY_FRAME
    | CATCH_STM_FRAME
    | WHITEHOLE
    | SMALL_MUT_ARR_PTRS_CLEAN
    | SMALL_MUT_ARR_PTRS_DIRTY
    | SMALL_MUT_ARR_PTRS_FROZEN_DIRTY
    | SMALL_MUT_ARR_PTRS_FROZEN_CLEAN
    | COMPACT_NFDATA
    | CONTINUATION
    | N_CLOSURE_TYPES
 deriving (Enum, Eq, Ord, Show, Generic)

type HalfWord = Word32 -- TODO support 32 bit

data StgInfoTable = StgInfoTable {
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: ClosureType,
   srtlen :: HalfWord 
  } deriving (Eq, Show, Generic)

data WhatNext
  = ThreadRunGHC
  | ThreadInterpret
  | ThreadKilled
  | ThreadComplete
  | WhatNextUnknownValue Word16 -- ^ Please report this as a bug
  deriving (Eq, Show, Generic, Ord)

data WhyBlocked
  = NotBlocked
  | BlockedOnMVar
  | BlockedOnMVarRead
  | BlockedOnBlackHole
  | BlockedOnRead
  | BlockedOnWrite
  | BlockedOnDelay
  | BlockedOnSTM
  | BlockedOnDoProc
  | BlockedOnCCall
  | BlockedOnCCall_Interruptible
  | BlockedOnMsgThrowTo
  | ThreadMigrating
  | WhyBlockedUnknownValue Word16 -- ^ Please report this as a bug
  deriving (Eq, Show, Generic, Ord)

data TsoFlags
  = TsoLocked
  | TsoBlockx
  | TsoInterruptible
  | TsoStoppedOnBreakpoint
  | TsoMarked
  | TsoSqueezed
  | TsoAllocLimit
  | TsoFlagsUnknownValue Word32 -- ^ Please report this as a bug
  deriving (Eq, Show, Generic, Ord)

newtype StgTSOProfInfo = StgTSOProfInfo {
    cccs :: Maybe CCSPtr
} deriving (Show, Generic, Eq, Ord)

------------------------------------------------------------------------
-- Closures


type Closure = DebugClosure CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr
type SizedClosure = DebugClosureWithSize CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr
type SizedClosureC = DebugClosureWithSize CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr
type SizedClosureP = DebugClosureWithSize CCSPtr SrtPayload PapPayload ConstrDesc StackCont ClosurePtr

-- | Information needed to decode a 'ConstrDesc'
type ConstrDescCont = InfoTablePtr

-- | Information needed to decode a PAP payload
data PayloadCont = PayloadCont ClosurePtr [Word64] deriving (Show, Eq)

type DebugClosureWithSize = DebugClosureWithExtra Size

data DebugClosureWithExtra x ccs srt pap string s b
  = DCS { extraDCS :: x
        , unDCS :: DebugClosure ccs srt pap string s b }
    deriving (Show, Ord, Eq)

-- | Exclusive size
newtype Size = Size { getSize :: Int }
  deriving stock (Show, Generic)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving newtype (Num, Ord, Eq)

newtype InclusiveSize = InclusiveSize { getInclusiveSize :: Int }
  deriving stock (Show, Generic)
  deriving (Semigroup, Monoid) via (Sum Int)

newtype RetainerSize = RetainerSize { getRetainerSize :: Int }
  deriving stock (Show, Generic, Ord, Eq)
  deriving (Semigroup, Monoid) via (Sum Int)


noSize :: DebugClosureWithSize ccs srt pap string s b -> DebugClosure ccs srt pap string s b
noSize = unDCS

dcSize :: DebugClosureWithSize ccs srt pap string s b -> Size
dcSize = extraDCS

instance Hextraversable (DebugClosureWithExtra x) where
  hextraverse f g h i j k (DCS x v) = DCS x <$> hextraverse f g h i j k v


data StgInfoTableWithPtr = StgInfoTableWithPtr {
                              tableId :: InfoTablePtr
                            , decodedTable :: StgInfoTable
                            } deriving (Show)

instance Ord StgInfoTableWithPtr where
  compare t1 t2 = compare (tableId t1) (tableId t2)

instance Eq StgInfoTableWithPtr where
  t1 == t2 = tableId t1 == tableId t2

data ProfHeader a = ProfHeader { ccs :: a, hp :: ProfHeaderWord }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ProfHeaderWord
  = RetainerHeader { trav :: !Bool, retainerSet :: !RetainerSetPtr }
  | LDVWord { state :: !Bool, creationTime :: !Word32, lastUseTime :: !Word32 }
  | EraWord Word64
  | OtherHeader Word64
  deriving (Eq, Ord, Show)

type ProfHeaderWithPtr = ProfHeader CCSPtr

-- | This is the representation of a Haskell value on the heap. It reflects
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/includes/rts/storage/Closures.h>
--
-- The data type is parametrized by 4 type parameters which correspond to
-- different pointer types.
--
-- All Heap objects have the same basic layout. A header containing a pointer
-- to the info table and a payload with various fields. The @info@ field below
-- always refers to the info table pointed to by the header. The remaining
-- fields are the payload.
--
-- See
-- <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects>
-- for more information.
data DebugClosure ccs srt pap string s b
  = -- | A data constructor
    ConstrClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        , constrDesc :: !string
        }

    -- | A function
  | FunClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , srt        :: !(srt)
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk, an expression not obviously in head normal form
  | ThunkClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , srt        :: !(srt)
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk which performs a simple selection operation
  | SelectorClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , selectee   :: !b              -- ^ Pointer to the object being
                                        --   selected from
        }

    -- | An unsaturated function application
  | PAPClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , arity      :: !HalfWord       -- ^ Arity of the partial application
        , n_args     :: !HalfWord       -- ^ Size of the payload in words
        , fun        :: !b              -- ^ Pointer to a 'FunClosure'
        , pap_payload    :: !pap            -- ^ Sequence of already applied
                                        --   arguments
        }

    -- In GHCi, if Linker.h would allow a reverse lookup, we could for exported
    -- functions fun actually find the name here.
    -- At least the other direction works via "lookupSymbol
    -- base_GHCziBase_zpzp_closure" and yields the same address (up to tags)
    -- | A function application
  | APClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , arity      :: !HalfWord       -- ^ Always 0
        , n_args     :: !HalfWord       -- ^ Size of payload in words
        , fun        :: !b              -- ^ Pointer to a 'FunClosure'
        , ap_payload    :: !pap            -- ^ Sequence of already applied
                                        --   arguments
        }

    -- | A suspended thunk evaluation
  | APStackClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , ap_st_size :: !Word
        , fun        :: !b              -- ^ Function closure
        , payload    :: !s            -- ^ Stack right before suspension
        }

    -- | A pointer to another closure, introduced when a thunk is updated
    -- to point at its value
  | IndClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , indirectee :: !b              -- ^ Target closure
        }

   -- | A byte-code object (BCO) which can be interpreted by GHC's byte-code
   -- interpreter (e.g. as used by GHCi)
  | BCOClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , instrs     :: !b              -- ^ A pointer to an ArrWords
                                        --   of instructions
        , literals   :: !b              -- ^ A pointer to an ArrWords
                                        --   of literals
        , bcoptrs    :: !b              -- ^ A pointer to an ArrWords
                                        --   of byte code objects
        , arity      :: !HalfWord       -- ^ The arity of this BCO
        , size       :: !HalfWord       -- ^ The size of this BCO in words
        , bitmap     :: ![Word]         -- ^ An StgLargeBitmap describing the
                                        --   pointerhood of its args/free vars
        }

    -- | A thunk under evaluation by another thread
  | BlackholeClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , indirectee :: !b              -- ^ The target closure
        }

    -- | A @ByteArray#@
  | ArrWordsClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , bytes      :: !Word           -- ^ Size of array in bytes
        , arrWords   :: ![Word]         -- ^ Array payload
        }

    -- | A @MutableByteArray#@
  | MutArrClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , mccPtrs    :: !Word           -- ^ Number of pointers
        , mccSize    :: !Word           -- ^ ?? Closures.h vs ClosureMacros.h
        , mccPayload :: ![b]            -- ^ Array payload
        -- Card table ignored
        }

    -- | A @SmallMutableArray#@
    --
    -- @since 8.10.1
  | SmallMutArrClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , mccPtrs    :: !Word           -- ^ Number of pointers
        , mccPayload :: ![b]            -- ^ Array payload
        }

    -- | An @MVar#@, with a queue of thread state objects blocking on them
  | MVarClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , queueHead  :: !b              -- ^ Pointer to head of queue
        , queueTail  :: !b              -- ^ Pointer to tail of queue
        , value      :: !b              -- ^ Pointer to closure
        }

    -- | A @MutVar#@
  | MutVarClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , var        :: !b              -- ^ Pointer to contents
        }

    -- | An STM blocking queue.
  | BlockingQueueClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , link       :: !b              -- ^ ?? Here so it looks like an IND
        , blackHole  :: !b              -- ^ The blackhole closure
        , owner      :: !b              -- ^ The owning thread state object
        , queue      :: !b              -- ^ ??
        }

  | TSOClosure
      { info :: !StgInfoTableWithPtr
      , profHeader :: Maybe (ProfHeader ccs)
      -- pointers
      , _link :: !b
      , global_link :: !b
      , tsoStack :: !b -- ^ stackobj from StgTSO
      , trec :: !b
      , blocked_exceptions :: !b
      , bq :: !b
      , threadLabel :: !(Maybe b)
      -- values
      , what_next :: WhatNext
      , why_blocked :: WhyBlocked
      , flags :: [TsoFlags]
      , threadId :: Word64
      , saved_errno :: Word32
      , dirty :: Word32
      , alloc_limit :: Int64
      , tot_stack_size :: Word32
      , prof :: Maybe StgTSOProfInfo
      }

 | StackClosure
     { info :: !StgInfoTableWithPtr
     , profHeader :: Maybe (ProfHeader ccs)
     , stack_size :: !Word32 -- ^ stack size in *words*
     , stack_dirty :: !Word8 -- ^ non-zero => dirty
     , stack_marking :: !Word8
     , frames :: s
     }


  | WeakClosure
     { info        :: !StgInfoTableWithPtr
     , profHeader :: Maybe (ProfHeader ccs)
     , cfinalizers :: !b
     , key         :: !b
     , value       :: !b
     , finalizer   :: !b
     , mlink       :: !(Maybe b) -- ^ next weak pointer for the capability, can be NULL.
     }

  | TVarClosure
    { info :: !StgInfoTableWithPtr
    , profHeader :: Maybe (ProfHeader ccs)
    , current_value :: !b
    , tvar_watch_queue :: !b
    , num_updates :: !Int }

  | TRecChunkClosure
    { info :: !StgInfoTableWithPtr
    , profHeader :: Maybe (ProfHeader ccs)
    , prev_chunk  :: !b
    , next_idx :: !Word
    , entries :: ![TRecEntry b]
    }

  | MutPrimClosure
    { info :: !StgInfoTableWithPtr
    , profHeader :: Maybe (ProfHeader ccs)
    , ptrArgs :: ![b]
    , dataArgs :: ![Word]
    }

    -----------------------------------------------------------
    -- Anything else

    -- | Another kind of closure
  | OtherClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        , hvalues    :: ![b]
        , rawWords   :: ![Word]
        }

  | UnsupportedClosure
        { info       :: !StgInfoTableWithPtr
        , profHeader :: Maybe (ProfHeader ccs)
        }
  deriving (Show, Generic, Functor, Foldable, Traversable, Ord, Eq)

data TRecEntry b = TRecEntry { tvar :: !b
                             , expected_value :: !b
                             , new_value :: !b
                             , trec_num_updates :: Int -- Only in THREADED, TODO: This is not an Int,
                                                       -- is it a pointer
                                                       -- to a haskell int
                             } deriving (Show, Generic, Functor, Foldable, Traversable, Ord, Eq)

newtype GenPapPayload b = GenPapPayload { getValues :: [FieldValue b] }
  deriving (Functor, Foldable, Traversable, Show, Ord, Eq)

type PapPayload = GenPapPayload ClosurePtr

newtype GenSrtPayload b = GenSrtPayload { getSrt :: Maybe b }
  deriving (Functor, Foldable, Traversable, Show, Ord, Eq)

type SrtPayload = GenSrtPayload ClosurePtr

type SrtCont = InfoTablePtr

data GenCCSPayload ccsPtr ccPtr
  = CCSPayload
  { ccsID :: !Int64
  , ccsCc :: ccPtr
  , ccsPrevStack :: Maybe ccsPtr
  , ccsIndexTable :: Word -- TODO
  , ccsRoot :: Maybe CCSPtr -- todo ccsPtr?
  , ccsDepth :: Word
  , ccsSccCount :: Word64
  , ccsSelected :: Word
  , ccsTimeTicks :: Word
  , ccsMemAlloc :: Word64
  , ccsInheritedAlloc :: Word64
  , ccsInheritedTicks :: Word
  } deriving (Show, Ord, Eq, Functor)

instance Bifunctor GenCCSPayload where
  bimap f g CCSPayload{..} = (\a b -> CCSPayload{ccsPrevStack = a, ccsCc = b, ..})
                              (fmap f ccsPrevStack)
                              (g ccsCc)

instance Bifoldable GenCCSPayload where
  bifoldMap f g CCSPayload{..} = foldMap f ccsPrevStack <> g ccsCc

instance Bitraversable GenCCSPayload where
  bitraverse f g CCSPayload{..} = (\a b -> CCSPayload{ccsPrevStack = a, ccsCc = b, ..})
                              <$> traverse f ccsPrevStack
                              <*> g ccsCc

type CCSPayload = GenCCSPayload CCSPtr CCPtr

data CCPayload
  = CCPayload
  { ccID :: !Int64
  , ccLabel :: String
  , ccMod :: String
  , ccLoc :: String
  , ccMemAlloc :: Word64
  , ccTimeTicks :: Word
  , ccIsCaf :: Bool
  , ccLink :: Maybe CCPtr -- todo ccPtr?
  } deriving (Show, Ord, Eq)

-- | Information needed to decode a set of stack frames
data StackCont = StackCont StackPtr -- Address of start of frames
                           RawStack -- The raw frames
                           deriving (Show, Eq, Ord)

type StackFrames = GenStackFrames SrtCont ClosurePtr
newtype GenStackFrames srt b = GenStackFrames { getFrames :: [DebugStackFrame srt b] }
  deriving (Functor, Foldable, Traversable, Show, Ord, Eq)

instance Bifoldable GenStackFrames where
  bifoldMap f g (GenStackFrames frames) = foldMap (bifoldMap f g) frames

instance Bitraversable GenStackFrames where
  bitraverse f g (GenStackFrames frames) = GenStackFrames <$> traverse (bitraverse f g) frames

instance Bifunctor GenStackFrames where
  bimap f g (GenStackFrames frames) = GenStackFrames (fmap (bimap f g) frames)



data DebugStackFrame srt b
  = DebugStackFrame
        { frame_info :: !StgInfoTableWithPtr
        , frame_srt        :: srt
        , values     :: [FieldValue b]
        } deriving (Traversable, Functor, Foldable, Show, Ord, Eq)


instance Bifunctor DebugStackFrame where
  bimap f g (DebugStackFrame itbl srt v) = DebugStackFrame itbl (f srt) (fmap (fmap g) v)

instance Bifoldable DebugStackFrame where
  bifoldMap f g (DebugStackFrame _ srt v) = f srt <> foldMap (foldMap g) v

instance Bitraversable DebugStackFrame where
  bitraverse f g (DebugStackFrame itbl srt v) = DebugStackFrame itbl <$> f srt <*> traverse (traverse g) v



data ConstrDesc = ConstrDesc {
          pkg        :: !String         -- ^ Package name
        , modl       :: !String         -- ^ Module name
        , name       :: !String         -- ^ Constructor name
        } deriving (Show, Eq, Ord)


-- Copied from ghc-heap
parseConstrDesc :: String -> ConstrDesc
parseConstrDesc input =
    if not . all (>0) . fmap length $ [p,m,occ]
                     then ConstrDesc "" "" input
                     else ConstrDesc p m occ
  where
    (p, rest1) = break (== ':') input
    (m, occ)
        = (intercalate "." $ reverse modWords, occWord)
        where
        (modWords, occWord) =
            if null rest1 --  XXXXXXXXx YUKX
                --then error "getConDescAddress:parse:length rest1 < 1"
                then parseModOcc [] []
                else parseModOcc [] (tail rest1)
    -- We only look for dots if str could start with a module name,
    -- i.e. if it starts with an upper case character.
    -- Otherwise we might think that "X.:->" is the module name in
    -- "X.:->.+", whereas actually "X" is the module name and
    -- ":->.+" is a constructor name.
    parseModOcc :: [String] -> String -> ([String], String)
    parseModOcc acc str@(c : _)
        | isUpper c =
            case break (== '.') str of
                (top, []) -> (acc, top)
                (top, _:bot) -> parseModOcc (top : acc) bot
    parseModOcc acc str = (acc, str)

class Hextraversable m where
  hextraverse ::
    Applicative f => (a -> f b)
                  -> (c -> f d)
                  -> (e -> f g)
                  -> (h -> f i)
                  -> (j -> f k)
                  -> (l -> f n)
                  ->    m a c e h j l
                  -> f (m b d g i k n)

hexmap :: forall a b c d e f g h i j k l t . Hextraversable t => (a -> b) -> (c -> d) -> (e -> f) -> (g -> h) -> (i -> j) -> (k -> l) -> t a c e g i k -> t b d f h j l
hexmap = coerce
  (hextraverse :: (a -> Identity b)
              -> (c -> Identity d)
              -> (e -> Identity f)
              -> (g -> Identity h)
              -> (i -> Identity j)
              -> (k -> Identity l)
              -> t a c e g i k -> Identity (t b d f h j l))

allClosures :: DebugClosure ccs (GenSrtPayload c) (GenPapPayload c) a (GenStackFrames (GenSrtPayload c) c) c -> [c]
allClosures c = getConst $ hextraverse (const (Const [])) (traverse (Const . (:[]))) (traverse (Const . (:[]))) (const (Const [])) (traverse (Const . (:[]))) (Const . (:[])) c

data FieldValue b = SPtr b
                  | SNonPtr !Word64 deriving (Show, Traversable, Functor, Foldable, Ord, Eq)


instance Hextraversable DebugClosure where
  hextraverse fccs srt p h f g c =
    case c of
      ConstrClosure a1 ph bs ds str ->
        (\ph1 cs cstr -> ConstrClosure a1 ph1 cs ds cstr) <$> (traverse . traverse) fccs ph <*> traverse g bs <*> h str
      FunClosure a1 ph srt_p bs ws -> (\ph1 srt' cs -> FunClosure a1 ph1 srt' cs ws) <$> (traverse . traverse) fccs ph <*> srt srt_p <*> traverse g bs
      ThunkClosure a1 ph srt_p bs ws -> (\ph1 srt' cs -> ThunkClosure a1 ph1 srt' cs ws) <$> (traverse . traverse) fccs ph <*> srt srt_p <*> traverse g bs
      SelectorClosure a1 ph b  -> SelectorClosure a1 <$> (traverse . traverse) fccs ph <*> g b
      PAPClosure a1 a2 a3 a4 a5 a6 -> (\a2 -> PAPClosure a1 a2 a3 a4) <$> (traverse . traverse) fccs a2 <*> g a5 <*> p a6
      APClosure a1 a2 a3 a4 a5 a6 -> (\a2 -> APClosure a1 a2 a3 a4) <$> (traverse . traverse) fccs a2 <*> g a5 <*> p a6
      APStackClosure a1 ph s b bs   -> (\ph -> APStackClosure a1 ph s) <$> (traverse . traverse) fccs ph <*> g b <*> f bs
      IndClosure a1 ph b -> IndClosure a1 <$> (traverse . traverse) fccs ph <*> g b
      BCOClosure a1 ph b1 b2 b3 a2 a3 a4 ->
        (\ph c1 c2 c3 -> BCOClosure a1 ph c1 c2 c3 a2 a3 a4) <$> (traverse . traverse) fccs ph <*> g b1 <*> g b2 <*> g b3
      BlackholeClosure a1 ph b -> BlackholeClosure a1 <$> (traverse . traverse) fccs ph <*> g b
      ArrWordsClosure a1 ph a2 a3 -> (\ph -> ArrWordsClosure a1 ph a2 a3) <$> (traverse . traverse) fccs ph
      MutArrClosure a1 ph a2 a3 bs -> (\ph -> MutArrClosure a1 ph a2 a3) <$> (traverse . traverse) fccs ph <*> traverse g bs
      SmallMutArrClosure a1 ph a2 bs -> (\ph -> SmallMutArrClosure a1 ph a2) <$> (traverse . traverse) fccs ph <*> traverse g bs
      MVarClosure a1 ph b1 b2 b3     -> MVarClosure a1 <$> (traverse . traverse) fccs ph <*> g b1 <*> g b2 <*> g b3
      MutVarClosure a1 ph b -> MutVarClosure a1 <$> (traverse . traverse) fccs ph <*> g b
      BlockingQueueClosure a1 ph b1 b2 b3 b4 ->
        BlockingQueueClosure a1 <$> (traverse . traverse) fccs ph <*> g b1 <*> g b2 <*> g b3 <*> g b4
      TSOClosure a1 ph b1 b2 b3 b4 b5 b6 b7 a2 a3 a4 a5 a6 a7 a8 a9 a10 ->
        (\ph c1 c2 c3 c4 c5 c6 c7 -> TSOClosure a1 ph c1 c2 c3 c4 c5 c6 c7 a2 a3 a4 a5 a6 a7 a8 a9 a10) <$> (traverse . traverse) fccs ph <*> g b1 <*> g b2 <*> g b3 <*> g b4 <*> g b5 <*> g b6 <*> traverse g b7
      StackClosure a1 ph a2 a3 a4 a5 -> (\ph -> StackClosure a1 ph a2 a3 a4) <$> (traverse . traverse) fccs ph <*> f a5
      WeakClosure a1 ph a2 a3 a4 a5 a6 ->
        WeakClosure a1 <$> (traverse . traverse) fccs ph <*> g a2 <*> g a3 <*> g a4 <*> g a5 <*> traverse g a6
      TVarClosure a1 ph a2 a3 a4 ->
        TVarClosure a1 <$> (traverse . traverse) fccs ph <*> g a2 <*> g a3 <*> pure a4
      TRecChunkClosure a1 ph a2 a3 a4 -> TRecChunkClosure a1 <$> (traverse . traverse) fccs ph <*> g a2 <*>  pure a3 <*> traverse (traverse g) a4
      MutPrimClosure a1 ph a2 a3 -> MutPrimClosure a1 <$> (traverse . traverse) fccs ph <*> traverse g a2 <*> pure a3
      OtherClosure a1 ph bs ws -> OtherClosure a1 <$> (traverse . traverse) fccs ph <*> traverse g bs <*> pure ws
      UnsupportedClosure i ph -> UnsupportedClosure i <$> (traverse . traverse) fccs ph
