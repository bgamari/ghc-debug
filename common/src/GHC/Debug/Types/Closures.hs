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
    -- * Closures
      Closure
    , SizedClosure
    , DebugClosure(..)
    , DebugClosureWithSize
    , Size(..)
    , InclusiveSize(..)
    , RetainerSize(..)
    , DebugClosureWithExtra(..)
    , TRecEntry(..)
    , noSize
    , dcSize
    , StgInfoTable(..)
    , StgInfoTableWithPtr(..)
    , FieldValue(..)
    , DebugStackFrame(..)
    , GenStackFrames(..)
    , StackFrames
    , GenPapPayload(..)
    , PapPayload
    , PayloadCont(..)
    , GHC.PrimType(..)
    , lookupStgInfoTableWithPtr
    , allClosures
    , Quadtraversable(..)
    , quadmap
    , ConstrDesc(..)
    , ConstrDescCont
    , parseConstrDesc
    ) where

import Prelude -- See note [Why do we import Prelude here?]
-- TODO: Support profiling
--import qualified GHC.Exts.Heap.InfoTableProf as ItblProf
import GHC.Exts.Heap.InfoTable
import qualified GHC.Exts.Heap as GHC
import GHC.Exts.Heap.ProfInfo.Types as ProfTypes


import Data.Functor.Identity
import Data.Int
import Data.Word
import GHC.Exts
import GHC.Generics
import GHC.Debug.Types.Ptr
import Data.List
import Data.Char
import Data.Kind

import Control.Applicative
import Data.Monoid


------------------------------------------------------------------------
-- Closures


type Closure = DebugClosure PayloadCont ConstrDescCont StackCont ClosurePtr
type SizedClosure = DebugClosureWithSize PayloadCont ConstrDescCont StackCont ClosurePtr

type ConstrDescCont = InfoTablePtr

data PayloadCont = PayloadCont ClosurePtr [Word64] deriving Show

type DebugClosureWithSize = DebugClosureWithExtra Size

data DebugClosureWithExtra x pap string s b = DCS { extraDCS :: x
                                              , unDCS :: DebugClosure pap string s b }
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


noSize :: DebugClosureWithSize pap string s b -> DebugClosure pap string s b
noSize = unDCS

dcSize :: DebugClosureWithSize pap string s b -> Size
dcSize = extraDCS

instance Quadtraversable (DebugClosureWithExtra x) where
  quadtraverse f g h i (DCS x v) = DCS x <$> quadtraverse f g h i v

data StgInfoTableWithPtr = StgInfoTableWithPtr {
                              tableId :: InfoTablePtr
                            , decodedTable :: StgInfoTable
                            } deriving (Show)

instance Ord StgInfoTableWithPtr where
  compare t1 t2 = compare (tableId t1) (tableId t2)

instance Eq StgInfoTableWithPtr where
  t1 == t2 = tableId t1 == tableId t2


-- | This is the representation of a Haskell value on the heap. It reflects
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/includes/rts/storage/Closures.h>
--
-- The data type is parametrized by the type to store references in. Usually
-- this is a 'Box' with the type synonym 'Closure'.
--
-- All Heap objects have the same basic layout. A header containing a pointer
-- to the info table and a payload with various fields. The @info@ field below
-- always refers to the info table pointed to by the header. The remaining
-- fields are the payload.
--
-- See
-- <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects>
-- for more information.
data DebugClosure pap string s b
  = -- | A data constructor
    ConstrClosure
        { info       :: !StgInfoTableWithPtr
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        , constrDesc :: !string
        }

    -- | A function
  | FunClosure
        { info       :: !StgInfoTableWithPtr
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk, an expression not obviously in head normal form
  | ThunkClosure
        { info       :: !StgInfoTableWithPtr
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk which performs a simple selection operation
  | SelectorClosure
        { info       :: !StgInfoTableWithPtr
        , selectee   :: !b              -- ^ Pointer to the object being
                                        --   selected from
        }

    -- | An unsaturated function application
  | PAPClosure
        { info       :: !StgInfoTableWithPtr
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
        , arity      :: !HalfWord       -- ^ Always 0
        , n_args     :: !HalfWord       -- ^ Size of payload in words
        , fun        :: !b              -- ^ Pointer to a 'FunClosure'
        , ap_payload    :: !pap            -- ^ Sequence of already applied
                                        --   arguments
        }

    -- | A suspended thunk evaluation
  | APStackClosure
        { info       :: !StgInfoTableWithPtr
        , ap_st_size :: !Word
        , fun        :: !b              -- ^ Function closure
        , payload    :: !s            -- ^ Stack right before suspension
        }

    -- | A pointer to another closure, introduced when a thunk is updated
    -- to point at its value
  | IndClosure
        { info       :: !StgInfoTableWithPtr
        , indirectee :: !b              -- ^ Target closure
        }

   -- | A byte-code object (BCO) which can be interpreted by GHC's byte-code
   -- interpreter (e.g. as used by GHCi)
  | BCOClosure
        { info       :: !StgInfoTableWithPtr
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
        , indirectee :: !b              -- ^ The target closure
        }

    -- | A @ByteArray#@
  | ArrWordsClosure
        { info       :: !StgInfoTableWithPtr
        , bytes      :: !Word           -- ^ Size of array in bytes
        , arrWords   :: ![Word]         -- ^ Array payload
        }

    -- | A @MutableByteArray#@
  | MutArrClosure
        { info       :: !StgInfoTableWithPtr
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
        , mccPtrs    :: !Word           -- ^ Number of pointers
        , mccPayload :: ![b]            -- ^ Array payload
        }

    -- | An @MVar#@, with a queue of thread state objects blocking on them
  | MVarClosure
        { info       :: !StgInfoTableWithPtr
        , queueHead  :: !b              -- ^ Pointer to head of queue
        , queueTail  :: !b              -- ^ Pointer to tail of queue
        , value      :: !b              -- ^ Pointer to closure
        }

    -- | A @MutVar#@
  | MutVarClosure
        { info       :: !StgInfoTableWithPtr
        , var        :: !b              -- ^ Pointer to contents
        }

    -- | An STM blocking queue.
  | BlockingQueueClosure
        { info       :: !StgInfoTableWithPtr
        , link       :: !b              -- ^ ?? Here so it looks like an IND
        , blackHole  :: !b              -- ^ The blackhole closure
        , owner      :: !b              -- ^ The owning thread state object
        , queue      :: !b              -- ^ ??
        }

  | TSOClosure
      { info :: !StgInfoTableWithPtr
      -- pointers
      , _link :: !b
      , global_link :: !b
      , tsoStack :: !b -- ^ stackobj from StgTSO
      , trec :: !b
      , blocked_exceptions :: !b
      , bq :: !b
      -- values
      , what_next :: GHC.WhatNext
      , why_blocked :: GHC.WhyBlocked
      , flags :: [GHC.TsoFlags]
      , threadId :: Word64
      , saved_errno :: Word32
      , dirty:: Word32
      , alloc_limit :: Int64
      , tot_stack_size :: Word32
      , prof :: Maybe ProfTypes.StgTSOProfInfo
      }

 | StackClosure
     { info :: !StgInfoTableWithPtr
     , stack_size :: !Word32 -- ^ stack size in *words*
     , stack_dirty :: !Word8 -- ^ non-zero => dirty
     , stack_marking :: !Word8
     , frames :: s
     }


  | WeakClosure
     { info        :: !StgInfoTableWithPtr
     , cfinalizers :: !b
     , key         :: !b
     , value       :: !b
     , finalizer   :: !b
     , mlink       :: !(Maybe b) -- ^ next weak pointer for the capability, can be NULL.
     }

  | TVarClosure
    { info :: !StgInfoTableWithPtr
    , current_value :: !b
    , tvar_watch_queue :: !b
    , num_updates :: !Int }

  | TRecChunkClosure
    { info :: !StgInfoTableWithPtr
    , prev_chunk  :: !b
    , next_idx :: !Word
    , entries :: ![TRecEntry b]
    }

  | MutPrimClosure
    { info :: !StgInfoTableWithPtr
    , ptrArgs :: ![b]
    , dataArgs :: ![Word]
    }

    -----------------------------------------------------------
    -- Anything else

    -- | Another kind of closure
  | OtherClosure
        { info       :: !StgInfoTableWithPtr
        , hvalues    :: ![b]
        , rawWords   :: ![Word]
        }

  | UnsupportedClosure
        { info       :: !StgInfoTableWithPtr
        }
  deriving (Show, Generic, Functor, Foldable, Traversable, Ord, Eq)

deriving instance Ord GHC.WhatNext
deriving instance Ord GHC.WhyBlocked
deriving instance Ord GHC.TsoFlags
deriving instance Ord CostCentreStack
deriving instance Ord CostCentre
deriving instance Ord IndexTable
deriving instance Ord StgTSOProfInfo
deriving instance Eq StgTSOProfInfo

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

type StackFrames = GenStackFrames ClosurePtr
newtype GenStackFrames b = GenStackFrames { getFrames :: [DebugStackFrame b] }
  deriving (Functor, Foldable, Traversable, Show, Ord, Eq)

data DebugStackFrame b
  = DebugStackFrame
        { frame_info :: !StgInfoTableWithPtr
        , values     :: [FieldValue b]
        } deriving (Traversable, Functor, Foldable, Show, Ord, Eq)



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

class Quadtraversable m where
  quadtraverse ::
    Applicative f => (a -> f b)
                  -> (c -> f d)
                  -> (e -> f g)
                  -> (h -> f i)
                  -> m a c e h
                  -> f (m b d g i)

quadmap :: forall a b c d e f g h t . Quadtraversable t => (a -> b) -> (c -> d) -> (e -> f) -> (g -> h) -> t a c e g -> t b d f h
quadmap = coerce
  (quadtraverse :: (a -> Identity b)
              -> (c -> Identity d)
              -> (e -> Identity f)
              -> (g -> Identity h)
              -> t a c e g -> Identity (t b d f h))

allClosures :: DebugClosure (GenPapPayload c) a (GenStackFrames c) c -> [c]
allClosures c = getConst $ quadtraverse (traverse (Const . (:[]))) (const (Const [])) (traverse (Const . (:[]))) (Const . (:[])) c

data FieldValue b = SPtr b
                  | SNonPtr !Word64 deriving (Show, Traversable, Functor, Foldable, Ord, Eq)


instance Quadtraversable DebugClosure where
  quadtraverse p h f g c =
    case c of
      ConstrClosure a1 bs ds str ->
        (\cs cstr -> ConstrClosure a1 cs ds cstr) <$> traverse g bs <*> h str
      FunClosure a1 bs ws -> (\cs -> FunClosure a1 cs ws) <$> traverse g bs
      ThunkClosure a1 bs ws -> (\cs -> ThunkClosure a1 cs ws) <$> traverse g bs
      SelectorClosure a1 b  -> SelectorClosure a1 <$> g b
      PAPClosure a1 a2 a3 a4 a5 -> PAPClosure a1 a2 a3 <$> g a4 <*> p a5
      APClosure a1 a2 a3 a4 a5 -> APClosure a1 a2 a3 <$> g a4 <*> p a5
      APStackClosure a1 s b bs   -> APStackClosure a1 s <$> g b <*> f bs
      IndClosure a1 b -> IndClosure a1 <$> g b
      BCOClosure a1 b1 b2 b3 a2 a3 a4 ->
        (\c1 c2 c3 -> BCOClosure a1 c1 c2 c3 a2 a3 a4) <$> g b1 <*> g b2 <*> g b3
      BlackholeClosure a1 b -> BlackholeClosure a1 <$> g b
      ArrWordsClosure a1 a2 a3 -> pure (ArrWordsClosure a1 a2 a3)
      MutArrClosure a1 a2 a3 bs -> MutArrClosure a1 a2 a3 <$> traverse g bs
      SmallMutArrClosure a1 a2 bs -> SmallMutArrClosure a1 a2 <$> traverse g bs
      MVarClosure a1 b1 b2 b3     -> MVarClosure a1 <$> g b1 <*> g b2 <*> g b3
      MutVarClosure a1 b -> MutVarClosure a1 <$> g b
      BlockingQueueClosure a1 b1 b2 b3 b4 ->
        BlockingQueueClosure a1 <$> g b1 <*> g b2 <*> g b3 <*> g b4
      TSOClosure a1 b1 b2 b3 b4 b5 b6 a2 a3 a4 a5 a6 a7 a8 a9 a10 ->
        (\c1 c2 c3 c4 c5 c6 -> TSOClosure a1 c1 c2 c3 c4 c5 c6 a2 a3 a4 a5 a6 a7 a8 a9 a10) <$> g b1 <*> g b2 <*> g b3 <*> g b4 <*> g b5 <*> g b6
      StackClosure a1 a2 a3 a4 a5 -> StackClosure a1 a2 a3 a4 <$> f a5
      WeakClosure a1 a2 a3 a4 a5 a6 ->
        WeakClosure a1 <$> g a2 <*> g a3 <*> g a4 <*> g a5 <*> traverse g a6
      TVarClosure a1 a2 a3 a4 ->
        TVarClosure a1 <$> g a2 <*> g a3 <*> pure a4
      TRecChunkClosure a1 a2 a3 a4 -> TRecChunkClosure a1 <$> g a2 <*>  pure a3 <*> traverse (traverse g) a4
      MutPrimClosure a1 a2 a3 -> MutPrimClosure a1 <$> traverse g a2 <*> pure a3
      OtherClosure a1 bs ws -> OtherClosure a1 <$> traverse g bs <*> pure ws
      UnsupportedClosure i  -> pure (UnsupportedClosure i)

lookupStgInfoTableWithPtr :: DebugClosure pap string s b -> StgInfoTableWithPtr
lookupStgInfoTableWithPtr dc = info dc
