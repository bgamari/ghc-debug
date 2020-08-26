{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- This module is mostly a copy of GHC.Exts.Heap.Closures but with
- additional support for STACK closures which are only possible to decode
- out of process
-}
module GHC.Debug.Types.Closures (
    -- * Closures
      Closure
    , DebugClosure(..)
    , StgInfoTable(..)
    , FieldValue(..)
    , DebugStackFrame(..)
    , Stack
    , GHC.PrimType(..)
    , allClosures
    , Fix1(..)
    , Fix2(..)
    , UClosure
    , UStack
    , Tritraversable(..)
    , trimap
    , ConstrDesc(..)
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


data Fix1 (string :: *) (f :: * -> *) (g :: * -> * -> * -> *) =
  MkFix1 (g string (Fix2 string f g) (Fix1 string f g))
data Fix2 s f g = MkFix2 (f (Fix1 s f g))

instance Show (g string (Fix2 string f g) (Fix1 string f g)) => Show (Fix1 string f g) where
        showsPrec n (MkFix1 x) = showParen (n > 10) $ \s ->
                "Fix1 " ++ showsPrec 11 x s

instance Show (f (Fix1 string f g)) => Show (Fix2 string f g) where
        showsPrec n (MkFix2 x) = showParen (n > 10) $ \s ->
                "Fix2 " ++ showsPrec 11 x s

type UClosure = Fix1 ConstrDesc DebugStackFrame DebugClosure
type UStack   = Fix2 ConstrDesc DebugStackFrame DebugClosure

------------------------------------------------------------------------
-- Closures

type Closure = DebugClosure ClosurePtr StackCont ClosurePtr

type Stack = DebugStackFrame ClosurePtr

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
data DebugClosure string s b
  = -- | A data constructor
    ConstrClosure
        { info       :: !StgInfoTable
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        , constrDesc :: !string
        }

    -- | A function
  | FunClosure
        { info       :: !StgInfoTable
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk, an expression not obviously in head normal form
  | ThunkClosure
        { info       :: !StgInfoTable
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk which performs a simple selection operation
  | SelectorClosure
        { info       :: !StgInfoTable
        , selectee   :: !b              -- ^ Pointer to the object being
                                        --   selected from
        }

    -- | An unsaturated function application
  | PAPClosure
        { info       :: !StgInfoTable
        , arity      :: !HalfWord       -- ^ Arity of the partial application
        , n_args     :: !HalfWord       -- ^ Size of the payload in words
        , fun        :: !b              -- ^ Pointer to a 'FunClosure'
        , payload    :: ![b]            -- ^ Sequence of already applied
                                        --   arguments
        }

    -- In GHCi, if Linker.h would allow a reverse lookup, we could for exported
    -- functions fun actually find the name here.
    -- At least the other direction works via "lookupSymbol
    -- base_GHCziBase_zpzp_closure" and yields the same address (up to tags)
    -- | A function application
  | APClosure
        { info       :: !StgInfoTable
        , arity      :: !HalfWord       -- ^ Always 0
        , n_args     :: !HalfWord       -- ^ Size of payload in words
        , fun        :: !b              -- ^ Pointer to a 'FunClosure'
        , payload    :: ![b]            -- ^ Sequence of already applied
                                        --   arguments
        }

    -- | A suspended thunk evaluation
  | APStackClosure
        { info       :: !StgInfoTable
        , fun        :: !b              -- ^ Function closure
        , payload    :: ![b]            -- ^ Stack right before suspension
        }

    -- | A pointer to another closure, introduced when a thunk is updated
    -- to point at its value
  | IndClosure
        { info       :: !StgInfoTable
        , indirectee :: !b              -- ^ Target closure
        }

   -- | A byte-code object (BCO) which can be interpreted by GHC's byte-code
   -- interpreter (e.g. as used by GHCi)
  | BCOClosure
        { info       :: !StgInfoTable
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
        { info       :: !StgInfoTable
        , indirectee :: !b              -- ^ The target closure
        }

    -- | A @ByteArray#@
  | ArrWordsClosure
        { info       :: !StgInfoTable
        , bytes      :: !Word           -- ^ Size of array in bytes
        , arrWords   :: ![Word]         -- ^ Array payload
        }

    -- | A @MutableByteArray#@
  | MutArrClosure
        { info       :: !StgInfoTable
        , mccPtrs    :: !Word           -- ^ Number of pointers
        , mccSize    :: !Word           -- ^ ?? Closures.h vs ClosureMacros.h
        , mccPayload :: ![b]            -- ^ Array payload
        -- Card table ignored
        }

    -- | A @SmallMutableArray#@
    --
    -- @since 8.10.1
  | SmallMutArrClosure
        { info       :: !StgInfoTable
        , mccPtrs    :: !Word           -- ^ Number of pointers
        , mccPayload :: ![b]            -- ^ Array payload
        }

    -- | An @MVar#@, with a queue of thread state objects blocking on them
  | MVarClosure
        { info       :: !StgInfoTable
        , queueHead  :: !b              -- ^ Pointer to head of queue
        , queueTail  :: !b              -- ^ Pointer to tail of queue
        , value      :: !b              -- ^ Pointer to closure
        }

    -- | A @MutVar#@
  | MutVarClosure
        { info       :: !StgInfoTable
        , var        :: !b              -- ^ Pointer to contents
        }

    -- | An STM blocking queue.
  | BlockingQueueClosure
        { info       :: !StgInfoTable
        , link       :: !b              -- ^ ?? Here so it looks like an IND
        , blackHole  :: !b              -- ^ The blackhole closure
        , owner      :: !b              -- ^ The owning thread state object
        , queue      :: !b              -- ^ ??
        }

  | TSOClosure
      { info :: !StgInfoTable
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
     { info :: !StgInfoTable
     , size :: !Word32 -- ^ stack size in *words*
     , stack_dirty :: !Word8 -- ^ non-zero => dirty
     , stack_marking :: Word8
     , stackPointer :: !s -- ^ current stack pointer
     , stack :: [Word]
     }

  | WeakClosure
     { info        :: !StgInfoTable
     , cfinalizers :: !b
     , key         :: !b
     , value       :: !b
     , finalizer   :: !b
     , mlink       :: !(Maybe b) -- ^ next weak pointer for the capability, can be NULL.
     }


    ------------------------------------------------------------
    -- Unboxed unlifted closures

    -- | Primitive Int
  | IntClosure
        { ptipe      :: GHC.PrimType
        , intVal     :: !Int }

    -- | Primitive Word
  | WordClosure
        { ptipe      :: GHC.PrimType
        , wordVal    :: !Word }

    -- | Primitive Int64
  | Int64Closure
        { ptipe      :: GHC.PrimType
        , int64Val   :: !Int64 }

    -- | Primitive Word64
  | Word64Closure
        { ptipe      :: GHC.PrimType
        , word64Val  :: !Word64 }

    -- | Primitive Addr
  | AddrClosure
        { ptipe      :: GHC.PrimType
        , addrVal    :: !Int }

    -- | Primitive Float
  | FloatClosure
        { ptipe      :: GHC.PrimType
        , floatVal   :: !Float }

    -- | Primitive Double
  | DoubleClosure
        { ptipe      :: GHC.PrimType
        , doubleVal  :: !Double }

    -----------------------------------------------------------
    -- Anything else

    -- | Another kind of closure
  | OtherClosure
        { info       :: !StgInfoTable
        , hvalues    :: ![b]
        , rawWords   :: ![Word]
        }

  | UnsupportedClosure
        { info       :: !StgInfoTable
        }
  deriving (Show, Generic, Functor, Foldable, Traversable)

data DebugStackFrame b
  = DebugStackFrame
        { frame_info :: !StgInfoTable
        , values     :: [FieldValue b]
        } deriving (Traversable, Functor, Foldable, Show)

data ConstrDesc = ConstrDesc {
          pkg        :: !String         -- ^ Package name
        , modl       :: !String         -- ^ Module name
        , name       :: !String         -- ^ Constructor name
        } deriving (Show, Eq)


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

class Tritraversable m where
  tritraverse ::
    Applicative f => (a -> f b) -> (c -> f d) -> (e -> f g) -> m a c e -> f (m b d g)

trimap :: forall a b c d e f t . Tritraversable t => (a -> b) -> (c -> d) -> (e -> f) -> t a c e -> t b d f
trimap = coerce
  (tritraverse :: (a -> Identity b)
              -> (c -> Identity d)
              -> (e -> Identity f) -> t a c e -> Identity (t b d f))


data FieldValue b = SPtr b
                  | SNonPtr !Word64 deriving (Show, Traversable, Functor, Foldable)


instance Tritraversable DebugClosure where
  tritraverse h f g c =
    case c of
      ConstrClosure a1 bs ds str ->
        (\cs str -> ConstrClosure a1 cs ds str) <$> traverse g bs <*> h str
      FunClosure a1 bs ws -> (\cs -> FunClosure a1 cs ws) <$> traverse g bs
      ThunkClosure a1 bs ws -> (\cs -> ThunkClosure a1 cs ws) <$> traverse g bs
      SelectorClosure a1 b  -> SelectorClosure a1 <$> g b
      PAPClosure a1 a2 a3 b bs -> PAPClosure a1 a2 a3 <$> g b <*> traverse g bs
      APClosure a1 a2 a3 b bs  -> APClosure a1 a2 a3 <$> g b <*> traverse g bs
      APStackClosure a1 b bs   -> APStackClosure a1 <$> g b <*> traverse g bs
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
      StackClosure a1 a2 a3 a4 s1 ss -> StackClosure a1 a2 a3 a4 <$> f s1 <*> pure ss
      WeakClosure a1 a2 a3 a4 a5 a6 ->
        WeakClosure a1 <$> g a2 <*> g a3 <*> g a4 <*> g a5 <*> traverse g a6
      IntClosure p i -> pure (IntClosure p i)
      WordClosure p i -> pure (WordClosure p i)
      Int64Closure p i -> pure (Int64Closure p i)
      Word64Closure p i -> pure (Word64Closure p i)
      AddrClosure p i -> pure (AddrClosure p i)
      FloatClosure p i -> pure (FloatClosure p i)
      DoubleClosure p i -> pure (DoubleClosure p i)
      OtherClosure a1 bs ws -> OtherClosure a1 <$> traverse g bs <*> pure ws
      UnsupportedClosure i  -> pure (UnsupportedClosure i)

{-
instance Bifunctor DebugClosure where
  bimap = bimapDefault

instance Bifoldable DebugClosure where
  bifoldMap = bifoldMapDefault
-}




-- | For generic code, this function returns all referenced closures.
allClosures :: DebugClosure str sta b -> [b]
allClosures ConstrClosure {..} = ptrArgs
allClosures ThunkClosure {..} = ptrArgs
allClosures SelectorClosure {..} = [selectee]
allClosures IndClosure {..} = [indirectee]
allClosures BlackholeClosure {..} = [indirectee]
allClosures APClosure {..} = fun:payload
allClosures PAPClosure {..} = fun:payload
allClosures APStackClosure {..} = fun:payload
allClosures BCOClosure {..} = [instrs,literals,bcoptrs]
allClosures ArrWordsClosure {} = []
allClosures MutArrClosure {..} = mccPayload
allClosures SmallMutArrClosure {..} = mccPayload
allClosures MutVarClosure {..} = [var]
allClosures MVarClosure {..} = [queueHead,queueTail,value]
allClosures FunClosure {..} = ptrArgs
allClosures BlockingQueueClosure {..} = [link, blackHole, owner, queue]
allClosures OtherClosure {..} = hvalues
allClosures _ = []
