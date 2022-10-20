{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

{- Convert a GenClosure to a DebugClosure -}
module GHC.Debug.Decode.Convert where

import qualified GHC.Exts.Heap as GHC

import GHC.Debug.Types.Closures
import GHC.Debug.Types.Ptr
import Data.Void

-- | Convert a GenClosure from ghc-heap to a 'DebugClosure'.
--
-- N.B. This only handles cases not already handled by
-- 'GHC.Debug.Decode.decodeClosure'. Eventually this codepath should be
-- retired.
convertClosure :: (Num a, Eq a, Show a) => StgInfoTableWithPtr -> GHC.GenClosure a -> DebugClosure Void InfoTablePtr Void a
convertClosure itb g =
  case g of
    -- N.B. decodeClosure doesn't handle THUNK_STATIC
    GHC.ThunkClosure _ a2 a3           -> ThunkClosure itb a2 a3
    GHC.SelectorClosure _ a2           -> SelectorClosure itb a2
    GHC.BCOClosure _ a2 a3 a4 a5 a6 a7 -> BCOClosure itb a2 a3 a4 a5 a6 a7
    GHC.BlackholeClosure _ a2          -> BlackholeClosure itb a2
    GHC.MutArrClosure _ a2 a3 a4       -> MutArrClosure itb a2 a3 a4
    GHC.SmallMutArrClosure _ a2 a3     -> SmallMutArrClosure itb a2 a3
    GHC.MVarClosure _ a2 a3 a4         -> MVarClosure itb a2 a3 a4
    GHC.OtherClosure _ a2 a3           -> OtherClosure itb a2 a3
    GHC.WeakClosure _ a2 a3 a4 a5 a6   ->
#if MIN_VERSION_GLASGOW_HASKELL(9,4,2,0)
      let w_link = a6
#else
      -- nullPtr check
      let w_link = if a6 == 0
                  then Nothing
                  else Just a6
#endif
      in WeakClosure itb a2 a3 a4 a5 w_link
    GHC.UnsupportedClosure _           -> UnsupportedClosure itb
    c -> error ("Unexpected closure type: " ++ show c)
