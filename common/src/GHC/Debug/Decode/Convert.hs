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
convertClosure :: (Num a, Eq a, Show a) => StgInfoTableWithPtr -> GHC.GenClosure a -> DebugClosure CCSPtr InfoTablePtr Void InfoTablePtr Void a
convertClosure itb g =
  case g of
    -- N.B. decodeClosure doesn't handle THUNK_STATIC
    GHC.OtherClosure _ a2 a3           -> OtherClosure itb Nothing a2 a3
    GHC.UnsupportedClosure _           -> UnsupportedClosure itb Nothing
    c -> error ("Unexpected closure type: " ++ show c)
