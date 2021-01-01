{- Convert a GenClosure to a DebugClosure -}
module GHC.Debug.Decode.Convert where


import qualified GHC.Exts.Heap as GHC
--import qualified GHC.Exts.Heap.InfoTable as Itbl
--import qualified GHC.Exts.Heap.InfoTableProf as ItblProf

import GHC.Debug.Types.Closures
import GHC.Debug.Types.Ptr
import System.Endian
import Data.Word
import Data.Void

-- | Convert a GenClosure from ghc-heap to a DebugClosure,
-- it is mostly an identity function, apart from STACK closures.
convertClosure :: (Num a, Eq a) => StgInfoTableWithPtr -> GHC.GenClosure a -> DebugClosure Void InfoTablePtr Void a
convertClosure itb g =
  case g of
    GHC.ConstrClosure _ a2 a3 _ _ _ -> ConstrClosure itb a2 a3 (tableId itb)
    GHC.FunClosure _ a2 a3             -> FunClosure itb a2 a3
    GHC.ThunkClosure _ a2 a3           -> ThunkClosure itb a2 a3
    GHC.SelectorClosure _ a2           -> SelectorClosure itb a2
--    GHC.PAPClosure _ a2 a3 a4 a5       -> PAPClosure itb a2 a3 a4 a5
--    GHC.APClosure _ a2 a3 a4 a5        -> APClosure itb a2 a3 a4 a5
--    GHC.APStackClosure _ a2 a3         -> APStackClosure itb a2
    GHC.IndClosure _ a2                -> IndClosure itb a2
    GHC.BCOClosure _ a2 a3 a4 a5 a6 a7 -> BCOClosure itb a2 a3 a4 a5 a6 a7
    GHC.BlackholeClosure _ a2          -> BlackholeClosure itb a2
    GHC.ArrWordsClosure _ a2 a3        -> ArrWordsClosure itb a2 a3
    GHC.MutArrClosure _ a2 a3 a4       -> MutArrClosure itb a2 a3 a4
    GHC.SmallMutArrClosure _ a2 a3     -> SmallMutArrClosure itb a2 a3
    GHC.MVarClosure _ a2 a3 a4         -> MVarClosure itb a2 a3 a4
    GHC.MutVarClosure _ a2             -> MutVarClosure itb a2
    GHC.BlockingQueueClosure _ a2 a3 a4 a5 -> BlockingQueueClosure itb a2 a3 a4 a5
    GHC.TSOClosure _ a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 -> TSOClosure itb a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16
--    GHC.StackClosure _ a2 a3 a4 a5      -> StackClosure itb a2 a3 a4 (a2, (StackPtr (toBE64 a5)))
    GHC.IntClosure a1 a2                -> IntClosure a1 a2
    GHC.WordClosure a1 a2               -> WordClosure a1 a2
    GHC.Int64Closure a1 a2              -> Int64Closure a1 a2
    GHC.Word64Closure a1 a2             -> Word64Closure a1 a2
    GHC.AddrClosure a1 a2               -> AddrClosure a1 a2
    GHC.FloatClosure a1 a2              -> FloatClosure a1 a2
    GHC.DoubleClosure a1 a2             -> DoubleClosure a1 a2
    GHC.OtherClosure _ a2 a3           -> OtherClosure itb a2 a3
    GHC.WeakClosure _ a2 a3 a4 a5 a6   ->
      -- nullPtr check
      let w_link = if a6 == 0
                  then Nothing
                  else Just a6
      in WeakClosure itb a2 a3 a4 a5 w_link
    GHC.UnsupportedClosure _           -> UnsupportedClosure itb
