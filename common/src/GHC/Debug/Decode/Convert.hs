{- Convert a GenClosure to a DebugClosure -}
module GHC.Debug.Decode.Convert where


import qualified GHC.Exts.Heap as GHC
import qualified GHC.Exts.Heap.InfoTable as Itbl
import qualified GHC.Exts.Heap.InfoTableProf as ItblProf

import GHC.Debug.Types

-- | Convert a GenClosure from ghc-heap to a DebugClosure,
-- it is mostly an identity function, apart from STACK closures.
convertClosure :: (Num a, Eq a) => GHC.GenClosure a -> DebugClosure () a a
convertClosure  g =
  case g of
    -- The () here will be overwritten by a constant value in
    -- `decodeClosure`
    GHC.ConstrClosure a1 a2 a3 a4 a5 a6 -> ConstrClosure a1 a2 a3 ()
    GHC.FunClosure a1 a2 a3             -> FunClosure a1 a2 a3
    GHC.ThunkClosure a1 a2 a3           -> ThunkClosure a1 a2 a3
    GHC.SelectorClosure a1 a2           -> SelectorClosure a1 a2
    GHC.PAPClosure a1 a2 a3 a4 a5       -> PAPClosure a1 a2 a3 a4 a5
    GHC.APClosure a1 a2 a3 a4 a5        -> APClosure a1 a2 a3 a4 a5
    GHC.APStackClosure a1 a2 a3         -> APStackClosure a1 a2 a3
    GHC.IndClosure a1 a2                -> IndClosure a1 a2
    GHC.BCOClosure a1 a2 a3 a4 a5 a6 a7 -> BCOClosure a1 a2 a3 a4 a5 a6 a7
    GHC.BlackholeClosure a1 a2          -> BlackholeClosure a1 a2
    GHC.ArrWordsClosure a1 a2 a3        -> ArrWordsClosure a1 a2 a3
    GHC.MutArrClosure a1 a2 a3 a4       -> MutArrClosure a1 a2 a3 a4
    GHC.SmallMutArrClosure a1 a2 a3     -> SmallMutArrClosure a1 a2 a3
    GHC.MVarClosure a1 a2 a3 a4         -> MVarClosure a1 a2 a3 a4
    GHC.MutVarClosure a1 a2             -> MutVarClosure a1 a2
    GHC.BlockingQueueClosure a1 a2 a3 a4 a5 -> BlockingQueueClosure a1 a2 a3 a4 a5
    GHC.TSOClosure a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 -> TSOClosure a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16
    GHC.StackClosure a1 a2 a3 a4        -> StackClosure a1 a2 a3 a4
    GHC.IntClosure a1 a2                -> IntClosure a1 a2
    GHC.WordClosure a1 a2               -> WordClosure a1 a2
    GHC.Int64Closure a1 a2              -> Int64Closure a1 a2
    GHC.Word64Closure a1 a2             -> Word64Closure a1 a2
    GHC.AddrClosure a1 a2               -> AddrClosure a1 a2
    GHC.FloatClosure a1 a2              -> FloatClosure a1 a2
    GHC.DoubleClosure a1 a2             -> DoubleClosure a1 a2
    GHC.OtherClosure a1 a2 a3           -> OtherClosure a1 a2 a3
    GHC.WeakClosure a1 a2 a3 a4 a5 a6   ->
      -- nullPtr check
      let link = if a6 == 0
                  then Nothing
                  else Just a6
      in WeakClosure a1 a2 a3 a4 a5 link
    GHC.UnsupportedClosure a1           -> UnsupportedClosure a1
