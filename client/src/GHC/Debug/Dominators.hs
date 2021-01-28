{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GHC.Debug.Dominators (computeDominators
                                   , retainerSize
                                   , convertToHeapGraph
                                   , annotateWithRetainerSize ) where

import Data.Maybe       ( catMaybes, fromJust )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import GHC.Debug.Types.Ptr
import GHC.Debug.Types.Closures
import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as F
import qualified Data.Graph.Dom as DO
import qualified Data.Tree as Tree
import GHC.Debug.Types.Graph



-- Dominators
closurePtrToInt :: ClosurePtr -> Int
closurePtrToInt (ClosurePtr p) = fromIntegral p

intToClosurePtr :: Int -> ClosurePtr
intToClosurePtr i = mkClosurePtr (fromIntegral i)

convertToDom :: HeapGraph a -> DO.Rooted
convertToDom  (HeapGraph groots is) = (0, new_graph)
  where
    rootNodes = IS.fromList (map closurePtrToInt (NE.toList groots))
    new_graph = IM.insert 0 rootNodes (IM.foldlWithKey' collectNodes IM.empty is)
    collectNodes newMap k h =  IM.insert k (IS.fromList (map closurePtrToInt (catMaybes (allClosures (hgeClosure h))))) newMap

computeDominators :: HeapGraph a -> [Tree.Tree (HeapGraphEntry a)]
computeDominators hg = map (fmap (fromJust . flip lookupHeapGraph hg . intToClosurePtr)) gentries
  where
    gentries = case DO.domTree (convertToDom hg) of
                Tree.Node 0 es -> es
                _ -> error "Dominator tree must contain 0"

retainerSize :: HeapGraph Size -> [Tree.Tree (HeapGraphEntry (Size, RetainerSize))]
retainerSize hg = map bottomUpSize doms
  where
    doms = computeDominators hg

annotateWithRetainerSize :: HeapGraph Size -> HeapGraph (Size, RetainerSize)
annotateWithRetainerSize h@(HeapGraph rs _) =
  HeapGraph rs (foldMap convertToHeapGraph (retainerSize h))

bottomUpSize :: Tree.Tree (HeapGraphEntry Size) -> Tree.Tree (HeapGraphEntry (Size, RetainerSize))
bottomUpSize (Tree.Node rl sf) =
  let ts = map bottomUpSize sf
      s'@(Size s) =  hgeData rl
      RetainerSize children_size = foldMap (snd . hgeData . Tree.rootLabel) ts
      inclusive_size :: RetainerSize
      !inclusive_size = RetainerSize  (s + children_size)
      rl' = rl { hgeData = (s', inclusive_size) }
  in Tree.Node rl' ts

convertToHeapGraph ::  Tree.Tree (HeapGraphEntry a) -> IM.IntMap (HeapGraphEntry a)
convertToHeapGraph t = IM.fromList ([(fromIntegral cp, c) | c <- F.toList t, let ClosurePtr cp = hgeClosurePtr c ])



