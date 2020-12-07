module GHC.Debug.Client.Search where

import GHC.Debug.Types
import GHC.Debug.Types.Graph
import qualified Data.IntMap as IM

-- Find all entries in the HeapGraph matching a specific predicate
findClosures :: (HeapGraphEntry a -> Bool) -> HeapGraph a -> [HeapGraphEntry a]
findClosures f = go
  where
    go (HeapGraph _ gs) =
      IM.foldl' (\hges hge -> if f hge then hge:hges else hges) [] gs

findWithInfoTable :: InfoTablePtr -> HeapGraph a -> [HeapGraphEntry a]
findWithInfoTable itp = findClosures p
  where
    p = (itp ==) . tableId . info . hgeClosure
