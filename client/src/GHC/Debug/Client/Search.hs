module GHC.Debug.Client.Search(module GHC.Debug.Client.Search, HeapGraph(..), HeapGraphEntry(..)) where

import GHC.Debug.Types
import GHC.Debug.Types.Graph
import qualified Data.IntMap as IM

-- Find all entries in the HeapGraph matching a specific predicate
findClosures :: (HeapGraphEntry a -> Bool) -> HeapGraph a -> [HeapGraphEntry a]
findClosures f = go
  where
    go (HeapGraph _ gs) =
      IM.foldl' (\hges hge -> if f hge then hge:hges else hges) [] gs

findConstructors :: String -> HeapGraph a -> [HeapGraphEntry a]
findConstructors con_name hg = findClosures predicate hg
    where
      predicate h = checkConstrTable (hgeClosure $ h)

      checkConstrTable (ConstrClosure _ _ _ (ConstrDesc _ _ n)) = n == con_name
      checkConstrTable _ = False

findWithInfoTable :: InfoTablePtr -> HeapGraph a -> [HeapGraphEntry a]
findWithInfoTable itp = findClosures p
  where
    p = (itp ==) . tableId . info . hgeClosure
