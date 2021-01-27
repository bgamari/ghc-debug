-- | Functions for creating and running snapshots.
module GHC.Debug.Snapshot ( -- * Generating snapshots
                            snapshot
                          , makeSnapshot
                          -- * Using a snapshot
                          , snapshotRun
                          -- * Low level
                          , traceFrom ) where

import GHC.Debug.Trace
import GHC.Debug.ParTrace
import GHC.Debug.Client.Monad
import GHC.Debug.Client
import Control.Monad.Identity
import Control.Monad.Trans

-- | Make a snapshot of the current heap and save it to the given file.
snapshot :: FilePath -> DebugM ()
snapshot fp = do
  precacheBlocks
  rs <- gcRoots
  _so <- savedObjects
  tracePar rs
  saveCache fp

-- | Traverse the tree from GC roots, to populate the caches
-- with everything necessary.
traceFrom :: [ClosurePtr] -> DebugM ()
traceFrom cps = runIdentityT (traceFromM funcs cps)
  where
    nop = const (return ())
    funcs = TraceFunctions nop nop clos (const (return ())) nop

    clos :: ClosurePtr -> SizedClosure -> (IdentityT DebugM) ()
              ->  (IdentityT DebugM) ()
    clos _cp sc k = do
      let itb = info (noSize sc)
      _traced <- lift $ getSourceInfo (tableId itb)
      k

-- | Pause the process and create a snapshot of
-- the heap. The snapshot can then be loaded with
-- 'snapshotRun' in order to perform offline analysis.
makeSnapshot :: Debuggee -> FilePath -> IO ()
makeSnapshot e fp = runAnalysis (snapshot fp) (const (return ())) e

