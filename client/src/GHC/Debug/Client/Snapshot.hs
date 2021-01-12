-- | Functions for creating and running a snapshot.
module GHC.Debug.Client.Snapshot ( makeSnapshot, snapshotRun ) where

import GHC.Debug.Client.Trace
import GHC.Debug.Client.Monad
import GHC.Debug.Client.Query

-- | Pause the process and create a snapshot of
-- the heap. The snapshot can then be loaded with
-- 'snapshotRun' in order to perform offline analysis.
makeSnapshot :: Debuggee -> FilePath -> IO ()
makeSnapshot e fp = do
  pause e
  runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    traceFrom rs
    saveCache fp
  resume e

snapshotRun :: FilePath -> (Debuggee -> IO a) -> IO a
snapshotRun fp k = do
  env <- snapshotInit fp
  k env
