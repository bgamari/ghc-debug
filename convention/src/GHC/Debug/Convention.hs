
module GHC.Debug.Convention (socketDirectory, snapshotDirectory, ghcDebugDirectory) where

import System.FilePath
import System.Directory

-- | The default socket directory in which to place unix domain sockets in
-- ghc-debug-stub. This is currently your XDG data directory.
ghcDebugDirectory :: IO FilePath
ghcDebugDirectory = do
    xdgDir <- getXdgDirectory XdgData ""
    return (xdgDir </> "ghc-debug/debuggee/")

socketDirectory :: IO FilePath
socketDirectory = (</> "sockets")  <$> ghcDebugDirectory

snapshotDirectory :: IO FilePath
snapshotDirectory = (</> "snapshots")  <$> ghcDebugDirectory
