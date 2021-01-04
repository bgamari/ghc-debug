module GHC.Debug.Convention (socketDirectory) where

import System.FilePath
import System.Directory

-- | The default socket directory in which to place unix domain sockets in
-- ghc-debug-stub.
socketDirectory :: IO FilePath
socketDirectory = do
    xdgDir <- getXdgDirectory XdgData ""
    return (xdgDir </> "ghc-debug/debuggee/")
