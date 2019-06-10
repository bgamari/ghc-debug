module GHC.Debug.Stub (pause) where

-- | Break program execution for debugging.
foreign import safe ccall "pause_mutators"
    pause :: IO ()

