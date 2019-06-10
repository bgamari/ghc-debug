module GHC.Debug.Stub (pause) where

-- | Break program execution for debugging.
foreign import ccall safe "pause_mutator"
    pause :: IO ()

