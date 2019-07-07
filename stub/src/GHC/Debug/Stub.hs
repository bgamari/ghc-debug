module GHC.Debug.Stub (pause, resume, start) where

-- | Start listening for remote debugging
foreign import ccall safe "start"
    start :: IO ()

-- | Break program execution for debugging.
foreign import ccall safe "pause_mutator"
    pause :: IO ()

-- | Resume program execution for debugging.
foreign import ccall safe "resume_mutator"
    resume :: IO ()

