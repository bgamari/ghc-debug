module Main where

import GHC.Debug.Client

main = withDebuggee "/tmp/ghc-debug" (\_ -> return ())



