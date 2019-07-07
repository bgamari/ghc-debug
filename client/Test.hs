module Main where

import GHC.Debug.Client

main2 = withDebuggee "/tmp/ghc-debug"
        (\d ->  pauseDebuggee d (print "help!") )



main = withDebuggee "/tmp/ghc-debug"
        (\d ->  do
                  request d RequestPause
                  print "req1"
                  request d RequestPause
                  request d RequestPause
                  request d RequestPause
                )



