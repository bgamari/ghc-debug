module Main where

import GHC.Debug.Client


main = withDebuggee "/tmp/ghc-debug" p3

-- Test pause/resume
p1 d = pauseDebuggee d (print "help!")



-- Testing error codes
p2 d = do
  request d RequestPause
  print "req1"
  request d RequestPause
  request d RequestPause
  request d RequestPause

-- Testing get version
p3 d = do
  request d RequestVersion >>= print
  request d RequestPause
  request d RequestResume



