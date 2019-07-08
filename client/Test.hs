module Main where

import GHC.Debug.Client

import Control.Monad


main = withDebuggee "/tmp/ghc-debug" p1

-- Test pause/resume
p1 d = pauseDebuggee d (void $ getChar)



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

p4 d = do
  request d RequestPause
  request d RequestRoots >>= print




