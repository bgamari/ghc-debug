{-# LANGUAGE BangPatterns #-}

module Main where

import GHC.Debug.Stub
import Control.Concurrent
import System.Mem.StableName
import Foreign.StablePtr

loop :: IO ()
loop = go 0
  where
   go x = print x >> threadDelay 1000000 >> go (x + 1)

data A = A Int

main :: IO ()
main = do
  start
  newStablePtr (A 5)
  print "start"
  loop
