{-# LANGUAGE BangPatterns #-}

module Main where

import GHC.Debug.Stub
import Control.Concurrent

loop :: IO ()
loop = go 0
  where
   go x = print x >> threadDelay 1000000 >> go (x + 1)

main :: IO ()
main = do
  start
  print "start"
  loop
