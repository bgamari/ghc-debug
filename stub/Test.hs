{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Debug.Stub
import Control.Concurrent
import System.Mem.StableName
import Foreign.StablePtr
import GHC.Prim
import GHC.Exts
import GHC.IO
import System.Mem

loop :: IO ()
loop = go 0
  where
   go 10 = go 11
   go x = print x >> threadDelay 1000000 >> go (x + 1)

data A = A Int deriving Show

v :: Int
v = 5
{-# NOINLINE v #-}

main :: IO ()
main = do
  start
  let x = A v
  print x
  performGC
  IO (\s -> saveClosure# x s)
  print "start"
  loop
  print x
