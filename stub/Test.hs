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

loop :: IO ()
loop = go 0
  where
   go 10 = pause >> go 11
   go x = print x >> threadDelay 1000000 >> go (x + 1)

data A = A Int deriving Show

main :: IO ()
main = do
  start
  let x = A 5
  IO (\s -> saveClosure# x s)
  print "start"
  loop
  print x
