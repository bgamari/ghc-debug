{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Debug.Stub
import Control.Concurrent
import System.Mem.StableName
import Foreign.StablePtr
import System.Mem
import qualified Data.Sequence
import qualified Data.HashMap.Strict

loop :: IO ()
loop = go 0
  where
   go 5 = pause >> go 11
   go x = print x >> threadDelay 1000000 >> go (x + 1)

data A = A Int deriving Show

v :: Int
v = 5
{-# NOINLINE v #-}

main :: IO ()
main = do
  start
  let x = A v
  let !y = Data.Sequence.fromList [1..5]
  let !z = replicate 5 0

  print (take 3 z)
  print y
--  print h
  performGC
  saveClosures [Box y]
  print "start"
  loop
  print x
  print y
  print z
