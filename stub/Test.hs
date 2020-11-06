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
  let !y = Data.Sequence.fromList [1..5]
  -- let !y = [1..5]
  performGC
--  saveClosures [Box y]
  saveClosures [Box (id ())]
  print "start"
  loop
  print y
