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
import qualified Data.Map as M
--import qualified Data.Map.Strict as M

loop :: IO ()
loop = go 0
  where
   go 5 = pause >> go 11
   go x = print x >> threadDelay 1000000 >> go (x + 1)
   go 100 = return ()

data A = A Int deriving Show

v :: Int
v = 5
{-# NOINLINE v #-}

type TestMap = M.Map String Int

main :: IO ()
main = withGhcDebug $ do
--  print "Enter Name"
--  name <- getLine
--  print "Enter Name"
--  name2 <- getLine
--  let !m = M.insert name (length name) (M.insert name2 (length name2) M.empty)

  --let !y = Data.Sequence.fromList [1..5]
  let !y = [1..100000]
  print (length y)
  performGC
--  saveClosures [Box y]
  saveClosures [Box y]
  print "start"
  loop
  print y




