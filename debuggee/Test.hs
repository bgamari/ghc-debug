{-# LANGUAGE BangPatterns #-}

module Main where

import GHC.Debug.Stub

main :: IO ()
main = do
  let !x = 42
  pause
  print x
