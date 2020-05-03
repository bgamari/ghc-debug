module Main where

import Test.Tasty
import Test.Tasty.Hspec
import SystemTest

main :: IO ()
main = do
  tree <- testSpec "hspec tests" $ parallel spec
  defaultMain tree

