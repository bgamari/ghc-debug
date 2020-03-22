module SystemTest where

import Test.Hspec

import GHC.Debug.Client
import Data.Text (unpack)

import Data.Dwarf.ADT

import Server

spec :: SpecWith ()
spec = do
  describe "debuggeeDwarf" $
    it "should return Dwarf of the executeable" $
      withStartedDebuggee "debug-test" $ \ d ->
        case debuggeeDwarf d of
              Just dwarf -> dwarf `shouldContainCuName` "Test.hs"
              Nothing -> error "No Dwarf"

  describe "request" $ do
    describe "RequestVersion" $
      it "should return the correct version" $
        withStartedDebuggee "debug-test" $ \ d -> do
          version <- request d RequestVersion
          version `shouldBe` 0

    describe "RequestRoots" $
      it "should return a non-empty result" $
        withStartedDebuggee "debug-test" $ \ d -> do
              request d RequestPause
              roots <- request d RequestRoots
              roots `shouldNotBe` []

shouldContainCuName :: Dwarf -> String -> Expectation
shouldContainCuName dwarf name = allCuNames `shouldContain` [name]
  where
    allCuNames :: [String]
    allCuNames =  map (unpack . cuName . bData) boxedCompilationUnits

    boxedCompilationUnits :: [Boxed CompilationUnit]
    boxedCompilationUnits = dwarfCompilationUnits dwarf
