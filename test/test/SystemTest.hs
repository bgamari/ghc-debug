module SystemTest where

import Test.Hspec

import System.Process
import GHC.Debug.Client
import System.IO.Extra
import Data.List.Extra (trim)
import Data.Text (unpack)

import Data.Dwarf.ADT

import Server

spec :: SpecWith ()
spec = do
  describe "debuggeeDwarf" $ do
    it "should return Dwarf of the executeable" $ do
      withTempDir $ \ tempDirPath -> do
        let socketName = tempDirPath ++ "/ghc-debug"
        withServer "debug-test" socketName True $ \serverIn serverOut serverProc -> do
          prog <- readCreateProcess (shell "which debug-test") []
          withDebuggee (trim prog) socketName $ \ d -> do
            case debuggeeDwarf d of
              Just dwarf -> dwarf `shouldContainCuName` "Test.hs"
              Nothing -> error "No Dwarf"

  describe "request" $ do
    describe "version" $ do
      it "should return the correct version" $ do
        withTempDir $ \ tempDirPath -> do
          let socketName = tempDirPath ++ "/ghc-debug"
          withServer "debug-test" socketName True $ \serverIn serverOut serverProc -> do
            prog <- readCreateProcess (shell "which debug-test") []
            withDebuggee (trim prog) socketName $ \ d -> do
              version <- request d RequestVersion
              request d RequestPause
              request d RequestResume
              version `shouldBe` 0

shouldContainCuName :: Dwarf -> String -> Expectation
shouldContainCuName dwarf name = allCuNames `shouldContain` [name]
  where
    allCuNames :: [String]
    allCuNames =  map (unpack . cuName . bData) boxedCompilationUnits

    boxedCompilationUnits :: [Boxed CompilationUnit]
    boxedCompilationUnits = dwarfCompilationUnits dwarf
