module SystemTest where

import Test.Hspec

import System.Process
import GHC.IO.Handle

import GHC.Debug.Client

import Control.Monad
import Debug.Trace
import Control.Exception
import Control.Concurrent
import Data.Bitraversable

import System.Directory
import System.IO.Extra hiding (map)
import Data.List.Extra hiding (map)
import Data.Text hiding (map)

import Data.Dwarf.ADT

import Server

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

shouldContainCuName :: Dwarf -> String -> Expectation
shouldContainCuName dwarf name = allCuNames `shouldContain` [name]
  where
    allCuNames :: [String]
    allCuNames =  map (unpack . cuName . bData) boxedCompilationUnits

    boxedCompilationUnits :: [Boxed CompilationUnit]
    boxedCompilationUnits = dwarfCompilationUnits dwarf
