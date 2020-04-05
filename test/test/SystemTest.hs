module SystemTest where

import Test.Hspec

import GHC.Debug.Client
import Data.Text (unpack)
import System.IO
import Data.Dwarf.ADT

import Server

import Debug.Trace
import Control.Monad

import Control.Concurrent.Async
import Control.Concurrent

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

    describe "RequestSavedObjects" $
      it "should return saved object" $
        withStartedDebuggeeAndHandles "save-one" $ \ h d -> do
          waitForSync $ Server.stdout h
          let errSinkThread = forever $ do
                l <- hGetLine (Server.stdout h)
                print l
          withAsync errSinkThread $ \_ -> do
            -- TODO Get rid of the `threadDelay`. `save-one` should signal that the GC has finished.
            threadDelay 5000000
            request d RequestPause
            ss@(s:_) <- request d RequestSavedObjects
            length ss `shouldBe` 1
            sos <- request d (RequestFindPtr s)
            print $ "sos : " ++ show sos
            length sos `shouldBe` 1
            dcs <- dereferenceClosures d sos
            mapM_ print dcs
            length dcs `shouldBe` 1

waitForSync :: Handle -> IO ()
waitForSync h = do
  hSetBuffering h LineBuffering
  l <- hGetLine h
  traceIO $ "line " ++ l
  if l == "\"sync\"" then
    return ()
  else
    waitForSync h
-- TODO There should be some exit condition.
--    error "Can not sync!"

shouldContainCuName :: Dwarf -> String -> Expectation
shouldContainCuName dwarf name = allCuNames `shouldContain` [name]
  where
    allCuNames :: [String]
    allCuNames =  map (unpack . cuName . bData) boxedCompilationUnits

    boxedCompilationUnits :: [Boxed CompilationUnit]
    boxedCompilationUnits = dwarfCompilationUnits dwarf
