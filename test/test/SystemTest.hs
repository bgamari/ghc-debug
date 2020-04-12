module SystemTest where

import Test.Hspec

import GHC.Debug.Client
import GHC.Debug.Types.Graph
import GHC.Vis
import Data.Text (unpack)
import System.IO
import Data.Dwarf.ADT

import Server

import Debug.Trace
import Control.Monad

import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad.Extra

import Data.Word
import Data.IORef
import GHC.Clock
import System.Timeout
import Data.List.Extra

-- TODO use timeout for tests
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
          withAsync (pipeStreamThread (Server.stdout h)) $ \_ -> do
            -- TODO Get rid of the `threadDelay`.
            -- `save-one` should signal that the GC has finished.
            threadDelay 5000000
            request d RequestPause
            os@(o:_) <- request d RequestSavedObjects
            length os `shouldBe` 1
            hg <- buildHeapGraph (derefBox d) 20 () o
            ppHeapGraph hg `shouldBe` "I# 1"

    describe "RequestResume" $
      it "should resume a paused debugee" $
        withStartedDebuggeeAndHandles "clock" $ \ h d -> do
          waitForSync $ Server.stdout h
          ref <- newIORef []
          withAsync (pipeStreamToListThread ref (Server.stdout h)) $ \_ -> do
            request d RequestPause
            (t:_) <- readIORef ref
            assertNoNewClockTimes ref t

            request d RequestResume

            assertNewClockTime ref
            where
              oneSecondInMicros = 1000000
              fiveSecondsInMicros = 5000000

              assertNoNewClockTimes :: IORef [ClockTime] -> ClockTime -> Expectation
              assertNoNewClockTimes ref t0 = do
                result <- timeout fiveSecondsInMicros $ whileM $ do
                  threadDelay oneSecondInMicros
                  (t1:_) <- readIORef ref
                  return $ t0 == t1

                result `shouldBe` Nothing

              assertNewClockTime :: IORef [ClockTime] -> Expectation
              assertNewClockTime ref = do
                now <- getMonotonicTimeNSec
                print $ "now2 : " ++ show now
                result <- timeout fiveSecondsInMicros $ whileM $ do
                  threadDelay 5000
                  (t:_) <- readIORef ref
                  return $ t < now

                result `shouldBe` Just ()


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

pipeStreamThread :: Handle -> IO ()
pipeStreamThread h = forever $ do
        l <- hGetLine h
        print l

type ClockTime = Word64

pipeStreamToListThread :: IORef [ClockTime] -> Handle -> IO ()
pipeStreamToListThread ref h = forever $ do
  l <- hGetLine h
  timesList <- readIORef ref
  writeIORef ref $ (toClockTime l) : timesList
  where
    toClockTime :: String -> ClockTime
    toClockTime s = read . trim $ s

shouldContainCuName :: Dwarf -> String -> Expectation
shouldContainCuName dwarf name = allCuNames `shouldContain` [name]
  where
    allCuNames :: [String]
    allCuNames =  map (unpack . cuName . bData) boxedCompilationUnits

    boxedCompilationUnits :: [Boxed CompilationUnit]
    boxedCompilationUnits = dwarfCompilationUnits dwarf
