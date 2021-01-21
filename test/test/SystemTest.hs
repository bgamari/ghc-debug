module SystemTest where

import Test.Tasty.Hspec

import GHC.Debug.Client
import GHC.Debug.Types
import GHC.Debug.Types.Graph hiding (buildHeapGraph)
import GHC.Debug.Types.Closures
import Data.Text (unpack)
import System.IO

import Server

import Control.Monad

import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad.Extra

import Data.Word
import Data.IORef
import GHC.Clock
import System.Timeout
import Data.List.Extra

spec :: SpecWith ()
spec = do
  describe "request" $ do
    describe "RequestVersion" $
      it "should return the correct version" $
        withStartedDebuggee "debug-test" $ \ _ d -> do
          version <- run d version
          version `shouldBe` 0

    describe "RequestRoots" $
      it "should return a non-empty result" $
        withStartedDebuggee "debug-test" $ \ _ d -> do
          pause d
          roots <- run d gcRoots
          roots `shouldSatisfy` notNull

    describe "RequestClosures" $
      it "should return a non-empty result" $
        withStartedDebuggee "debug-test" $ \ _ d -> do
          pause d
          closures <- run d (gcRoots >>= dereferenceClosures)
          closures `shouldSatisfy` notNull

    describe "RequestSavedObjects" $
      it "should return saved object" $
        withStartedDebuggee "save-one-pause" $ \ h d -> do
          waitForSync $ Server.stdout h
          withAsync (pipeStreamThread (Server.stdout h)) $ \_ -> do
            pausePoll d
            os@(o:_) <- run d savedObjects
            length os `shouldBe` 1
            hg <- run d $ buildHeapGraph (Just 20) o
            ppHeapGraph (const "") hg `shouldBe` "let x1() = I# 1\nin () r0: x1\n\n"

    describe "RequestInfoTables" $
      it "should return decodable RawInfoTables" $
        withStartedDebuggee "save-one-pause" $ \ h d -> do
          waitForSync $ Server.stdout h
          pausePoll d
          sos <- run d savedObjects
          closures <- run d (dereferenceClosures sos)
          let itptrs = map (tableId . info . noSize) closures
          its <- run d $ mapM dereferenceInfoTable itptrs
          length its `shouldBe` 1

    describe "RequestConstrDesc" $
      it "should return ConstrDesc of saved value (I# 1)" $
        withStartedDebuggee "save-one-pause" $ \ h d -> do
          waitForSync $ Server.stdout h
          pausePoll d
          (c:_) <- run d (savedObjects >>= dereferenceClosures)
          let itptr = tableId . info . noSize $ c
          cd <- run d (dereferenceConDesc itptr)
          cd `shouldBe` ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = "I#"}

    describe "RequestResume" $
      it "should resume a paused debugee" $
        withStartedDebuggee "clock" $ \ h d -> do
          waitForSync $ Server.stdout h
          ref <- newIORef []
          withAsync (pipeStreamToListThread ref (Server.stdout h)) $ \_ -> do
            pause d
            (t:_) <- readIORef ref
            assertNoNewClockTimes ref t

            resume d

            assertNewClockTime ref
            where
              oneSecondInMicros = 1000000

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
                result <- timeout fiveSecondsInMicros $ whileM $ do
                  threadDelay 5000
                  (t:_) <- readIORef ref
                  return $ t < now

                result `shouldBe` Just ()

fiveSecondsInMicros :: Int
fiveSecondsInMicros = 5000000

waitForSync :: Handle -> IO ()
waitForSync h = do
  result <- timeout fiveSecondsInMicros $ do
    hSetBuffering h LineBuffering
    l <- hGetLine h
    if l == "\"sync\"" then
      return ()
    else
      waitForSync h

  case result of
    Nothing -> error "Can not sync!"
    _ -> return ()

pipeStreamThread :: Handle -> IO ()
pipeStreamThread h = forever $ do
        l <- hGetLine h
        print l

type ClockTime = Word64

pipeStreamToListThread :: IORef [ClockTime] -> Handle -> IO ()
pipeStreamToListThread ref h = forever $ do
  l <- hGetLine h
  timesList <- readIORef ref
  writeIORef ref $ toClockTime l : timesList
  where
    toClockTime :: String -> ClockTime
    toClockTime = read . trim
