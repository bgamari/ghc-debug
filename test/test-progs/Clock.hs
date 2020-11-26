{-# LANGUAGE BangPatterns #-}

import GHC.Debug.Stub
import System.Mem
import Control.Concurrent
import System.IO
import Data.Word
import Data.IORef
import GHC.Stats
import GHC.Clock

loop :: Int -> IORef [Int] -> IO ()
loop i growingListIORef = do
  time <- getMonotonicTimeNSec
  print time
  hFlush stdout
  threadDelay oneSecond
  modifyIORef growingListIORef (i:)
  loop (i+1) growingListIORef
  where
    oneSecond = 1000000

main :: IO ()
main = withGhcDebug $ do
  print "sync"
  growingListIORef <- newIORef []
  hFlush stdout
  let helloWorld = "Hello World!"
      !n = length helloWorld
  saveClosures
    [ Box helloWorld
    , Box growingListIORef
    ]
  loop 0 growingListIORef
