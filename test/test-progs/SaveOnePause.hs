import GHC.Debug.Stub
import System.Mem
import Control.Concurrent
import System.IO
import Data.Word
import GHC.Stats

main :: IO ()
main = do
  start

  print "sync"
  hFlush stdout

  let v = 1 :: Int
  saveClosures [Box v]

  performGC

  -- Give the test a chance to RequestPoll
  threadDelay 5000000

  pause
  print v
