import GHC.Debug.Stub
import System.Mem
import Control.Concurrent
import System.IO
import Data.Word
import GHC.Stats

main :: IO ()
main = withGhcDebug $ do
  print "sync"
  hFlush stdout

  saveClosures [Box (id 5)]

  performGC

  -- Give the test a chance to RequestPoll
  threadDelay 50000000

