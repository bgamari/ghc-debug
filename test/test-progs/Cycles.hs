import GHC.Debug.Stub
import System.Mem
import Control.Concurrent
import System.IO
import Data.Word
import GHC.Stats

-- Create a cycle in the heap, to make sure that e.g. generalBuildHeapGraph
-- with no depth limit terminates

main :: IO ()
main = withGhcDebug $ do
  print "sync"
  hFlush stdout

  let {-# NOINLINE l #-}
      l = cycle [True, False]
  -- Force a bit; I'm not sure if this matters:
  case l of
    (True:False:True:_) -> return ()
    _ -> error "impossible"

  performGC

  -- Give the test a chance to RequestPoll
  threadDelay $ 60*1000*1000

  print $ head $ drop 42 l

