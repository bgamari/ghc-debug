import GHC.Debug.Stub
import System.Mem
import Control.Concurrent
import System.IO
import Data.Word
import GHC.Stats

loop :: IO ()
loop = go 0
  where
   go 0 = threadDelay 500000 >> pause >> go 1
   go x = threadDelay 500000 >> go (x + 1)

main :: IO ()
main = do
  start

  print "sync"
  hFlush stdout

  let v = 1 :: Int
  performGC
  saveClosures [Box v]
  loop
  print v
