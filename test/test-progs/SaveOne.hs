import GHC.Debug.Stub
import System.Mem
import Control.Concurrent
import System.IO
import Data.Word
import GHC.Stats

loop :: IO ()
loop = go 0
  where
   go x = hFlush stdout >> print x >> threadDelay 1000000 >> go (x + 1)

main :: IO ()
main = do
  start
  let v = 1 :: Int
  performGC
  saveClosures [Box v]
  print "sync"
  hFlush stdout
  loop
  print $ v
