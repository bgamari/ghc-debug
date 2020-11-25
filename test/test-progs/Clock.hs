import GHC.Debug.Stub
import System.Mem
import Control.Concurrent
import System.IO
import Data.Word
import GHC.Stats
import GHC.Clock

loop :: IO ()
loop = do
  time <- getMonotonicTimeNSec
  print time
  hFlush stdout
  threadDelay oneSecond
  loop
  where
    oneSecond = 1000000

main :: IO ()
main = withGhcDebug $ do
  print "sync"
  hFlush stdout
  loop
