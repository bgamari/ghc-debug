{-# language NumericUnderscores #-}

import GHC.Debug.Stub
import Control.Monad
import Control.Concurrent
import System.Mem
import System.IO

mk_fun :: Int -> Int -> Bool
mk_fun x = (> x)

fun_list1 :: [Int -> Bool]
fun_list1 = [mk_fun x | x <- [0]]

thunk_list5 :: [Bool]
thunk_list5 = [ mk_fun x 1 | x <- [0..10]]

main :: IO ()
main = do
  withGhcDebug $ do
    print "sync"
    hFlush stdout
    let x = fun_list1
    saveClosures [Box fun_list1, Box thunk_list5]
    performGC

    -- Give the test a chance to RequestPoll
    threadDelay 2_000_000

    pause

    print (length [Box fun_list1, Box thunk_list5] )
