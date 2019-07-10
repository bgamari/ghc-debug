module Main where

import GHC.Debug.Client

import Control.Monad
import Debug.Trace
import Control.Exception


main = withDebuggee "/tmp/ghc-debug" p5

-- Test pause/resume
p1 d = pauseDebuggee d (void $ getChar)


-- Testing error codes
p2 d = do
  request d RequestPause
  print "req1"
  request d RequestPause
  request d RequestPause
  request d RequestPause

-- Testing get version
p3 d = do
  request d RequestVersion >>= print
  request d RequestPause
  request d RequestResume

-- Testing get roots
p4 d = do
  request d RequestPause
  request d RequestRoots >>= print

-- request closures
p5 d = do
  request d RequestPause
  r <- request d RequestRoots
  print (length r)
  forM_ [0..length r - 1] $ \i -> do
    let cs = [r !! i]
    print cs
    (c:_) <- request d (RequestClosures cs)
    let it = getInfoTblPtr c
    print it
    (itr:_) <- request d (RequestInfoTables [it])
    print itr
    print c
    print (decodeClosure itr c)

-- request all closures
p5a d = do
  request d RequestPause
  rs <- request d RequestRoots
  print rs
  cs <- request d (RequestClosures rs)
  print cs
  {-
  let it = getInfoTblPtr c
  print it
  (itr:_) <- request d (RequestInfoTables [it])
  print itr
  print c
  print (decodeClosure itr c)
  -}

-- request all closures
p5b d = do
  request d RequestPause
  rs <- request d RequestRoots
  print rs
  cs <- request d (RequestClosures rs)
  res <- mapM (lookupInfoTable d) cs
  mapM print (zip (map getInfoTblPtr cs) rs)
  mapM (print . uncurry decodeClosure . traceShowId) res



p6 d = do
  -- This blocks until a pause
  request d RequestPoll
  -- Should return already paused
  request d RequestPause
  -- Now unpause
  request d RequestResume

-- Request saved objects
p7 d = do
  request d RequestPause
  request d RequestSavedObjects >>= print

-- request saved objects
p8 d = do
  request d RequestPause
  cs <- request d RequestSavedObjects
  request d (RequestClosures (take 1 cs))

-- Using findPtr
p9 d = do
  request d RequestPause
  (s:_) <- request d RequestSavedObjects
  request d (RequestFindPtr s) >>= print






