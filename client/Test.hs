module Main where

import GHC.Debug.Client

import Control.Monad
import Debug.Trace
import Control.Exception
import Control.Concurrent
import GHC.Exts.Heap

prog = "/home/matt/ghc-debug/dist-newstyle/build/x86_64-linux/ghc-8.9.0.20190718/ghc-debug-stub-0.1.0.0/x/debug-test/build/debug-test/debug-test"

prog2 = "/home/matt/dyepack/dist-newstyle/build/x86_64-linux/ghc-8.9.0.20190628/dyepack-0.1.0.0/x/example/build/example/example"

main = withDebuggeeSocket "/tmp/ghc-debug" Nothing p13
--main = withDebuggee prog p13

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
  print "POLL"
  -- Should return already paused
  request d RequestPause
  print "PAUSE"
  -- Now unpause
  request d RequestResume
  print "RESUME"

-- Request saved objects
p7 d = do
  request d RequestPause
  request d RequestSavedObjects >>= print

-- request saved objects
p8 d = do
  request d RequestPause
  sos <- request d RequestSavedObjects
  cs <- request d (RequestClosures sos)
  res <- mapM (lookupInfoTable d) cs
  mapM print (zip (map getInfoTblPtr cs) sos)
  mapM (print . uncurry decodeClosure . traceShowId) res

-- Using findPtr
p9 d = do
  request d RequestPause
  (s:_) <- request d RequestSavedObjects
  print s
  sos <- request d (RequestFindPtr s)
  print ("FIND_PTR_RES", sos)
  cs <- request d (RequestClosures sos)
  res <- mapM (lookupInfoTable d) cs
  mapM print (zip (map getInfoTblPtr cs) sos)
  mapM (print . uncurry decodeClosure . traceShowId) res

p10 d = do
  request d RequestPause
  (s:_) <- request d RequestRoots
  request d (RequestFindPtr s) >>= print

p11 d = do
  threadDelay 10000000
  request d RequestPause
  ss <- request d RequestSavedObjects
  [c] <- request d (RequestClosures ss)
  let itb = getInfoTblPtr c
  case lookupDwarf d itb of
    Just r -> showFileSnippet r
    Nothing -> return ()

p12 d = do
  request d RequestPoll
  [ss] <- request d RequestSavedObjects
  [r] <- request d (RequestFindPtr ss)
  [c] <- request d (RequestClosures [r])
  let itb = getInfoTblPtr c
  case lookupDwarf d itb of
    Just r -> showFileSnippet r
    Nothing -> return ()


-- testing stack decoding
p13 d = do
  request d RequestPause
  -- Just get TSO closure
  (r:_) <- request d RequestRoots
  let rs = [r]
  print rs
  cs <- request d (RequestClosures rs)
  res <- mapM (lookupInfoTable d) cs
  mapM print (zip (map getInfoTblPtr cs) rs)
  -- Should be one TSO closure
  let is = map (decodeInfoTable . fst) res
  let [cs] = map (uncurry decodeClosure) res
  let sps = stackFromTSO cs
  print ("SPS", sps)
  [sps_cs] <- request d (RequestClosures [sps])
  sps_res <- lookupInfoTable d sps_cs
  let s = uncurry decodeClosure sps_res
  print ("SPS", sps)
  print ("SP", (stackPointer s))
  print (rawClosureSize sps_cs)
  let n = (subtractClosurePtr (stackPointer s) sps)
  print n
  let stack = (dropRawClosure (fromIntegral n) sps_cs)
  print stack
  i <- lookupInfoTable d stack
  let st_it = decodeInfoTable . fst $ i
  print i
  print st_it
  bt <- request d (RequestBitmap (getInfoTblPtr stack))
  let decoded_stack = decodeStack stack st_it bt
  print decoded_stack


stackFromTSO (TSOClosure _ sp) = sp




