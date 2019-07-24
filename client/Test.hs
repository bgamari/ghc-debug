module Main where

import GHC.Debug.Client

import Control.Monad
import Debug.Trace
import Control.Exception
import Control.Concurrent
import Data.Bitraversable

prog = "/home/matt/ghc-debug/dist-newstyle/build/x86_64-linux/ghc-8.9.0.20190718/ghc-debug-stub-0.1.0.0/x/debug-test/build/debug-test/debug-test"

prog2 = "/home/matt/dyepack/dist-newstyle/build/x86_64-linux/ghc-8.9.0.20190718/dyepack-0.1.0.0/x/example/build/example/example"

--main = withDebuggeeSocket "/tmp/ghc-debug" Nothing p14
main = withDebuggee prog2 p12

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
    dereferenceClosures d cs

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
  dereferenceClosures d rs



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
  dereferenceClosures d sos

-- Using findPtr
p9 d = do
  request d RequestPause
  (s:_) <- request d RequestSavedObjects
  print s
  sos <- request d (RequestFindPtr s)
  print ("FIND_PTR_RES", sos)
  dereferenceClosures d sos

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
  r <- request d (RequestFindPtr ss)
  print ss
  putStrLn "Retaining closures"
  dcs <- dereferenceClosures d r
  mapM print dcs
  putStrLn ""
  cs <- request d (RequestClosures r)
  forM_ cs $ \c -> do
    let itb = getInfoTblPtr c
    case lookupDwarf d itb of
      Just r -> showFileSnippet r
      Nothing -> return ()

  print "Following thunk"
  let thunk = r !! 2
  r <- request d (RequestFindPtr thunk)
  putStrLn "Retaining closures 2"
  dereferenceClosures d r >>= mapM print
  putStrLn ""
  cs <- request d (RequestClosures r)
  forM_ cs $ \c -> do
    let itb = getInfoTblPtr c
    case lookupDwarf d itb of
      Just r -> showFileSnippet r
      Nothing -> return ()

-- testing stack decoding
p13 d = do
  request d RequestPause
  rs <- request d RequestRoots
  forM_ rs $ \r -> do
    print r
    res <- fullTraversal d r
    print res


p14 d = do
  request d RequestPause
  rs <- request d RequestSavedObjects
  forM_ rs $ \r -> do
    print r
    res <- fullTraversal d r
    print res



