module Main where

import GHC.Debug.Client
import GHC.Debug.Types.Graph

import Control.Monad
import Control.Monad.State.Lazy (liftIO, get)
import Debug.Trace
import Control.Exception
import Control.Concurrent
import Data.Bitraversable
import GHC.Vis

prog = "/home/matt/ghc-debug/dist-newstyle/build/x86_64-linux/ghc-8.9.0.20190806/ghc-debug-stub-0.1.0.0/x/debug-test/build/debug-test/debug-test"

prog2 = "/home/matt/ghc-debug/dist-newstyle/build/x86_64-linux/ghc-8.9.0.20190806/dyepack-test-0.1.0.0/x/dyepack-test/build/dyepack-test/dyepack-test"

--main = withDebuggeeSocket "/tmp/ghc-debug" Nothing p14
main = withDebuggee prog2 p12
-- main = withDebuggee prog p15

-- Test pause/resume
p1 :: DebuggeeAction ()
p1 = do
  pauseDebuggee (liftIO getChar)
  return ()

-- Testing error codes
p2 = do
  request RequestPause
  liftIO $ print "req1"
  request RequestPause
  request RequestPause
  request RequestPause

-- Testing get version
p3 :: DebuggeeAction ()
p3 = do
  version <- request RequestVersion
  liftIO $ print version
  request RequestPause
  request RequestResume
  return ()

-- Testing get roots
p4 = do
  request RequestPause
  roots <- request RequestRoots
  liftIO $ print roots

-- request closures
p5 = do
  request RequestPause
  r <- request RequestRoots
  liftIO $ print (length r)
  forM_ [0..length r - 1] $ \i -> do
    let cs = [r !! i]
    liftIO $ print cs
    dereferenceClosures cs

-- request all closures
p5a = do
  request RequestPause
  rs <- request RequestRoots
  liftIO $ print rs
  cs <- request (RequestClosures rs)
  liftIO $ print cs
  {-
  let it = getInfoTblPtr c
  print it
  (itr:_) <- request d (RequestInfoTables [it])
  print itr
  print c
  print (decodeClosure itr c)
  -}

-- request all closures
p5b = do
  request RequestPause
  rs <- request RequestRoots
  dereferenceClosures rs


p6 = do
  -- This blocks until a pause
  request RequestPoll
  liftIO $ print "POLL"
  -- Should return already paused
  request RequestPause
  liftIO $ print "PAUSE"
  -- Now unpause
  request RequestResume
  liftIO $ print "RESUME"

-- Request saved objects
p7 = do
  request RequestPause
  objs <- request RequestSavedObjects
  liftIO $ print objs

-- request saved objects
p8 = do
  request RequestPause
  sos <- request RequestSavedObjects
  dereferenceClosures sos

-- Using findPtr
p9 = do
  request RequestPause
  (s:_) <- request RequestSavedObjects
  liftIO $ print s
  sos <- request (RequestFindPtr s)
  liftIO $ print ("FIND_PTR_RES", sos)
  dereferenceClosures sos

p10 d = do
  request RequestPause
  (s:_) <- request RequestRoots
  ptr <- request (RequestFindPtr s)
  liftIO $ print ptr

p11 = do
  liftIO $ threadDelay 10000000
  request RequestPause
  ss <- request RequestSavedObjects
  [c] <- request (RequestClosures ss)
  let itb = getInfoTblPtr c
  mDwarf <- lookupDwarf itb
  case mDwarf of
    Just r -> showFileSnippet r
    Nothing -> return ()

p12 = do
  liftIO $ putStrLn "Polling.."
  request RequestPoll
  liftIO $ putStrLn "Requesting saved objects.."
  [ss] <- request RequestSavedObjects
  liftIO $ putStrLn "Requesting pointer to saved objects.."
  r <- request (RequestFindPtr ss)
  liftIO $ print ss
  liftIO $ putStrLn "Retaining closures"
  dcs <- dereferenceClosures r
  liftIO $ mapM print dcs
  liftIO $ putStrLn ""
  liftIO $ putStrLn "Requesting closures.."
  cs <- request (RequestClosures r)
  forM_ cs $ \c -> do
    let itb = getInfoTblPtr c
    mDwarf <- lookupDwarf itb
    case mDwarf of
      Just r -> showFileSnippet r
      Nothing -> return ()

  liftIO $ print "Following thunk"
  let thunk = r !! 2
  r <- request (RequestFindPtr thunk)
  liftIO $ putStrLn "Retaining closures 2"
  closures <- dereferenceClosures r
  liftIO $ mapM_ print closures
  liftIO $ putStrLn ""
  cs <- request (RequestClosures r)
  forM_ cs $ \c -> do
    let itb = getInfoTblPtr c
    mDwarf <- lookupDwarf itb
    case mDwarf of
      Just r -> showFileSnippet r
      Nothing -> return ()

-- testing stack decoding
p13 = do
  request RequestPause
  rs <- request RequestRoots
  forM_ rs $ \r -> do
    liftIO $ print r
    res <- fullTraversal r
    liftIO $ print res


p14 = do
  request RequestPause
  rs <- request RequestSavedObjects
  forM_ rs $ \r -> do
    liftIO $ print r
    res <- fullTraversal r
    liftIO $ print res

-- Testing ghc-vis
p15 :: DebuggeeAction Char
p15 = do
  request RequestPause
  (r:_) <- request RequestSavedObjects
  d <- get
  liftIO $ vis d
  liftIO $ view r "saved"
  liftIO $ getChar

-- pretty-print graph
p16 = do
  request RequestPause
  [so] <- request RequestSavedObjects
  dbg <- get
  hg <- liftIO $ buildHeapGraph (derefBox dbg) 20 () so
  liftIO $ putStrLn $ ppHeapGraph hg
