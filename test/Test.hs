module Main where

import GHC.Debug.Client
import GHC.Debug.Types.Graph
import GHC.Debug.Types.Closures

import Control.Monad
import Debug.Trace
import Control.Exception
import Control.Concurrent
import Data.Bitraversable
import GHC.Vis
import Data.Monoid
import Control.Applicative

import Data.List.Extra (trim)
import System.Process

saveOnePath :: IO FilePath
saveOnePath = testProgPath "save-one"

debugTestPath :: IO FilePath
debugTestPath = testProgPath "debug-test"

dyePackTestPath :: IO FilePath
dyePackTestPath = testProgPath "dyepack-test"

testProgPath :: String -> IO FilePath
testProgPath progName = do
  path <- readCreateProcess shellCmd []
  return $ trim path
  where
    shellCmd = shell $ "which " ++ progName

main = withDebuggeeSocket "banj" "/tmp/ghc-debug" Nothing p13
{-
main = do
  -- Get the path to the "debug-test" executable
--  prog <- debugTestPath -- Or @dyePackTestPath@
--  print prog
  let prog = "/home/matt/ghc-debug/dist-newstyle/build/x86_64-linux/ghc-9.1.0.20201106/ghc-debug-stub-0.1.0.0/x/debug-test/build/debug-test/debug-test"

  -- Start the program and do some debugging
  let someDebuggingAction = p13
  withDebuggee prog "/tmp/ghc-debug" someDebuggingAction
  -}

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
  print =<< dereferenceClosures d sos

-- Using findPtr
{-
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
  -}

p11 d = do
  threadDelay 10000000
  request d RequestPause
  ss <- request d RequestSavedObjects
  [c] <- request d (RequestClosures ss)
  let itb = getInfoTblPtr c
  case lookupDwarf d itb of
    Just r -> showFileSnippet d r
    Nothing -> print "No Dwarf!"

{-
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
      Just r -> showFileSnippet d r
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
      Just r -> showFileSnippet d r
      Nothing -> return ()
      -}

-- testing stack decoding
p13 d = do
  request d RequestPause
  rs <- request d RequestRoots
  putStrLn ("NUMBER OF ROOTS = " ++ show (length rs))
  results <- forM (zip rs [0..]) $ \(r, n) -> do
              print ("ROOT", n, r)
              fullTraversal d r
  putStrLn "Full Traversal complete"
  putStrLn ("Number of roots traversed: " ++ show (length results))
  let counts = map countNodes results
      inclusive_counts = map inclusive results
  forM (zip results [0..]) $ \(re@(MkFix1 r), n) -> do
    putStrLn (show n ++ "(" ++ show (tipe (info (noSize r))) ++ "): " ++ show (treeSize re))
    --print (inclusive re)
  putStrLn ("Total: " ++ show (sum counts))
  traceRequestLog d


p14 d = do
  request d RequestPause
  rs <- request d RequestSavedObjects
  forM_ rs $ \r -> do
    print r
    res <- fullTraversal d r
    print res

-- Testing ghc-vis
p15 d = do
  request d RequestPause
  (r:_) <- request d RequestSavedObjects
  vis d
  view r "saved"
  getChar

-- pretty-print graph
p16 d = do
  request d RequestPause
  [so] <- request d RequestSavedObjects
  hg <- buildHeapGraph (derefBox d) 20 () so
  putStrLn $ ppHeapGraph hg

-- Testing IPE
p17 d = do
  request d RequestPause
  [so] <- request d RequestSavedObjects
  [c] <- request d (RequestClosures [so])
  let it = getInfoTblPtr c
  print c
  print it
  print =<< request d (RequestSourceInfo it)
