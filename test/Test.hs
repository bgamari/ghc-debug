{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import GHC.Debug.Client hiding (traceFrom)
import GHC.Debug.Client.Retainers
import GHC.Debug.Client.Fragmentation
import GHC.Debug.Client.Profile
import GHC.Debug.Client.Count
import GHC.Debug.Client.Monad  hiding (withDebuggeeConnect)
import GHC.Debug.Types.Graph
import GHC.Debug.Types.Closures
import GHC.Debug.Client.Trace
import GHC.Debug.Client.ObjectEquiv
import Control.Monad.RWS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.State
import Data.Text (Text)
import GHC.Exts.Heap.ClosureTypes
import qualified Data.Foldable as F

import Control.Monad
import Debug.Trace
import Control.Exception
import Control.Concurrent
import Data.Bitraversable
import Data.Monoid
import Control.Applicative

import Data.List.Extra (trim)
import System.Process
import Data.Tree
import Data.Maybe
import qualified Data.Map as Map
import Data.Ord
import Data.List
import Data.List.NonEmpty(NonEmpty(..))

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

--main = withDebuggeeConnect "banj" "/tmp/ghc-debug" (\(Debuggee e) -> p33 e  >> outputRequestLog e)

main = snapshotRun "/tmp/ghc-debug-cache" (\(Debuggee e) -> p37 e)
{-
main = do
  -- Get the path to the "debug-test" executable
--  prog <- debugTestPath -- Or @dyePackTestPath@
--  print prog
  let prog = "/home/matt/ghc-debug/dist-newstyle/build/x86_64-linux/ghc-9.1.0.20201106/ghc-debug-stub-0.1.0.0/x/debug-test/build/debug-test/debug-test"

  -- Start the program and do some debugging
  let someDebuggingAction = p13
  withDebuggeeRun prog "/tmp/ghc-debug" someDebuggingAction
  -}

-- Test pause/resume
p1 e = withPause e ((void $ getChar))


-- Testing error codes
p2 e = runTrace e $ do
  request RequestPause
  traceWrite "req1"
  request RequestPause
  request RequestPause
  request RequestPause

-- Testing get version
p3 e = runTrace e $ do
  ver <- request RequestVersion
  request RequestPause
  request RequestResume
  return ver


-- Testing get roots
p4 e = pauseThen e $ do
  request RequestRoots >>= traceWrite

-- request closures
p5 e = pauseThen e $ do
  r <- request RequestRoots
  traceWrite (length r)
  forM_ [0..length r - 1] $ \i -> do
    let cs = [r !! i]
    traceWrite cs
    dereferenceClosures cs

-- request all closures
p5a e = pauseThen e $ do
  rs <- request RequestRoots
  traceWrite rs
  cs <- dereferenceClosures rs
  traceWrite cs
  {-
  let it = getInfoTblPtr c
  print it
  (itr:_) <- request d (RequestInfoTables [it])
  print itr
  print c
  print (decodeClosure itr c)
  -}

-- request all closures
p5b e = pauseThen e $ do
  rs <- request RequestRoots
  dereferenceClosures rs





p6 e = do
  -- This blocks until a pause
  run e $ request RequestPoll
  putStrLn "POLL"
  -- Should return already paused
  pause (Debuggee e)
  putStrLn "PAUSE"
  -- Now unpause
  resume (Debuggee e)
  putStrLn "RESUME"

-- Request saved objects
p7 e = pauseThen e $ do
  request RequestSavedObjects >>= traceWrite

-- request saved objects
p8 e = pauseThen e $ do
  sos <- request RequestSavedObjects
  traceWrite =<< dereferenceClosures sos

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

p11 e = do
  threadDelay 10000000
  pause e
  itb <- runTrace e $ do
    ss <- request d RequestSavedObjects
    [c] <- request d (RequestClosures ss)
    return (getInfoTblPtr c)
  case lookupDwarf d itb of
    Just r -> showFileSnippet d r
    Nothing -> print "No Dwarf!"
  -}

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
p13 e = pauseThen e $ do
  rs <- request RequestRoots
  traceWrite ("NUMBER OF ROOTS = " ++ show (length rs))
  results <- forM (zip rs [0..]) $ \(r, n) -> do
              traceWrite ("ROOT", n, r)
              fullTraversal r
  return results
  traceMsg "Full Traversal complete"
  traceMsg ("Number of roots traversed: " ++ show (length results))
  let counts = map countNodes results
      inclusive_counts = map inclusive results
  forM (zip results [0..]) $ \(re@(MkFix1 r), n) -> do
    traceMsg (show n ++ "(" ++ show (tipe (decodedTable (info (noSize r)))) ++ "): " ++ show (treeSize re))
    --print (inclusive re)
  traceMsg ("Total: " ++ show (sum counts))


p14 e = pauseThen e $ do
  rs <- request RequestSavedObjects
  forM_ rs $ \r -> do
    traceWrite r
    res <- fullTraversal r
    return res
    --traceWrite res

-- Testing ghc-vis
{-
p15 d = do
  request d RequestPause
  (r:_) <- request d RequestSavedObjects
  vis d
  view r "saved"
  getChar
  -}

-- pretty-print graph
p16 e = do
  pause (Debuggee e)
  hg <- run e $ do
          (so:_) <- request RequestSavedObjects
          buildHeapGraph derefFuncM Nothing so
  putStrLn $ ppHeapGraph (const "") hg

-- Testing IPE
p17 e = do
  pause (Debuggee e)
  runTrace e $ do
    [so] <- request RequestSavedObjects
    c <- request (RequestClosure so)
    let it = getInfoTblPtr c
    traceWrite c
    traceWrite it
    traceWrite =<< request (RequestSourceInfo it)


-- Use with large-thunk
p18 e = do
  -- Wait fro the pause
  run e $ request RequestPoll
  runTrace e $ do
    rs <- request RequestSavedObjects
    forM_ rs $ \r -> do
      traceWrite r
      fullTraversal r
      traceWrite ("DONE", r)

derefFunc e c = run e $ derefFuncM c

derefFuncM c = do
  c <- dereferenceClosureFromBlock c
  quadtraverse dereferencePapPayload dereferenceConDesc dereferenceStack pure c

-- Use with large-thunk
p19 e = do
  run e $ request RequestPoll
  hg <- run e $ do
          (so:_) <- request RequestSavedObjects
          hg <- buildHeapGraph derefFuncM Nothing so
          annotateWithSource hg
  putStrLn $ ppHeapGraph (maybe "" show) hg

-- | Lookup the source location of THUNKs
annotateWithSource :: HeapGraph a -> DebugM (HeapGraph (Maybe SourceInformation))
annotateWithSource hg = traverseHeapGraph go2 hg
  where
    go2 (HeapGraphEntry a1 a2 _) = HeapGraphEntry a1 a2 <$> go a2
    go (ThunkClosure (StgInfoTableWithPtr i _) _ _) = request (RequestSourceInfo i)
    go _ = return Nothing

p20 e = do
  res <- pauseThen e $ request RequestAllBlocks
  print (length res)

-- request closures, using blocks
p21 e = pauseThen e $ do
  r <- request RequestRoots
  traceWrite (length r)
  forM_ r $ \c -> do
    traceWrite c
    dereferenceClosureFromBlock c

-- Use with large-thunk
p22 e = do
  run e $ request RequestPause
  runTrace e $ do
    precacheBlocks
    rs <- request RequestRoots
    s <- forM rs $ \r -> do
      traceWrite r
      c <- fullTraversalViaBlocks r
      traceWrite ("DONE", countNodes c)
      return $ countNodes c
    traceWrite (sum s)

-- Use with large-thunk
p24 e = do
  run e $ request RequestPause
  runTrace e $ do
    precacheBlocks
    rs <- request RequestRoots
    (hg, rs') <- case rs of
      [] -> error "Empty roots"
      (x:xs) -> multiBuildHeapGraph derefFuncM Nothing (x :| xs)
    case retainerSize hg of
      rs -> forM_ rs $ \r -> case r of Node n _ -> traceWrite n

p25 e = runTrace e $ precacheBlocks >>= traceWrite

p26 e = do
  run e $ request RequestPause
  runTrace e $ do
    precacheBlocks
    rs <- request RequestRoots
    (hg, rs') <- case rs of
      [] -> error "Empty roots"
      (x:xs) -> multiBuildHeapGraph derefFuncM Nothing (x :| xs)
    traceWrite (heapGraphSize hg)

p27 e = do
  run e $ request RequestPause
  runTrace e $ do
    precacheBlocks
    rs <- request RequestRoots
    traceFrom rs

p28 e = do
  p27 e
  run e $ request RequestResume
  threadDelay 1_000_000
  p28 e

p29 e = do
  run e $ request RequestPause
  r <- runTrace e $ do
    precacheBlocks
    rs <- request RequestRoots
    traceWrite (length rs)
    censusClosureType rs
  printCensusByClosureType r
  run e $ request RequestResume
  threadDelay 1_000_000
  p29 e


p30 e = profile 10_000_000 e

p31 e = analyseFragmentation 1_000_000 e

-- Given the roots and bad closures, find out why they are being retained
doAnalysis rs bad_ptrs = do
  let ptrs = snd (fst bad_ptrs)
      l = snd bad_ptrs
  rs <- findRetainers rs ptrs
  stack <- case rs of
    [] -> traceWrite "EMPTY RETAINERS" >> return Nothing
    (r:_) -> do
      cs <- dereferenceClosures r
      cs' <- mapM (quadtraverse pure dereferenceConDesc pure pure) cs
      locs <- mapM getSourceLoc cs'
      return $ Just (zip cs' locs)
  return ((l,) <$> stack)

analyseFragmentation :: Int -> DebugEnv DebugM -> IO ()
analyseFragmentation interval e = loop
  where
    loop ::IO ()
    loop = do
      threadDelay interval
      run e $ request RequestPause
      putStrLn "PAUSED"
      (mb_census, bs, rs) <- runTrace e $ do
        -- Get all known blocks
        bs <- precacheBlocks
        rs <- request RequestRoots
        traceWrite ("ROOTS", length rs)
        mb_census <- censusPinnedBlocks bs rs
        let bads = findBadPtrs mb_census
        -- Print how many objects there are in the badly fragmented blocks
        traceWrite ("FRAG_OBJECTS", (foldl1 (<>) (map (fst . fst) bads)))
        -- Only take 5 bad results as otherwise can take a long time as
        -- each call to `doAnalysis` will perform a full heap traversal.
        as <- mapM (doAnalysis rs) (take 5 bads)
        return (mb_census, bs, as)
      run e $ request RequestResume
      summariseBlocks bs
      outBlockCensus (Map.map fst mb_census)
      let disp (d, l) =
            maybe "nl" infoModule l ++ ":" ++ (ppClosure (maybe "" tdisplay l)  (\_ -> show) 0 . noSize $ d)
            where
              tdisplay sl = infoModule sl ++ ":" ++ infoPosition sl
          do_one k (l, stack) = do
            putStrLn (show k ++ "-------------------------------------")
            print l
            mapM (putStrLn . disp) stack
      zipWithM do_one [0..] (catMaybes rs)
      putStrLn "------------------------"
      loop

getSourceLoc c = do
  case lookupStgInfoTableWithPtr (noSize c) of
    infoTableWithptr -> request (RequestSourceInfo (tableId infoTableWithptr))

-- Testing the snapshot
p32 e = do
  run e $ request RequestPause
  runTrace e $ do
    precacheBlocks
    rs <- request RequestRoots
    traceFrom rs
    saveCache "/tmp/ghc-debug-cache"
    traceMsg "saved"
    loadCache "/tmp/ghc-debug-cache"
    traceMsg "loaded"

p33 e = forM [0..] $ \i -> do
  makeSnapshot e "/tmp/ghc-debug-cache"
  putStrLn ("CACHED: " ++ show i)
  threadDelay 1_000_000

p34 e = forM [0..] $ \i -> do
  run e $ request RequestPause
  res <- runTrace e $ do
    precacheBlocks
    rs <- request RequestRoots
    res <- sebAnalysis rs
    return res
  run e $ request RequestResume
  print i
  top10 <- printResult res
  {-
  -- Use this code if we are returning ClosurePtr not SourceInformation
  (hg, _) <- run e $ case top10 of
    [] -> error "None"
    (c:cs) -> multiBuildHeapGraph derefFuncM (Just 10) (c :| cs)
  let cs = map (flip GHC.Debug.Types.Graph.lookupHeapGraph hg) top10
  mapM print (zip top10 cs)
  putStrLn $ ppHeapGraph show hg
  -}
  threadDelay 10_000_000


p35 e = objectEquiv e

p36 e = do
  run e $ request RequestPause
  runTrace e $ do
    precacheBlocks
    rs <- request RequestRoots
    traceFrom rs
    traceFrom rs

p37 e = do
  run e $ request RequestPause
  cs <- runTrace e $ do
    precacheBlocks
    rs <- request RequestRoots
    count rs
  print cs


data TwoContext a = TwoContext a a
                | OneContext a
                | NoContext
                deriving Show

consContext a (TwoContext b c) = TwoContext a b
consContext a (OneContext b) = TwoContext a b
consContext a NoContext = OneContext a

printResult :: Show a => Map.Map a Count -> IO [a]
printResult m = do
  putStrLn $ "TOTAL: " ++ show total
  mapM_ show_line top10
  return (map fst top10)
  where
    show_line (k, Count v) = T.putStrLn (T.pack (show k) <> ": " <> T.pack (show v))
    top10 = take 10 $ reverse (sortBy (comparing snd) (Map.toList m))
    total = F.fold (Map.elems m)

-- | Analays the TyConApp[IND_STATIC, _] closures
benAnalysis :: [ClosurePtr] -> DebugM (Map.Map _ Count)
benAnalysis rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) NoContext (Map.empty)
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = visited
              , conDescTrace = const (return ())

            }

    -- Already visited the closure, but if it's still an IND_STATIC
    -- closure, we still want to count it (because we are counting
    -- TyConApp really) so call `closAccum`
    visited cp = do
      sc <- lift $ dereferenceClosureFromBlock cp
      closAccum cp sc (return ())

    -- First time we have visited a closure
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> (RWST (TwoContext _) () (Map.Map _ Count) DebugM) ()
               -> (RWST (TwoContext _) () (Map.Map _ Count) DebugM) ()
    closAccum cp sc k
      | (tipe (decodedTable (info (noSize sc)))) == IND_STATIC
      = do
          ctx <- ask
          case ctx of
            TwoContext ("ghc:GHC.Core.TyCo.Rep:TyConApp", a) p -> do
              loc <-  lift $ a
              --lift $ modify' (Map.insertWith (<>) cp (Count 1))
              modify' (Map.insertWith (<>) loc (Count 1))

              k
            OneContext p -> k
            _ -> k
      | otherwise = do
          s' <- lift $ quadtraverse pure dereferenceConDesc pure pure sc
          let ty = closureToKey (noSize s')
          local (consContext (ty, getSourceLoc s'))  k

-- | Analays the TyConApp[IND_STATIC, _] closures
sebAnalysis :: [ClosurePtr] -> DebugM (Map.Map _ Count)
sebAnalysis rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) () (Map.empty)
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }

    -- First time we have visited a closure
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> (RWST () () (Map.Map _ Count) DebugM) ()
               -> (RWST () () (Map.Map _ Count) DebugM) ()
    closAccum cp sc k = do
          s' <- lift $ quadtraverse pure dereferenceConDesc pure pure sc
          let ty = closureToKey (noSize s')
          when (ty == "ghc:GHC.Types.Demand:DmdType") $ do
            loc <- lift $ getSourceLoc sc
            modify' (Map.insertWith (<>) loc (Count 1))
          k

indStaticAnalysis :: [ClosurePtr] -> DebugM ()
indStaticAnalysis = undefined

