{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import GHC.Debug.Client
import GHC.Debug.Client.Retainers
import GHC.Debug.Client.Fragmentation
import GHC.Debug.Client.Profile
import GHC.Debug.Client.Dominators
import GHC.Debug.Client.Count
import GHC.Debug.Types.Graph (heapGraphSize, traverseHeapGraph, ppClosure)
--import GHC.Debug.Types.Closures
import GHC.Debug.Client.Trace
import GHC.Debug.Client.ObjectEquiv
import Control.Monad.RWS
import Control.Monad.Writer
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

--main = withDebuggeeConnect "banj" "/tmp/ghc-debug" (\(Debuggee e) -> p40 e  >> outputRequestLog e)

main = snapshotRun "/tmp/ghc-debug-cache" p39
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
p4 :: Debuggee -> IO ()
p5 :: Debuggee -> IO ()
p6 :: Debuggee -> IO ()
p7 :: Debuggee -> IO ()
p8 :: Debuggee -> IO ()
p16 :: Debuggee -> IO ()
p17 :: Debuggee -> IO ()
p19 :: Debuggee -> IO ()
p20 :: Debuggee -> IO ()
p21 :: Debuggee -> IO ()
p24 :: Debuggee -> IO ()
p25 :: Debuggee -> IO ()
p26 :: Debuggee -> IO ()
p27 :: Debuggee -> IO ()
p28 :: Debuggee -> IO ()
p29 :: Debuggee -> IO ()
p30 :: Debuggee -> IO ()
p31 :: Debuggee -> IO ()
p32 :: Debuggee -> IO ()
p33 :: Debuggee -> IO ()
p34 :: Debuggee -> IO ()
p35 :: Debuggee -> IO ()
p36 :: Debuggee -> IO ()
p37 :: Debuggee -> IO ()
p38 :: Debuggee -> IO ()
p39 :: Debuggee -> IO ()
p40 :: Debuggee -> IO ()


-- Testing get roots
p4 e = pauseThen e $ do
  gcRoots >>= traceWrite

-- request closures
p5 e = pauseThen e $ do
  r <- gcRoots
  traceWrite (length r)
  forM_ [0..length r - 1] $ \i -> do
    let cs = [r !! i]
    traceWrite cs
    dereferenceClosures cs

-- request all closures
p5a e = pauseThen e $ do
  rs <- gcRoots
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
  rs <- gcRoots
  dereferenceClosures rs





p6 e = do
  -- This blocks until a pause
  pausePoll e
  putStrLn "POLL"
  -- Should return already paused
  pause e
  putStrLn "PAUSE"
  -- Now unpause
  resume e
  putStrLn "RESUME"

-- Request saved objects
p7 e = pauseThen e $ do
  savedObjects >>= traceWrite

-- request saved objects
p8 e = pauseThen e $ do
  sos <- savedObjects
  traceWrite =<< dereferenceClosures sos

-- pretty-print graph
p16 e = do
  pause e
  hg <- run e $ do
          (so:_) <- savedObjects
          buildHeapGraph Nothing so
  putStrLn $ ppHeapGraph (const "") hg

-- Testing IPE
p17 e = do
  pause e
  runTrace e $ do
    [so] <- savedObjects
    c <- dereferenceClosure so
    let it = tableId (info (noSize c))
    traceWrite c
    traceWrite it
    traceWrite =<< getSourceInfo it


-- Use with large-thunk
p19 e = do
  pausePoll e
  hg <- run e $ do
          (so:_) <- savedObjects
          hg <- buildHeapGraph Nothing so
          annotateWithSource hg
  putStrLn $ ppHeapGraph (maybe "" show) hg

-- | Lookup the source location of THUNKs
annotateWithSource :: HeapGraph a -> DebugM (HeapGraph (Maybe SourceInformation))
annotateWithSource hg = traverseHeapGraph go2 hg
  where
    go2 (HeapGraphEntry a1 a2 _) = HeapGraphEntry a1 a2 <$> go a2
    go (ThunkClosure (StgInfoTableWithPtr i _) _ _) = getSourceInfo i
    go _ = return Nothing

p20 e = do
  res <- pauseThen e $ allBlocks
  print (length res)

-- request closures, using blocks
p21 e = pauseThen e $ do
  r <- gcRoots
  traceWrite (length r)
  forM_ r $ \c -> do
    traceWrite c
    dereferenceClosure c

-- Use with large-thunk
p24 e = do
  pause e
  runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    hg <- case rs of
      [] -> error "Empty roots"
      (x:xs) -> multiBuildHeapGraph  Nothing (x :| xs)
    case retainerSize hg of
      rs -> forM_ rs $ \r -> case r of Node n _ -> traceWrite n

p25 e = runTrace e $ precacheBlocks >>= traceWrite

p26 e = do
  pause e
  runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    hg <- case rs of
      [] -> error "Empty roots"
      (x:xs) -> multiBuildHeapGraph  Nothing (x :| xs)
    traceWrite (heapGraphSize hg)

p27 e = do
  pause e
  runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    traceFrom rs

p28 e = do
  p27 e
  resume e
  threadDelay 1_000_000
  p28 e

p29 e = do
  pause e
  r <- runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    traceWrite (length rs)
    censusClosureType rs
  printCensusByClosureType r
  resume e
  threadDelay 1_000_000
  p29 e


p30 e = profile 10_000_000 e

p31 e = analyseFragmentation 1_000_000 e

-- Given the roots and bad closures, find out why they are being retained
doAnalysis rs (l, ptrs) = do
  rs <- findRetainers (Just 10) rs ptrs
  stack <- case rs of
    [] -> traceWrite "EMPTY RETAINERS" >> return Nothing
    (r:_) -> do
      cs <- dereferenceClosures r
      cs' <- mapM (quadtraverse pure dereferenceConDesc pure pure) cs
      locs <- mapM getSourceLoc cs'
      return $ Just (zip cs' locs)
  return ((l,) <$> stack)

displayRetainerStack :: _
displayRetainerStack rs = do
      let disp (d, l) =
            maybe "nl" infoModule l ++ ":" ++ (ppClosure (maybe "" tdisplay l)  (\_ -> show) 0 . noSize $ d)
            where
              tdisplay sl = infoModule sl ++ ":" ++ infoPosition sl
          do_one k (l, stack) = do
            putStrLn (show k ++ "-------------------------------------")
            print l
            mapM (putStrLn . disp) stack
      zipWithM_ do_one [0..] (catMaybes rs)

analyseFragmentation :: Int -> Debuggee -> IO ()
analyseFragmentation interval e = loop
  where
    loop ::IO ()
    loop = do
      threadDelay interval
      pause e
      putStrLn "PAUSED"
      (mb_census, bs, rs) <- runTrace e $ do
        -- Get all known blocks
        bs <- precacheBlocks
        rs <- gcRoots
        traceWrite ("ROOTS", length rs)
        mb_census <- censusPinnedBlocks bs rs
        let bads = findBadPtrs mb_census
        -- Print how many objects there are in the badly fragmented blocks
        traceWrite ("FRAG_OBJECTS", (foldl1 (<>) (map (fst . fst) bads)))
        -- Only take 5 bad results as otherwise can take a long time as
        -- each call to `doAnalysis` will perform a full heap traversal.
        as <- mapM (doAnalysis rs) ([(l, ptrs) | ((c, ptrs), l) <- take 5 (bads)])
        return (mb_census, bs, as)
      resume e
      summariseBlocks bs
      outBlockCensus (Map.map fst mb_census)
      displayRetainerStack rs
      putStrLn "------------------------"
      loop

getSourceLoc c = getSourceInfo (tableId (info (noSize c)))

-- Testing the snapshot
p32 e = do
  pause e
  runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    traceFrom rs
    saveCache "/tmp/ghc-debug-cache"
    traceMsg "saved"
    loadCache "/tmp/ghc-debug-cache"
    traceMsg "loaded"

p33 e = forM_ [0..] $ \i -> do
  makeSnapshot e "/tmp/ghc-debug-cache"
  putStrLn ("CACHED: " ++ show i)
  threadDelay 1_000_000

p34 e = forM_ [0..] $ \i -> do
  pause e
  res <- runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    res <- sebAnalysis rs
    return res
  resume e
  print i
  top10 <- printResult res
  {-
  -- Use this code if we are returning ClosurePtr not SourceInformation
  (hg, _) <- run e $ case top10 of
    [] -> error "None"
    (c:cs) -> multiBuildHeapGraph  (Just 10) (c :| cs)
  let cs = map (flip GHC.Debug.Types.Graph.lookupHeapGraph hg) top10
  mapM print (zip top10 cs)
  putStrLn $ ppHeapGraph show hg
  -}
  threadDelay 10_000_000


p35 e = objectEquiv e

p36 e = do
  pause e
  runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    traceFrom rs
    traceFrom rs

p37 e = do
  pause e
  cs <- runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    count rs
  print cs

p38 e = do
  u <- pauseThen e $ unfoldingAnalysis
  printCensusByClosureType u

p39 e = do
  pause e
  (hg, rs) <- runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    (Biggest cp sc) <- bigBoyAnalysis rs
    hg <- multiBuildHeapGraph  (Just 10) (cp :| [])
    rs <- doAnalysis rs ("BIG", [cp])
    return (hg, rs)
  putStrLn $ ppHeapGraph show hg
  displayRetainerStack [rs]

p40 e = forM_ [0..] $ \i -> do
  pause e
  res <- runTrace e $ do
    precacheBlocks
    rs <- gcRoots
    res <- thunkAnalysis rs
    return res
  resume e
  print i
  top10 <- printResult res
  {-
  -- Use this code if we are returning ClosurePtr not SourceInformation
  (hg, _) <- run e $ case top10 of
    [] -> error "None"
    (c:cs) -> multiBuildHeapGraph  (Just 10) (c :| cs)
  let cs = map (flip GHC.Debug.Types.Graph.lookupHeapGraph hg) top10
  mapM print (zip top10 cs)
  putStrLn $ ppHeapGraph show hg
  -}
  threadDelay 10_000_000




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
    top10 = take 100 $ reverse (sortBy (comparing snd) (Map.toList m))
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
      sc <- lift $ dereferenceClosure cp
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
thunkAnalysis :: [ClosurePtr] -> DebugM (Map.Map _ Count)
thunkAnalysis rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) () (Map.empty)
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
          case (noSize sc) of
            ThunkClosure {} ->  do
              loc <- lift $ getSourceLoc sc
              modify' (Map.insertWith (<>) loc (Count 1))
            _ -> k

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


-- | 1. Find all CoreUnfolding closures.
-- 2. Perform a census of everything they retain.
unfoldingAnalysis :: DebugM CensusByClosureType
unfoldingAnalysis = do
  rroots <- gcRoots
  precacheBlocks
  unfolding_ptrs <- findUnfoldings rroots
  traceWrite (length unfolding_ptrs)
  count rroots >>= traceWrite
  count unfolding_ptrs >>= traceWrite
  censusClosureType unfolding_ptrs



findUnfoldings rroots = execStateT (traceFromM funcs rroots) []
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }

    closAccum  :: ClosurePtr
               -> SizedClosure
               -> StateT [ClosurePtr] DebugM ()
               -> StateT [ClosurePtr] DebugM ()
    closAccum cp sc k = do
          s' <- lift $ quadtraverse pure dereferenceConDesc pure pure sc
          let ty = closureToKey (noSize s')
          when (ty == "ghc:GHC.Core:CoreUnfolding") $ do
            modify' (cp :)
          k


data Biggest = Biggest ClosurePtr SizedClosure | NoBiggest

instance Monoid Biggest where
    mempty = NoBiggest

instance Semigroup Biggest where
  (Biggest cp sc) <> (Biggest cp1 sc1) = if dcSize sc > dcSize sc1 then Biggest cp sc
                                                                   else Biggest cp1 sc1
  NoBiggest <> b = b
  b <> NoBiggest = b

-- What is the biggest closure?
-- This is probably going to be something like an ARR_WORDS closure usually
bigBoyAnalysis rroots = execStateT (traceFromM funcs rroots) NoBiggest
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }

    closAccum  :: ClosurePtr
               -> SizedClosure
               -> StateT Biggest DebugM ()
               -> StateT Biggest DebugM ()
    closAccum cp sc k = do
      modify' (Biggest cp sc <>)
      k

