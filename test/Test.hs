{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import GHC.Debug.Client
import GHC.Debug.Retainers
import GHC.Debug.Fragmentation
import GHC.Debug.Profile
import GHC.Debug.Dominators
import GHC.Debug.Snapshot
import GHC.Debug.Count
import GHC.Debug.TypePointsFrom
import GHC.Debug.Types.Ptr
import GHC.Debug.Types.Graph (heapGraphSize, traverseHeapGraph, ppClosure)
--import GHC.Debug.Types.Closures
import GHC.Debug.Trace
import GHC.Debug.ParTrace(tracePar)
import GHC.Debug.ObjectEquiv
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
import GHC.Debug.Client.Monad

import Data.List.Extra (trim)
import System.Process
import Data.Tree
import Data.Maybe
import qualified Data.Map as Map
import Data.Ord
import Data.List
import qualified Data.Set as S
import Data.List.NonEmpty(NonEmpty(..), fromList)
import Data.Semigroup
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

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

--main = withDebuggeeConnect "/tmp/ghc-debug" (\e -> p50 e) --  >> outputRequestLog e)

--main = snapshotRun "/tmp/ghc-debug-cache" p37
--
main = snapshotRun "/tmp/ghc-debug-cache" p37

{-
main = do
    --before <- snapshotInit "/tmp/ghc-debug-cache-baseline-hasura"
    --after <- snapshotInit "/tmp/ghc-debug-cache-after-hasura"
    after <- snapshotInit "/tmp/ghc-debug-cache"
--    p44ec before after
    --p44c2 after
    --p44i after
    --p44c4 before
    putStrLn "hello"
    p44d after
    --analyseFragmentation 10 before
    --analyseFragmentation 10 after
    -}

printCensusByClosureType = writeCensusByClosureType "profile/profile_out.txt"

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
p41 :: Debuggee -> IO ()
p42 :: Debuggee -> IO ()
p43 :: Debuggee -> IO ()
--p44 :: Debuggee -> IO ()
p45 :: Debuggee -> IO ()


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
    go c = getSourceInfo (tableId $ info c)

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
  hg <- runTrace e $ do
    precacheBlocks
    rs <- savedObjects
    hg <- case rs of
      [] -> error "Empty roots"
      (x:xs) -> multiBuildHeapGraph (Just 20) (x :| xs)
    return hg
  putStrLn $ ppHeapGraph show hg
--    case retainerSize hg of
--      rs -> forM_ rs $ \r -> case r of Node n _ -> traceWrite n

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
  resume e
  printCensusByClosureType r
  threadDelay 5_000_000
  p29 e


p30 e = profile "profile/profile_out.txt" 10_000_000 e

p31 e = analyseFragmentation 5_000_000 e

-- Given the roots and bad closures, find out why they are being retained
doAnalysis rs (l, ptrs) = do
  rs <- findRetainersOf (Just 1) rs ptrs
  stack <- case rs of
    [] -> traceWrite "EMPTY RETAINERS" >> return Nothing
    (r:_) -> do
      cs <- dereferenceClosures r
      cs' <- mapM (quadtraverse pure dereferenceConDesc pure pure) cs
      locs <- mapM getSourceLoc cs'
      return $ Just (zip cs' locs)
  return ((l,) <$> stack)

displayRetainerStack :: [(String, [(SizedClosureC, Maybe SourceInformation)])] -> IO ()
displayRetainerStack rs = do
      let disp (d, l) =
            maybe "nl" tdisplay l ++ ":" ++ (ppClosure ""  (\_ -> show) 0 . noSize $ d)
            where
              tdisplay sl = infoType sl ++ ":" ++ infoModule sl ++ ":" ++ infoPosition sl
          do_one k (l, stack) = do
            putStrLn (show k ++ "-------------------------------------")
            print l
            mapM (putStrLn . disp) stack
      zipWithM_ do_one [0..] rs

analyseFragmentation :: Int -> Debuggee -> IO ()
analyseFragmentation interval e = loop
  where
    loop ::IO ()
    loop = do
      pause e
      putStrLn "PAUSED"
      (mb_census, mbb_census, mbb_census2, cen, bs, rs, rets) <- runTrace e $ do
        -- Get all known blocks
        bs <- precacheBlocks
        rs <- gcRoots
        traceWrite ("ROOTS", length rs)
        mb_census <- censusPinnedBlocks bs rs
        mbb_census <- censusByMBlock rs
        mbb_census2 <- censusByBlock rs
        let is_small (CS _ (Size s) _) = fromIntegral s < 4096 * 0.9
        let small_blocks = S.fromList (Map.keys (Map.filter is_small mbb_census2))
        let pred cp = applyBlockMask cp `S.member` small_blocks
        cen <- censusClosureTypeF (not . pred) rs
        rets <- findRetainers (Just 10) rs (\cp _ -> return $ pred cp)
        rets' <- traverse addLocationToStack rets
        let bads = findBadPtrs mb_census
        -- Print how many objects there are in the badly fragmented blocks
        traceWrite ("FRAG_OBJECTS", (foldl1 (<>) (map (fst . fst) bads)))
        -- Only take 5 bad results as otherwise can take a long time as
        -- each call to `doAnalysis` will perform a full heap traversal.
        as <- mapM (doAnalysis rs) ([(l, ptrs) | ((c, ptrs), l) <- take 5 (bads)])
        return (mb_census, mbb_census, mbb_census2, cen, bs, as, rets')
      resume e
      summariseBlocks bs
      let go (PinnedCensusStats (m, _)) = m
      printBlockCensus (Map.map go mb_census)
      printMBlockCensus mbb_census
      printBlockCensus mbb_census2
      printMBlockCensus cen
      --displayRetainerStack (("one",) <$> rets)

--      displayRetainerStack (catMaybes rs)
      putStrLn "------------------------"
      --loop

censusClosureTypeF :: (ClosurePtr -> Bool) -> [ClosurePtr] -> DebugM _
censusClosureTypeF p = closureCensusBy go
  where
    go :: ClosurePtr -> SizedClosure
       -> DebugM (Maybe (BlockPtr, CensusStats))
    go cp s | p cp = do
      d <- quadtraverse pure dereferenceConDesc pure pure s
      let siz :: Size
          siz = dcSize d
          v =  mkCS siz
      return $ Just (applyMBlockMask cp, v)
    go _ _ = return Nothing

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
  getLine
  makeSnapshot e "/tmp/ghc-debug-cache"
  putStrLn ("CACHED: " ++ show i)
  --threadDelay 1_000_000

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


p35 e = do
  objectEquiv e
  --stacks <- run e $ traverse addLocationToStack rs
  --displayRetainerStack (("one",) <$> stacks)

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
    _ <- precacheBlocks
    rs <- gcRoots
    c <- parCount rs
    --mbb_census <- censusByMBlock rs
    return c
  print cs
  --printMBlockCensus mb

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
  putStrLn $ ppHeapGraph (show . getSize) hg
  displayRetainerStack (catMaybes [rs])


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
            TwoContext ("graphql-engine-1.0.0-inplace:Hasura.GraphQL.Parser.Schema:Definition", a) p -> do
              loc <-  lift $ a
              --lift $ modify' (Map.insertWith (<>) cp (Count 1))
              modify' (Map.insertWith (<>) cp (Count 1))

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
              k
            _ -> k

-- | Analays the TyConApp[IND_STATIC, _] closures
arrWordsAnalysis :: [ClosurePtr] -> DebugM (Map.Map _ Count)
arrWordsAnalysis rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) () (Map.empty)
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
            ArrWordsClosure _ _ p ->  do
              modify' (Map.insertWith (<>) (arrWordsBS p) (Count 1))
              k
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
          --when (ty == "graphql-engine-1.0.0-inplace:Hasura.GraphQL.Parser.Internal.Parser:InputFieldsParser") $ do
--          when (ty == "graphql-engine-1.0.0-inplace:Hasura.GraphQL.Parser.Schema:Definition") $ do
--          when (ty == "ghc-prim:GHC.Tuple:(,)") $ do
--          when (ty == "text-1.2.4.0:Data.Text.Internal:Text") $ do
          when (ty == "graphql-engine-1.0.0-inplace:Hasura.GraphQL.Parser.Internal.Parser:Parser") $ do
            loc <- lift $ getSourceLoc sc
            let it = info (noSize sc)
            modify' (Map.insertWith (<>) (maybe (Left it) Right loc) (Count 1))
          k

getDescriptionName :: ClosurePtr -> SizedClosure -> DebugM (Maybe ByteString)
getDescriptionName cp sc = do
  case noSize sc of
    ConstrClosure _ ps _ cd -> do
      cd' <- dereferenceConDesc cd
      case cd' of
        ConstrDesc _ "Hasura.GraphQL.Parser.Schema" "Definition" -> do
          -- First pointer is a String pointer
          Just <$> follow (head ps)
        _ -> return Nothing
    _ -> return Nothing
  where
        follow p = do
          sc' <- noSize <$> (dereferenceClosure p)
          case sc' of
            IndClosure _ p -> follow p
            ConstrClosure _ ps _ cd -> do
              s <- noSize <$> (dereferenceClosure (head ps))
              case s of
                ArrWordsClosure _ _ ws -> do
                  return (arrWordsBS ws)
                _ -> error "No ArrWords"

            c -> do
              c' <- quadtraverse pure dereferenceConDesc pure pure c
              error (ppClosure "" (\_ _ -> "") 0 c')

-- | Analays the TyConApp[IND_STATIC, _] closures
descriptionNames :: [ClosurePtr] -> DebugM (Map.Map _ Count)
descriptionNames rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) () (Map.empty)
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
      mname <- lift $ getDescriptionName cp sc

      case mname of
        Nothing -> k
        Just bs ->  do
          loc <- lift $ getSourceLoc sc
          modify' (Map.insertWith (<>) (bs, loc) (Count 1))
          k



-- | Analays the TyConApp[IND_STATIC, _] closures
getAllPtrs :: [ClosurePtr] -> DebugM [(ClosurePtr, ByteString)]
getAllPtrs rroots = (\(_, (_, r), _) -> r) <$> runRWST (traceFromM funcs rroots) () (0, [])
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
               -> (RWST () () (Int, [(ClosurePtr, ByteString)]) DebugM) ()
               -> (RWST () () (Int, [(ClosurePtr, ByteString)]) DebugM) ()
    closAccum cp sc k = do
          s' <- lift $ quadtraverse pure dereferenceConDesc pure pure sc
          (!n, cs) <- get
          mname <- lift $ getDescriptionName cp sc
          case mname of
            Nothing -> k
            Just name | "ny_court_nyscef" == (BS.filter (/= 0) name)
              -> do
                let new = (n + 1, (cp, BS.filter (/=0) name) : cs)
                put new
                k
            Just {} -> k
          {-
          let ty = closureToKey (noSize s')
          --when (ty == "graphql-engine-1.0.0-inplace:Hasura.GraphQL.Parser.Internal.Parser:InputFieldsParser") $ do
          when (ty == "graphql-engine-1.0.0-inplace:Hasura.GraphQL.Parser.Schema:Definition") $ do
            let new = (n + 1, cp : cs)
            put new
          k
            {-
          if n >= 1000000
            then return ()
            else k
            -}
            -}


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
  census2LevelClosureType unfolding_ptrs



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


addLocationToStack r = do
  cs <- dereferenceClosures r
  cs' <- mapM (quadtraverse pure dereferenceConDesc pure pure) cs
  locs <- mapM getSourceLoc cs'
  return $ (zip cs' locs)

p41 e = do
  pause e
  stacks <- runTrace e $ do
    precacheBlocks
    roots <- gcRoots
    rs <- modIface roots
    traverse (\c -> (show (head c),) <$> (addLocationToStack c)) rs
  displayRetainerStack stacks

p42 e = do
  pause e
  (res, hg) <- runTrace e $ do
    precacheBlocks
    roots <- gcRoots
    rs <- findTcModResult roots
    res <- thunkAnalysis rs
    hg <- case rs of
      [] -> error "none"
      (r:rs) -> multiBuildHeapGraph  (Just 7) (r :| rs)
    return (res, hg)

  resume e
  printResult res
  putStrLn $ ppHeapGraph (show . getSize) hg

p43 e = do
  pause e
  runTrace e $ do
    _ <- precacheBlocks
    rs <- gcRoots
    tracePar rs

p44 e = do
  pause e
  c <- runTrace e $ do
    bs <- precacheBlocks
    ro <- savedObjects
    censusClosureType ro
  printCensusByClosureType c

p44a e = do
  pause e
  c <- runTrace e $ do
    bs <- precacheBlocks
    rs <- savedObjects
    count rs
  print c

p44h e = do
  pause e
  c <- runTrace e $ do
    bs <- precacheBlocks
    rs <- savedObjects
    arrWordsAnalysis rs
  printResult c

p44b e = do
  pause e
  cs <- runTrace e $ do
    bs <- precacheBlocks
    rs <- savedObjects
    ps <- map fst <$> getAllPtrs rs
    mapM (\c -> (c,) <$> count [c]) ps
  let Size m = (maximum (map (cssize . snd) cs))
  print m
  histogram (fromIntegral m) (map snd cs)
  let big = filter (\(c, s) -> cssize s >= Size m) cs
  (hg, res) <- runTrace e $ do
--          h1 <- multiBuildHeapGraph (Just 10) (fromList $ map fst big)
--          annotateWithSource h1
          (,) <$> census2LevelClosureType (map fst big) <*> arrWordsAnalysis (map fst big)
  printCensusByClosureType hg
  printResult res
  --putStrLn $ ppHeapGraph (maybe  "" show) hg

p44c e = do
  pause e
  stacks <- runTrace e $ do
    bs <- precacheBlocks
    rs <- gcRoots
    ss <- roleContext rs
    traverse (\c -> (show (head c),) <$> (addLocationToStack c)) ss
  displayRetainerStack stacks

p44c2 e = do
  pause e
  stacks <- runTrace e $ do
    bs <- precacheBlocks
    rs <- gcRoots
    ss <- buildOutputs rs
    traverse (\c -> (show (head c),) <$> (addLocationToStack c)) ss
  displayRetainerStack stacks

p44c3 e = do
  pause e
  stacks <- runTrace e $ do
    bs <- precacheBlocks
    rs <- gcRoots
    so <- savedObjects
    ss <- hashTuples (rs \\ so)
    traverse (\c -> (show (head c),) <$> (addLocationToStack c)) ss
  displayRetainerStack stacks


p44c4 e = do
  pause e
  stacks <- runTrace e $ do
    bs <- precacheBlocks
    rs <- gcRoots
    ss <- thunks_f rs
    traverse (\c -> (show (head c),) <$> (addLocationToStack c)) ss
  displayRetainerStack stacks

p44d e = do
  pause e
  (!c, !indiv_c, !r, !stacks, names, a, tpf) <- runTrace e $ do
    bs <- precacheBlocks
    rs <- gcRoots
    ps <- getAllPtrs rs
    traceWrite (length ps)
    -- Total size, with sharing
    indiv_c_raw <-sortBy (comparing (\(_, _, cs) -> cssize cs)) <$> mapM (\(c, d) -> (c,d,) <$> count [c]) ps
    let n = 4
    let indiv_c = take n $ reverse indiv_c_raw

    c <- count [cp | (cp, _, _) <- indiv_c ]

    traceWrite "DONE2"
    let comb (s, cs) (s', cs') = (max s s', cs <> cs')
    r <- census2LevelClosureType [cp | (cp, _, _) <- indiv_c]

    ss <- findRetainersOf (Just n) rs [cp | (cp, _,_) <- indiv_c]
    names <- descriptionNames [cp | (cp, _, _) <- indiv_c ]
    stack <- traverse (\c -> (show (head c),) <$> (addLocationToStack c)) ss
    a <- sebAnalysis [cp | (cp, _, _) <- indiv_c]

    tpf <- typePointsFrom rs --[cp | (cp, _, _) <- indiv_c]
    let es = Map.toList (getEdges tpf)

    let getKey :: InfoTablePtr -> DebugM String
        getKey itblp = do
          loc <- getSourceInfo itblp
          itbl <- dereferenceInfoTable itblp
          case loc of
            Nothing -> getKeyFallback itblp itbl
            Just s -> return $ show (tipe itbl) ++ ":" ++ renderSourceInfo s

        renderSourceInfo :: SourceInformation -> String
        renderSourceInfo s = (infoName s ++ ":" ++ infoType s ++ ":" ++ infoPosition s)

        getKeyFallback itbp itbl = do
          case tipe itbl of
            t | CONSTR <= t && t <= CONSTR_NOCAF   -> do
              ConstrDesc a b c <- dereferenceConDesc itbp
              return $ a ++ ":" ++ b ++ ":" ++ c
            _ -> return $ show (tipe itbl)
    es' <- mapM (\(Edge e1 e2, cs) -> (,cs) . T.pack . show <$> ((,) <$> getKey e1 <*> getKey e2)) es


    return (c, indiv_c,  r, stack, names, a, Map.fromList es')

  printResult names
  printResult a
  displayRetainerStack stacks
  printCensusByClosureType r
  printCensusByClosureType tpf
  print c
  mapM print indiv_c
  -- Heuristic to choose candidates which are probably big, based on their
  -- height
--  let high_dups = take 100 (reverse $ sortBy (comparing (getCHeight . snd)) d)
  -- This doesn't memoise any size calculation so can be expensive to run a
  -- few times.
--  savings <- (runTrace e $ mapM savings2 high_dups)

p44ec before after = do
  before_census <- runTrace before $ do
                      precacheBlocks
                      gcRoots >>= count
  after_census  <- runTrace after $ do
                      precacheBlocks
                      gcRoots >>= count
  let diff_census = (\cs1 cs2 -> Just (CS (cscount cs1 - cscount cs2) (cssize cs1 - cssize cs2) (Max 0)))
  print before_census
  print after_census
  print (diff_census after_census before_census)

p44e before after = do
  before_census <- runTrace before $ do
                      precacheBlocks
                      gcRoots >>= censusClosureType
  after_census  <- runTrace after $ do
                      precacheBlocks
                      gcRoots >>= censusClosureType
  let diff_census = Map.differenceWith (\cs1 cs2 -> Just (CS (cscount cs1 - cscount cs2) (cssize cs1 - cssize cs2) (Max 0))) after_census before_census
  printCensusByClosureType diff_census

p44f before after = do
  before_census <- runTrace before $ do
                      precacheBlocks
                      gcRoots >>= census2LevelClosureType
  after_census  <- runTrace after $ do
                      precacheBlocks
                      gcRoots >>= census2LevelClosureType
  let diff_census = Map.differenceWith (\cs1 cs2 -> Just (CS (cscount cs1 - cscount cs2) (cssize cs1 - cssize cs2) (Max 0))) after_census before_census
  printCensusByClosureType diff_census

p44g after = do
  res <- runTrace after $ do
    precacheBlocks
    gcRoots >>= descriptionNames
  printResult res

{-
p44i after = do
  res <- runTrace after $ do
    precacheBlocks
    gcRoots >>= descriptionNamesHash
  let ks = Map.keys res
  g <- runTrace after $ do
          hg <- multiBuildHeapGraph (Just 10) (fromList ks)
          annotateWithSource hg
  let disp  s = infoType s ++ ", " ++ infoModule s ++ "," ++ infoPosition s
  putStrLn $ ppHeapGraph (maybe "" disp) g
  -}


p45 e = do
  pause e
  t <- runTrace e $ do
    _ <- precacheBlocks
    rs <- gcRoots
    typePointsFrom rs
  --printCensusByClosureType (getNodes t)
  print (Map.size (getNodes t))
  print (Map.size (getEdges t))
  mapM_ print (take 10 (reverse $ sortBy (comparing (cssize . snd)) (Map.assocs (getEdges t))))

p46 e = detectLeaks 1 e

p50 e = do
  pause e
  run e $ snapshot "/tmp/ghc-debug-cache"

p51 e = fork e >> threadDelay 100000000

modIface rroots = findRetainers (Just 100) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _ _  cname <- dereferenceConDesc cd
          return $ cname == "HomeModInfo"
        _ -> return $ False

tcModResult rroots = findRetainers (Just 10) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _ _  cname <- dereferenceConDesc cd
          return $ cname == "TcModuleResult"
        _ -> return $ False

splitEnv rroots = findRetainers (Just 10) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _ _  cname <- dereferenceConDesc cd
          return $ cname == "MkSplitUniqSupply"
        _ -> return $ False

dmaps rroots = findRetainers (Just 10) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _  mname cname <- dereferenceConDesc cd
          return $ mname == "Data.Dependent.Map.Internal" && (cname == "Tip" || cname == "Bin")
        _ -> return $ False

roleContext rroots = findRetainers (Just 2) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _  mname cname <- dereferenceConDesc cd
          return $ mname == "Hasura.RQL.DDL.Schema.Cache.Common" && (cname == "RebuildableSchemaCache")
        _ -> return $ False

buildOutputs rroots = findRetainers (Just 2) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _  mname cname <- dereferenceConDesc cd
          return $ mname == "Hasura.RQL.DDL.Schema.Cache.Common" && (cname == "BuildOutputs")
        _ -> return $ False

hashTuples rroots = findRetainers (Just 100) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _  mname cname <- dereferenceConDesc cd
          loc <- getSourceLoc sc
          return $ (cname == "(,)" && ((infoLabel <$> loc) == Just "pruneDanglingDependents"))
        _ -> return $ False

enumTuples rroots = findRetainers (Just 100) rroots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _  mname cname <- dereferenceConDesc cd
          loc <- getSourceLoc sc
--          return $ (cname == "(,)" && ((infoLabel <$> loc) == Just "tableSelectColumnsEnum"))
          return $ (cname == "InputFieldsParser" && ((infoLabel <$> loc) == Just "orderByExp"))
        _ -> return $ False

thunks_f rroots = findRetainers (Just 100) rroots go
  where
    go cp sc =
      case noSize sc of
        ThunkClosure {} -> do
          loc <- getSourceLoc sc
          return $ (((infoName <$> loc) == Just "f_info"))
        _ -> return $ False

findTcModResult rroots = execStateT (traceFromM funcs rroots) []
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
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _ _  cname <- lift $ dereferenceConDesc cd
          if (cname == "TcModuleResult")
            then modify' (cp :) >> k
            else k
        _ -> k




