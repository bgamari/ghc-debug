{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
module GHC.Debug.Client
  ( -- * Running/Connecting to a debuggee
    Debuggee(..)
  , debuggeeRun
  , debuggeeConnect
  , debuggeeClose
  , withDebuggeeRun
  , withDebuggeeConnect
  , socketDirectory

    -- * Pause/Resume
  , pause
  , resume
  , pausePoll
  , withPause

    -- * Querying the paused debuggee
  , rootClosures
  , savedClosures

    -- * Closures
  , Closure
  , closureShowAddress
  , closureExclusiveSize
  , closureRetainerSize
  , closureSourceLocation
  , SourceInformation(..)
  , closureReferences
  , closurePretty
  , fillConstrDesc

    -- * Common initialisation
  , initialTraversal
  , HG.HeapGraph(..)
    -- * Dominator Tree
  , dominatorRootClosures
  , closureDominatees
  , runAnalysis
  , Analysis(..)
  , Size(..)
  , RetainerSize(..)
    -- * Reverse Edge Map
  , HG.mkReverseGraph
  , reverseClosureReferences
  , lookupHeapGraph

  -- * Types
  , ConstrDesc(..)
  , ConstrDescCont
  , GenPapPayload(..)
  , StackCont
  , PayloadCont
  , ClosurePtr
  , HG.StackHI
  , HG.PapHI
  , HG.HeapGraphIndex
    --
    -- $dominatorTree

    -- * All this stuff feels too low level to be exposed to the frontend, but
    --   still could be used for tests.
  , pauseThen -- This feels odd. Like we should just have a better choice of monad
  , request
  , Request(..)
  , getInfoTblPtr
  , decodeClosure
  , FieldValue(..)
  , decodeInfoTable
  , lookupInfoTable
  , DebugClosure(..)
  , Ptr(..)
  , toPtr
  , dereferencePtr
  , dereferenceClosures
  , dereferenceClosure
  , dereferenceSizedClosure
  , dereferenceClosureFromBlock
  , dereferenceStack
  , dereferencePapPayload
  , dereferenceConDesc
  , fullTraversal
  , fullTraversalViaBlocks
  , Quadtraversable(..)
  , precacheBlocks
  , traceFrom
  , DebugEnv
  , DebugM
  ) where

import           Control.Exception
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Graph as G
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified GHC.Debug.Types as GD
import           GHC.Debug.Types hiding (Closure, DebugClosure)
import           GHC.Debug.Decode
import           GHC.Debug.Decode.Stack
import           GHC.Debug.Convention (socketDirectory)
import           GHC.Debug.Client.Monad (DebugEnv, DebugM, request, requestBlock, run)
import qualified GHC.Debug.Client.Monad as GD
import           GHC.Debug.Client.BlockCache
import qualified GHC.Debug.Types.Graph as HG
import qualified Data.IntSet as IS
import qualified Data.HashMap.Strict as HM
import Control.Monad.State
import Data.Tree

import Debug.Trace


data Debuggee = Debuggee
  { debuggeeEnv :: DebugEnv DebugM
  }

data Analysis = Analysis
  { analysisDominatorRoots :: ![ClosurePtr]
  , analysisDominatees :: !(ClosurePtr -> [ClosurePtr])
  -- ^ Unsorted dominatees of a closure
  , analysisSizes :: !(ClosurePtr -> (Size, RetainerSize))
  -- ^ Size and retainer size (via dominator tree) of closures
  }

mkDebuggee :: DebugEnv DebugM -> Debuggee
mkDebuggee e = Debuggee e

initialTraversal :: Debuggee -> IO (HG.HeapGraph Size)
initialTraversal (Debuggee e) = run e $ do
    -- Calculate the dominator tree with retainer sizes
    -- TODO perhaps this conversion to a graph can be done in GHC.Debug.Types.Graph
    precacheBlocks
    rs <- request RequestRoots
    let derefFuncM cPtr = do
          c <- dereferenceClosureFromBlock cPtr
          quadtraverse dereferencePapPayload dereferenceConDesc dereferenceStack pure c
    (hg, _) <- case rs of
      [] -> error "Empty roots"
      (x:xs) -> HG.multiBuildHeapGraph derefFuncM (x :| xs)
    return hg

-- This function is very very very slow, it needs to be optimised.
runAnalysis :: Debuggee -> HG.HeapGraph Size -> IO Analysis
runAnalysis (Debuggee e) hg = run e $ do
    let drs :: [G.Tree (ClosurePtr, (Size, RetainerSize))]
        drs = fmap (\ent -> (HG.hgeClosurePtr ent, HG.hgeData ent)) <$> HG.retainerSize hg

        !hmGraph = HM.unions (map snd $ foldTree buildGraphNode <$> (HG.retainerSize hg))

        buildGraphNode :: HG.HeapGraphEntry v
                       -> [(ClosurePtr, HM.HashMap ClosurePtr (v, [ClosurePtr]))]
                       -> (ClosurePtr, HM.HashMap ClosurePtr (v, [ClosurePtr]))
        buildGraphNode hge subtrees =
            (cptr, HM.insert cptr v (HM.unions submaps))
          where
            cptr = HG.hgeClosurePtr hge
            v = (HG.hgeData hge, children)
            (children, submaps) = unzip subtrees

        cPtrToData
          = fromMaybe ((-12221, RetainerSize (-12221)), [])
          -- ^ TODO I would expect the mapping to be complete unless out analysis misses some closures.
          . flip HM.lookup hmGraph

    return $ Analysis
              [drPtr | G.Node (drPtr, _) _ <- drs]
              ((\(_,x) -> x) . cPtrToData)
              ((\(x,_) -> x) . cPtrToData)

-- | Bracketed version of @debuggeeRun@. Runs a debuggee, connects to it, runs
-- the action, kills the process, then closes the debuggee.
withDebuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
                -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                -> (Debuggee -> IO a)
                -> IO a
withDebuggeeRun exeName socketName action = GD.withDebuggeeRun exeName socketName (\e -> action (mkDebuggee e))

-- | Bracketed version of @debuggeeConnect@. Connects to a debuggee, runs the
-- action, then closes the debuggee.
withDebuggeeConnect :: FilePath  -- ^ executable name of the debuggee
                   -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                   -> (Debuggee -> IO a)
                   -> IO a
withDebuggeeConnect exeName socketName action = GD.withDebuggeeConnect exeName socketName (\e -> action (mkDebuggee e))

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
            -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
            -> IO Debuggee
debuggeeRun exeName socketName = mkDebuggee <$> GD.debuggeeRun exeName socketName

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeConnect :: FilePath  -- ^ path to executable to run as the debuggee
                -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                -> IO Debuggee
debuggeeConnect exeName socketName = mkDebuggee <$> GD.debuggeeConnect exeName socketName

-- | Close the connection to the debuggee.
debuggeeClose :: Debuggee -> IO ()
debuggeeClose = GD.debuggeeClose . debuggeeEnv

-- | Pause the debuggee
pause :: Debuggee -> IO ()
pause (Debuggee e) = do
  run e $ request RequestPause

resume :: Debuggee -> IO ()
resume (Debuggee e) = run e $ request RequestResume

-- | Like pause, but wait for the debuggee to pause itself. It currently
-- impossible to resume after a pause caused by a poll.?????????? Is that true???? can we not just call resume????
pausePoll :: Debuggee -> IO ()
pausePoll (Debuggee e) = do
  run e $ request RequestPoll

-- | Bracketed version of pause/resume.
withPause :: Debuggee -> IO a -> IO a
withPause dbg act = bracket_ (pause dbg) (resume dbg) act

-- | Request the debuggee's root pointers.
rootClosures :: Debuggee -> IO [Closure]
rootClosures (Debuggee e) = run e $ do
  closurePtrs <- request RequestRoots
  closures <- dereferenceClosures closurePtrs
  return [ Closure closurePtr' closure
            | closurePtr' <- closurePtrs
            | closure <- closures
            ]

-- | A client can save objects by calling a special RTS method
-- This function returns the closures it saved.
savedClosures :: Debuggee -> IO [Closure]
savedClosures (Debuggee e) = run e $ do
  closurePtrs <- request RequestSavedObjects
  closures <- dereferenceClosures closurePtrs
  return $ zipWith Closure
            closurePtrs
            closures

-- -- | Request the description for an info table.
-- -- The `InfoTablePtr` is just used for the equality
-- requestConstrDesc :: Debuggee -> PayloadWithKey InfoTablePtr ClosurePtr -> IO ConstrDesc
-- requestConstrDesc (Debuggee e _) = run e $ request RequestConstrDesc

-- -- | Lookup source information of an info table
-- requestSourceInfo :: Debuggee -> InfoTablePtr -> IO [String]
-- requestSourceInfo (Debuggee e _) = run e $ request RequestSourceInfo

-- -- | Request a set of closures.
-- requestClosures :: Debuggee -> [ClosurePtr] -> IO [RawClosure]
-- requestClosures (Debuggee e _) = run e $ request RequestClosures

type Closure = DebugClosure PayloadCont ConstrDescCont StackCont ClosurePtr

data DebugClosure p cd s c
  = Closure
    { _closurePtr :: ClosurePtr
    , _closureSized :: DebugClosureWithSize p cd s c
    }
  | Stack
    { _stackPtr :: StackCont
    , _stackStack :: GD.GenStackFrames c
    }

toPtr :: DebugClosure p cd s c -> Ptr
toPtr (Closure cp _) = CP cp
toPtr (Stack sc _)   = SP sc

data Ptr = CP ClosurePtr | SP StackCont

dereferencePtr :: Debuggee -> Ptr -> IO (DebugClosure PayloadCont ConstrDescCont StackCont ClosurePtr)
dereferencePtr (Debuggee dbg) (CP cp) = run dbg (Closure <$> pure cp <*> dereferenceSizedClosure cp)
dereferencePtr (Debuggee dbg) (SP sc) = run dbg (Stack <$> pure sc <*> dereferenceStack sc)

instance Quadtraversable DebugClosure where
  quadtraverse p f g h (Closure cp c) = Closure cp <$> quadtraverse p f g h c
  quadtraverse _ _ _ h (Stack sp s) = Stack sp <$> traverse h s

closureShowAddress :: DebugClosure p cd s c -> String
closureShowAddress (Closure c _) = show c
closureShowAddress (Stack   s _) = show s

-- | Get the exclusive size (not including any referenced closures) of a closure.
closureExclusiveSize :: DebugClosure p cd s c -> Size
closureExclusiveSize (Stack{}) = Size (-1)
closureExclusiveSize (Closure _ c) = (GD.dcSize c)

-- | Get the retained size (including all dominated closures) of a closure.
closureRetainerSize :: Analysis -> DebugClosure p cd s c -> RetainerSize
closureRetainerSize analysis c = snd (closureExcAndRetainerSizes analysis c)

closureExcAndRetainerSizes :: Analysis -> DebugClosure p cd s c -> (Size, RetainerSize)
closureExcAndRetainerSizes _ Stack{} = (Size (-1), RetainerSize (-1))
  -- ^ TODO How should we handle stack size? only used space on the stack?
  -- Include underflow frames? Return Maybe?
closureExcAndRetainerSizes analysis (Closure cPtr _) =
  let getSizes = analysisSizes analysis
  in getSizes cPtr

closureSourceLocation :: Debuggee -> DebugClosure p cd s c -> IO (Maybe SourceInformation)
closureSourceLocation _ (Stack _ _) = return Nothing
closureSourceLocation (Debuggee e) (Closure _ c) = run e $ do
  request (RequestSourceInfo (tableId (info (noSize c))))

-- | Get the directly referenced closures (with a label) of a closure.
closureReferences :: Debuggee -> DebugClosure PayloadCont ConstrDesc StackCont ClosurePtr -> IO [(String, Closure)]
closureReferences (Debuggee e) (Stack _ stack) = run e $ do
  let lblAndPtrs = [ ( "Frame " ++ show frameIx ++ " Pointer " ++ show ptrIx
                     , ptr
                     )
                      | (frameIx, frame) <- zip [(0::Int)..] (GD.getFrames stack)
                      , (ptrIx  , ptr  ) <- zip [(0::Int)..] [ptr | GD.SPtr ptr <- GD.values frame]
                   ]
  closures <- dereferenceClosures (snd <$> lblAndPtrs)
  return $ zipWith (\(lbl,ptr) c -> (lbl, Closure ptr c))
            lblAndPtrs
            closures
closureReferences (Debuggee e) (Closure _ closure) = run e $ do
  let refPtrs = closureReferencesAndLabels (unDCS closure)
  forM refPtrs $ \(label, ptr) -> case ptr of
    Left cPtr -> do
      refClosure' <- dereferenceSizedClosure cPtr
      return (label, Closure cPtr refClosure')
    Right sPtr -> do
      refStack' <- dereferenceStack sPtr
      return (label, Stack sPtr refStack')

reverseClosureReferences :: HG.HeapGraph Size
                         -> HG.ReverseGraph
                         -> Debuggee
                         -> DebugClosure HG.PapHI ConstrDesc HG.StackHI (Maybe HG.HeapGraphIndex)
                         -> IO [(String, DebugClosure
                                            HG.PapHI
                                            ConstrDesc HG.StackHI
                                            (Maybe HG.HeapGraphIndex))]
reverseClosureReferences hg rm _ c =
  case c of
    Stack {} -> error "Nope - Stack"
    Closure cp _ -> case (HG.reverseEdges cp rm) of
                      Nothing -> return []
                      Just es ->
                        let revs = mapMaybe (flip HG.lookupHeapGraph hg) es
                        in return [(show n, Closure (HG.hgeClosurePtr hge)
                                               (DCS (HG.hgeData hge) (HG.hgeClosure hge) ))
                                    | (n, hge) <- zip [0 :: Int ..] revs]

lookupHeapGraph :: HG.HeapGraph Size -> ClosurePtr -> Maybe (DebugClosure HG.PapHI ConstrDesc HG.StackHI (Maybe HG.HeapGraphIndex))
lookupHeapGraph hg cp =
  case HG.lookupHeapGraph cp hg of
    Just (HG.HeapGraphEntry ptr d s) -> Just (Closure ptr (DCS s d))
    Nothing -> Nothing

fillConstrDesc :: Debuggee
               -> DebugClosure pap ConstrDescCont s c
               -> IO (DebugClosure pap ConstrDesc s c)
fillConstrDesc (Debuggee e) closure = do
  run e $ GD.quadtraverse pure dereferenceConDesc pure pure closure

-- | Pretty print a closure
closurePretty :: Show c => DebugClosure p ConstrDesc s c ->  String
closurePretty (Stack _ _) = "STACK"
closurePretty (Closure _ closure) =
  HG.ppClosure
    "??"
    (\_ refPtr -> show refPtr)
    0
    (unDCS closure)

-- $dominatorTree
--
-- Closure `a` dominates closure `b` if all paths from GC roots to `b` pass
-- through `a`. This means that if `a` is GCed then all dominated closures can
-- be GCed. The relationship is transitive. Transitive edges are omitted in the
-- "dominator tree".
--
-- see http://kohlerm.blogspot.com/2009/02/memory-leaks-are-easy-to-find.html

-- | The roots of the dominator tree.
dominatorRootClosures :: Debuggee -> Analysis -> IO [Closure]
dominatorRootClosures (Debuggee e) analysis = run e $ do
  let domRoots = analysisDominatorRoots analysis
  closures <- dereferenceClosures domRoots
  return [ Closure closurePtr' closure
            | closurePtr' <- domRoots
            | closure <- closures
            ]

-- | Get the dominatess of a closure i.e. the children in the dominator tree.
closureDominatees :: Debuggee -> Analysis -> DebugClosure p cd s ClosurePtr -> IO [Closure]
closureDominatees _ _ (Stack{}) = error "TODO dominator tree does not yet support STACKs"
closureDominatees (Debuggee e) analysis (Closure cPtr _) = run e $ do
  let cPtrToDominatees = analysisDominatees analysis
      cPtrs = cPtrToDominatees cPtr
  closures <- dereferenceClosures cPtrs
  return [ Closure closurePtr' closure
            | closurePtr' <- cPtrs
            | closure <- closures
            ]

--
-- Internal Stuff
--

closureReferencesAndLabels :: GD.DebugClosure pap string stack pointer -> [(String, Either pointer stack)]
closureReferencesAndLabels closure = case closure of
  TSOClosure {..} ->
    [ ("Stack", Left tsoStack)
    , ("Link", Left _link)
    , ("Global Link", Left global_link)
    , ("TRec", Left trec)
    , ("Blocked Exceptions", Left blocked_exceptions)
    , ("Blocking Queue", Left bq)
    ]
  StackClosure{..} -> [("Frames", Right frames )]
  WeakClosure {..} -> [ ("Key", Left key)
                      , ("Value", Left value)
                      , ("C Finalizers", Left cfinalizers)
                      , ("Finalizer", Left finalizer)
                      ] ++
                      [ ("Link", Left link)
                      | Just link <- [mlink] -- TODO do we want to show NULL pointers some how?
                      ]
  TVarClosure {..} -> [("val", Left current_value)]
  MutPrimClosure {..} -> withArgLables ptrArgs
  ConstrClosure {..} -> withFieldLables ptrArgs
  ThunkClosure {..} -> withArgLables ptrArgs
  SelectorClosure {..} -> [("Selectee", Left selectee)]
  IndClosure {..} -> [("Indirectee", Left indirectee)]
  BlackholeClosure {..} -> [("Indirectee", Left indirectee)]
  APClosure {..} -> ("Function", Left fun) : [] -- TODO withBitmapLables ap_payload
  PAPClosure {..} -> ("Function", Left fun) : [] -- TODO: withBitmapLables pap_payload
  APStackClosure {..} -> ("Function", Left fun) : ("Frames", Right payload) : []
  BCOClosure {..} -> [ ("Instructions", Left instrs)
                      , ("Literals", Left literals)
                      , ("Byte Code Objects", Left bcoptrs)
                      ]
  ArrWordsClosure {} -> []
  MutArrClosure {..} -> withIxLables mccPayload
  SmallMutArrClosure {..} -> withIxLables mccPayload
  MutVarClosure {..} -> [("Value", Left var)]
  MVarClosure {..} -> [ ("Queue Head", Left queueHead)
                      , ("Queue Tail", Left queueTail)
                      , ("Value", Left value)
                      ]
  FunClosure {..} -> withArgLables ptrArgs
  BlockingQueueClosure {..} -> [ ("Link", Left link)
                                , ("Black Hole", Left blackHole)
                                , ("Owner", Left owner)
                                , ("Queue", Left queue)
                                ]
  OtherClosure {..} -> ("",) . Left <$> hvalues
  TRecChunkClosure{}  -> [] --TODO
  UnsupportedClosure {} -> []
  where
  withIxLables elements   = [("[" <> show i <> "]" , Left x) | (i, x) <- zip [(0::Int)..] elements]
  withArgLables ptrArgs   = [("Argument " <> show i, Left x) | (i, x) <- zip [(0::Int)..] ptrArgs]
  withFieldLables ptrArgs = [("Field " <> show i   , Left x) | (i, x) <- zip [(0::Int)..] ptrArgs]
--  withBitmapLables pap = [("Argument " <> show i   , Left x) | (i, SPtr x) <- zip [(0::Int)..] (getValues pap)]

--
-- TODO move stuff below here to a lower level module.
--


lookupInfoTable :: RawClosure -> DebugM (StgInfoTableWithPtr, RawInfoTable, RawClosure)
lookupInfoTable rc = do
    let ptr = getInfoTblPtr rc
    [(itbl, rit)] <- request (RequestInfoTables [ptr])
    return (itbl,rit, rc)

pauseThen :: Debuggee -> DebugM b -> IO b
pauseThen dbg@(Debuggee e) d =
  pause dbg >> run e d

dereferenceClosure :: ClosurePtr -> DebugM GD.Closure
dereferenceClosure c = noSize . head <$> dereferenceClosures [c]

dereferenceSizedClosure :: ClosurePtr -> DebugM SizedClosure
dereferenceSizedClosure c = head <$> dereferenceClosures [c]

dereferenceClosures  :: [ClosurePtr] -> DebugM [SizedClosure]
dereferenceClosures cs = do
    raw_cs <- request (RequestClosures cs)
    let its = map getInfoTblPtr raw_cs
    --print $ map (lookupDwarf d) its
    raw_its <- request (RequestInfoTables its)
    return $ zipWith decodeClosureWithSize raw_its (zip cs raw_cs)

dereferenceStack :: StackCont -> DebugM GD.StackFrames
dereferenceStack (StackCont sp stack) = do
--  req_stack <- request (RequestStack (coerce cp))
  let get_bitmap o = request (RequestStackBitmap sp o)
      get_info_table rc = (\(a, _, _) -> a) <$> lookupInfoTable rc
--  traceShowM ("BAD", printStack stack, rawStackSize stack)
--  traceShowM ("GOOD", printStack req_stack, rawStackSize req_stack)
  decoded_stack <- decodeStack get_info_table get_bitmap stack
  return decoded_stack

dereferencePapPayload :: PayloadCont -> DebugM GD.PapPayload
dereferencePapPayload (PayloadCont fp raw) = do
  bm <- request (RequestFunBitmap (fromIntegral $ length raw) fp)
  return $ GenPapPayload (evalState (traversePtrBitmap decodeField bm) raw)
  where
    getWord = do
      v <- gets head
      modify tail
      return v

    decodeField True  = SPtr . ClosurePtr <$> getWord
    decodeField False = SNonPtr <$> getWord



dereferenceConDesc :: ConstrDescCont -> DebugM ConstrDesc
dereferenceConDesc i = request (RequestConstrDesc i)

_noConDesc :: ConstrDescCont -> DebugM ConstrDesc
_noConDesc c = traceShow c (return emptyConDesc)

emptyConDesc :: ConstrDesc
emptyConDesc = ConstrDesc "" "" ""

-- | Do a traversal requesting closures one by one using RequestClosure
fullTraversal :: ClosurePtr -> DebugM UClosure
fullTraversal = fullTraversalX dereferenceSizedClosure

-- | Do a traversal using the block cache
fullTraversalViaBlocks :: ClosurePtr -> DebugM UClosure
fullTraversalViaBlocks = fullTraversalX dereferenceClosureFromBlock

fullTraversalX :: (ClosurePtr -> DebugM SizedClosure) -> ClosurePtr -> DebugM UClosure
fullTraversalX derefClosure c = do
--  putStrLn ("TIME TO DEREFERENCE: " ++ show c)
  dc <- derefClosure c
--  putStrLn ("FULL TRAVERSE(" ++ show c ++ ") = " ++ show dc)
  MkFix1 <$> quadtraverse (fullPAPTraversal derefClosure) dereferenceConDesc (fullStackTraversal derefClosure) (fullTraversalX derefClosure) dc

fullStackTraversal :: (ClosurePtr -> DebugM SizedClosure) -> StackCont -> DebugM UStack
fullStackTraversal k sc = do
  ds <- dereferenceStack sc
--  print ("FULL STACK", ds)
  MkFix2 <$> traverse (fullTraversalX k) ds

fullPAPTraversal :: (ClosurePtr -> DebugM SizedClosure) -> PayloadCont -> DebugM UPapPayload
fullPAPTraversal k pa = do
  p <- dereferencePapPayload pa
  MkFix3 <$> traverse (fullTraversalX k) p

{-
-- | Print out the number of request made for each request type
traceRequestLog :: Env u w -> IO ()
traceRequestLog d = do
  s <- readIORef (statsRef d)
  putStrLn (ppStats s)

traceProfile :: Env u w -> IO ()
traceProfile e = do
  p <- readIORef (profRef e)
  print (profile p)
  -}

-- | Consult the BlockCache for the block which contains a specific
-- closure, if it's not there then try to fetch the right block, if that
-- fails, call 'dereferenceClosure'
dereferenceClosureFromBlock :: ClosurePtr -> DebugM SizedClosure
dereferenceClosureFromBlock cp
  | not (heapAlloced cp) = dereferenceSizedClosure cp
  | otherwise = do
      rc <-  requestBlock (LookupClosure cp)
      let it = getInfoTblPtr rc
      [st_it] <- request (RequestInfoTables [it])
      return $ decodeClosureWithSize st_it (cp, rc)

precacheBlocks :: DebugM [RawBlock]
precacheBlocks = requestBlock PopulateBlockCache

-- Traverse the tree from GC roots, to populate the caches
-- with everything necessary.
traceFrom :: [ClosurePtr] -> DebugM ()
traceFrom cps = evalStateT (mapM_ traceClosureFrom cps) IS.empty

traceClosureFrom :: ClosurePtr -> StateT IS.IntSet DebugM ()
traceClosureFrom cp@(ClosurePtr c) = do
    m <- get
    unless (IS.member (fromIntegral c) m) $ do
      modify (IS.insert (fromIntegral c))
      sc <- lift $ dereferenceClosureFromBlock cp
      quadtraverse tracePapFrom traceConstrDesc traceStackFrom traceClosureFrom sc
      case lookupStgInfoTableWithPtr (noSize sc) of
        infoTableWithptr -> lift $ request (RequestSourceInfo (tableId infoTableWithptr))
      return ()

traceStackFrom :: StackCont -> StateT IS.IntSet DebugM ()
traceStackFrom st = do
  traverse traceClosureFrom  =<< lift (dereferenceStack st)
  return ()

tracePapFrom :: PayloadCont -> StateT IS.IntSet DebugM ()
tracePapFrom st = do
  traverse traceClosureFrom  =<< lift (dereferencePapPayload st)
  return ()

traceConstrDesc :: ConstrDescCont -> StateT s DebugM ()
traceConstrDesc d = lift $ () <$ dereferenceConDesc d

