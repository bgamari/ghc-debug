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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib
  ( -- * Running/Connecting to a debuggee
    Debuggee
  , debuggeeRun
  , debuggeeConnect
  , snapshotConnect
  , debuggeeClose
  , withDebuggeeRun
  , withDebuggeeConnect
  , socketDirectory
  , snapshotDirectory

    -- * Pause/Resume
  , GD.pause
  , GD.resume
  , GD.pausePoll
  , GD.withPause

    -- * Querying the paused debuggee
  , rootClosures
  , savedClosures

    -- * Closures
  , Closure
  , DebugClosure(..)
  , closureShowAddress
  , closureExclusiveSize
  , closureRetainerSize
  , closureSourceLocation
  , SourceInformation(..)
  , closureReferences
  , closurePretty
  , fillConstrDesc
  , InfoTablePtr
  , ListItem(..)
  , closureInfoPtr
  , infoSourceLocation

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

    -- * Profiling
  , profile

    -- * Retainers
  , retainersOfConstructor
  , retainersOfAddress
  , retainersOfConstructorExact
  , retainersOfArrWords
  , retainersOfInfoTable

    -- * Snapshot
  , snapshot

  -- * Types
  , Ptr(..)
  , CCSPtr
  , toPtr
  , dereferencePtr
  , ConstrDesc(..)
  , ConstrDescCont
  , GenPapPayload(..)
  , StackCont
  , PayloadCont
  , SrtCont
  , ClosurePtr
  , readClosurePtr
  , HG.StackHI
  , HG.PapHI
  , HG.SrtHI
  , HG.HeapGraphIndex
  , ProfHeaderWord
    --
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Graph as G
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified GHC.Debug.Types as GD
import           GHC.Debug.Types hiding (Closure, DebugClosure)
import           GHC.Debug.Convention (socketDirectory, snapshotDirectory)
import           GHC.Debug.Client.Monad (request, run, Debuggee)
import qualified GHC.Debug.Client.Monad as GD
import qualified GHC.Debug.Client.Query as GD
import qualified GHC.Debug.Profile as GD
import qualified GHC.Debug.Retainers as GD
import qualified GHC.Debug.Snapshot as GD
import qualified GHC.Debug.Types.Graph as HG
import qualified GHC.Debug.Dominators as HG
import qualified Data.HashMap.Strict as HM
import Data.Tree
import Control.Monad
import System.FilePath
import System.Directory
import Control.Tracer
import Data.Bitraversable
import Data.Text (Text, pack)

data Analysis = Analysis
  { analysisDominatorRoots :: ![ClosurePtr]
  , analysisDominatees :: !(ClosurePtr -> [ClosurePtr])
  -- ^ Unsorted dominatees of a closure
  , analysisSizes :: !(ClosurePtr -> (Size, RetainerSize))
  -- ^ Size and retainer size (via dominator tree) of closures
  }

initialTraversal :: Debuggee -> IO (HG.HeapGraph Size)
initialTraversal e = run e $ do
    -- Calculate the dominator tree with retainer sizes
    -- TODO perhaps this conversion to a graph can be done in GHC.Debug.Types.Graph
    _ <- GD.precacheBlocks
    rs <- request RequestRoots
    let derefFuncM cPtr = do
          c <- GD.dereferenceClosure cPtr
          hextraverse pure GD.dereferenceSRT GD.dereferencePapPayload GD.dereferenceConDesc (bitraverse GD.dereferenceSRT pure <=< GD.dereferenceStack) pure c
    hg <- case rs of
      [] -> error "Empty roots"
      (x:xs) -> HG.multiBuildHeapGraph derefFuncM Nothing (x :| xs)
    return hg

-- This function is very very very slow, it needs to be optimised.
runAnalysis :: Debuggee -> HG.HeapGraph Size -> IO Analysis
runAnalysis e hg = run e $ do
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
withDebuggeeRun exeName socketName action = GD.withDebuggeeRun exeName socketName action

-- | Bracketed version of @debuggeeConnect@. Connects to a debuggee, runs the
-- action, then closes the debuggee.
withDebuggeeConnect :: FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                   -> (Debuggee -> IO a)
                   -> IO a
withDebuggeeConnect socketName action = GD.withDebuggeeConnect socketName action

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
            -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
            -> IO Debuggee
debuggeeRun exeName socketName = GD.debuggeeRun exeName socketName

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeConnect :: (Text -> IO ())
                -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                -> IO Debuggee
debuggeeConnect toChan socketName = GD.debuggeeConnectWithTracer (contramap pack $ Tracer (emit toChan)) socketName


snapshotConnect :: (Text -> IO ()) -> FilePath -> IO Debuggee
snapshotConnect toChan snapshotName = GD.snapshotInitWithTracer (contramap pack $ Tracer (emit toChan)) snapshotName

-- | Close the connection to the debuggee.
debuggeeClose :: Debuggee -> IO ()
debuggeeClose = GD.debuggeeClose

-- | Request the debuggee's root pointers.
rootClosures :: Debuggee -> IO [Closure]
rootClosures e = run e $ do
  closurePtrs <- request RequestRoots
  closures <- GD.dereferenceClosures closurePtrs
  return [ Closure closurePtr' closure
            | closurePtr' <- closurePtrs
            | closure <- closures
            ]

-- | A client can save objects by calling a special RTS method
-- This function returns the closures it saved.
savedClosures :: Debuggee -> IO [Closure]
savedClosures e = run e $ do
  closurePtrs <- request RequestSavedObjects
  closures <- GD.dereferenceClosures closurePtrs
  return $ zipWith Closure
            closurePtrs
            closures

profile :: Debuggee -> FilePath -> IO ()
profile dbg fp = do
  c <- run dbg $ do
    roots <- GD.gcRoots
    GD.censusClosureType roots
  GD.writeCensusByClosureType fp c

snapshot :: Debuggee -> FilePath -> IO ()
snapshot dbg fp = do
  dir <- snapshotDirectory
  createDirectoryIfMissing True dir
  GD.run dbg $ GD.snapshot (dir </> fp)

retainersOfAddress :: Maybe Int -> Maybe [ClosurePtr] -> Debuggee -> [ClosurePtr] -> IO [[Closure]]
retainersOfAddress n mroots dbg address = do
  run dbg $ do
    roots <- maybe GD.gcRoots return mroots
    stack <- GD.findRetainersOf n roots address
    traverse (\cs -> zipWith Closure cs <$> (GD.dereferenceClosures cs)) stack

retainersOfConstructor :: Maybe Int -> Maybe [ClosurePtr] -> Debuggee -> String -> IO [[Closure]]
retainersOfConstructor n mroots dbg con_name = do
  run dbg $ do
    roots <- maybe GD.gcRoots return mroots
    stack <- GD.findRetainersOfConstructor n roots con_name
    traverse (\cs -> zipWith Closure cs <$> (GD.dereferenceClosures cs)) stack

retainersOfConstructorExact :: Maybe Int -> Debuggee -> String -> IO [[Closure]]
retainersOfConstructorExact n dbg con_name = do
  run dbg $ do
    roots <- GD.gcRoots
    stack <- GD.findRetainersOfConstructorExact n roots con_name
    traverse (\cs -> zipWith Closure cs <$> (GD.dereferenceClosures cs)) stack

retainersOfArrWords :: Maybe Int -> Debuggee -> Word -> IO [[Closure]]
retainersOfArrWords n dbg lim = do
  run dbg $ do
    roots <- GD.gcRoots
    stack <- GD.findRetainersOfArrWords n roots lim
    traverse (\cs -> zipWith Closure cs <$> (GD.dereferenceClosures cs)) stack

retainersOfInfoTable :: Maybe Int -> Maybe [ClosurePtr] -> Debuggee -> InfoTablePtr -> IO [[Closure]]
retainersOfInfoTable n mroots dbg info_ptr = do
  run dbg $ do
    roots <- maybe GD.gcRoots return mroots
    stack <- GD.findRetainersOfInfoTable n roots info_ptr
    traverse (\cs -> zipWith Closure cs <$> (GD.dereferenceClosures cs)) stack

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

type Closure = DebugClosure CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr

data ListItem ccs srt a b c d = ListData | ListOnlyInfo InfoTablePtr | ListFullClosure (DebugClosure ccs srt a b c d)

data DebugClosure ccs srt p cd s c
  = Closure
    { _closurePtr :: ClosurePtr
    , _closureSized :: DebugClosureWithSize ccs srt p cd s c
    }
  | Stack
    { _stackPtr :: StackCont
    , _stackStack :: GD.GenStackFrames srt c
    }

toPtr :: DebugClosure ccs srt p cd s c -> Ptr
toPtr (Closure cp _) = CP cp
toPtr (Stack sc _)   = SP sc

data Ptr = CP ClosurePtr | SP StackCont deriving (Eq, Ord)


dereferencePtr :: Debuggee -> Ptr -> IO (DebugClosure CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr)
dereferencePtr dbg (CP cp) = run dbg (Closure <$> pure cp <*> GD.dereferenceClosure cp)
dereferencePtr dbg (SP sc) = run dbg (Stack <$> pure sc <*> GD.dereferenceStack sc)

instance Hextraversable DebugClosure where
  hextraverse p f g h i j (Closure cp c) = Closure cp <$> hextraverse p f g h i j c
  hextraverse _ p _ _ _ h (Stack sp s) = Stack sp <$> bitraverse p h s

closureShowAddress :: DebugClosure ccs srt p cd s c -> String
closureShowAddress (Closure c _) = show c
closureShowAddress (Stack  (StackCont s _) _) = show s

-- | Get the exclusive size (not including any referenced closures) of a closure.
closureExclusiveSize :: DebugClosure ccs srt p cd s c -> Size
closureExclusiveSize (Stack{}) = Size (-1)
closureExclusiveSize (Closure _ c) = (GD.dcSize c)

-- | Get the retained size (including all dominated closures) of a closure.
closureRetainerSize :: Analysis -> DebugClosure ccs srt p cd s c -> RetainerSize
closureRetainerSize analysis c = snd (closureExcAndRetainerSizes analysis c)

closureExcAndRetainerSizes :: Analysis -> DebugClosure ccs srt p cd s c -> (Size, RetainerSize)
closureExcAndRetainerSizes _ Stack{} = (Size (-1), RetainerSize (-1))
  -- ^ TODO How should we handle stack size? only used space on the stack?
  -- Include underflow frames? Return Maybe?
closureExcAndRetainerSizes analysis (Closure cPtr _) =
  let getSizes = analysisSizes analysis
  in getSizes cPtr

closureSourceLocation :: Debuggee -> DebugClosure ccs srt p cd s c -> IO (Maybe SourceInformation)
closureSourceLocation _ (Stack _ _) = return Nothing
closureSourceLocation e (Closure _ c) = run e $ do
  request (RequestSourceInfo (tableId (info (noSize c))))

closureInfoPtr :: DebugClosure ccs srt p cd s c -> Maybe InfoTablePtr
closureInfoPtr (Stack {}) = Nothing
closureInfoPtr (Closure _ c) = Just (tableId (info (noSize c)))

infoSourceLocation :: Debuggee -> InfoTablePtr -> IO (Maybe SourceInformation)
infoSourceLocation e ip = run e $ request (RequestSourceInfo ip)

-- | Get the directly referenced closures (with a label) of a closure.
closureReferences :: Debuggee -> DebugClosure CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr -> IO [(String, ListItem CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr)]
closureReferences e (Stack _ stack) = run e $ do
  stack' <- bitraverse GD.dereferenceSRT pure stack
  let action (GD.SPtr ptr) = ("Pointer", ListFullClosure . Closure ptr <$> GD.dereferenceClosure ptr)
      action (GD.SNonPtr dat) = ("Data:" ++ show dat, return ListData)

      frame_items frame = ("Info: " ++ show (tableId (frame_info frame)), return (ListOnlyInfo (tableId (frame_info frame)))) :
                          [ ("SRT: ", ListFullClosure . Closure srt <$> GD.dereferenceClosure srt)  | Just srt <- [getSrt (frame_srt frame)]]
                          ++ map action (GD.values frame)

      add_frame_ix ix (lbl, x) = ("Frame " ++ show ix ++ " " ++ lbl, x)
  let lblAndPtrs = [ map (add_frame_ix frameIx) (frame_items frame)
                      | (frameIx, frame) <- zip [(0::Int)..] (GD.getFrames stack')
                   ]
--  traverse GD.dereferenceClosures (snd <$> lblAndPtrs)
  traverse (traverse id) (concat lblAndPtrs)
  {-
  return $ zipWith (\(lbl,ptr) c -> (lbl, Closure ptr c))
            lblAndPtrs
            closures
            -}
closureReferences e (Closure _ closure) = run e $ do
  closure' <- hextraverse pure GD.dereferenceSRT GD.dereferencePapPayload pure pure pure closure
  let refPtrs = closureReferencesAndLabels (unDCS closure')
  forM refPtrs $ \(label, ptr) -> case ptr of
    Left cPtr -> do
      refClosure' <- GD.dereferenceClosure cPtr
      return (label, ListFullClosure $ Closure cPtr refClosure')
    Right sPtr -> do
      refStack' <- GD.dereferenceStack sPtr
      return (label, ListFullClosure $ Stack sPtr refStack')

reverseClosureReferences :: HG.HeapGraph Size
                         -> HG.ReverseGraph
                         -> Debuggee
                         -> DebugClosure CCSPtr HG.SrtHI HG.PapHI ConstrDesc HG.StackHI (Maybe HG.HeapGraphIndex)
                         -> IO [(String, DebugClosure
                                            CCSPtr
                                            HG.SrtHI
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

lookupHeapGraph :: HG.HeapGraph Size -> ClosurePtr -> Maybe (DebugClosure CCSPtr HG.SrtHI HG.PapHI ConstrDesc HG.StackHI (Maybe HG.HeapGraphIndex))
lookupHeapGraph hg cp =
  case HG.lookupHeapGraph cp hg of
    Just (HG.HeapGraphEntry ptr d s) -> Just (Closure ptr (DCS s d))
    Nothing -> Nothing

fillConstrDesc :: Debuggee
               -> DebugClosure ccs srt pap ConstrDescCont s c
               -> IO (DebugClosure ccs srt pap ConstrDesc s c)
fillConstrDesc e closure = do
  run e $ GD.hextraverse pure pure pure GD.dereferenceConDesc pure pure closure

-- | Pretty print a closure
closurePretty :: Debuggee -> DebugClosure CCSPtr InfoTablePtr PayloadCont ConstrDesc s ClosurePtr ->  IO String
closurePretty _ (Stack _ frames) = return $ (show (length frames) ++ " frames")
closurePretty dbg (Closure _ closure) = run dbg $  do
  closure' <- hextraverse pure GD.dereferenceSRT GD.dereferencePapPayload pure pure pure closure
  return $ HG.ppClosure
    (\_ refPtr -> show refPtr)
    0
    (unDCS closure')

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
dominatorRootClosures e analysis = run e $ do
  let domRoots = analysisDominatorRoots analysis
  closures <- GD.dereferenceClosures domRoots
  return [ Closure closurePtr' closure
            | closurePtr' <- domRoots
            | closure <- closures
            ]

-- | Get the dominatess of a closure i.e. the children in the dominator tree.
closureDominatees :: Debuggee -> Analysis -> DebugClosure ccs srt p cd s ClosurePtr -> IO [Closure]
closureDominatees _ _ (Stack{}) = error "TODO dominator tree does not yet support STACKs"
closureDominatees e analysis (Closure cPtr _) = run e $ do
  let cPtrToDominatees = analysisDominatees analysis
      cPtrs = cPtrToDominatees cPtr
  closures <- GD.dereferenceClosures cPtrs
  return [ Closure closurePtr' closure
            | closurePtr' <- cPtrs
            | closure <- closures
            ]

--
-- Internal Stuff
--

closureReferencesAndLabels :: GD.DebugClosure ccs (GenSrtPayload pointer) PapPayload string stack pointer -> [(String, Either pointer stack)]
closureReferencesAndLabels closure = case closure of
  TSOClosure {..} ->
    [ ("Thread label", Left lbl) | Just lbl <- pure threadLabel ] ++
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
  ThunkClosure {..} -> [ ("SRT", Left cp) | Just cp <- [getSrt srt]]
                        ++ withArgLables ptrArgs
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
  FunClosure {..} ->
    [ ("SRT", Left cp) | Just cp <- [getSrt srt]]
    ++ withArgLables ptrArgs
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
