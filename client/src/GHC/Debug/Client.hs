{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeApplications #-}
module GHC.Debug.Client
  ( -- * Running/Connecting to a debuggee
    Debuggee
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
  , closureReferences
  , closurePretty

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
  , dereferenceClosures
  , dereferenceClosure
  , dereferenceClosureFromBlock
  , dereferenceStack
  , dereferenceConDesc
  , fullTraversal
  , fullTraversalViaBlocks
  , Tritraversable(..)
  , precacheBlocks
  , DebugEnv
  , DebugM
  ) where

import Control.Exception
import Control.Monad (forM)
import qualified GHC.Debug.Types as GD
import GHC.Debug.Types hiding (Closure, Stack)
import GHC.Debug.Decode
import GHC.Debug.Decode.Stack

import GHC.Debug.Convention (socketDirectory)
import GHC.Debug.Client.Monad (DebugEnv, DebugM, request, requestBlock, run)
import qualified GHC.Debug.Client.Monad as GD
import GHC.Debug.Client.BlockCache
import qualified GHC.Debug.Types.Graph as GD

import Debug.Trace


newtype Debuggee = Debuggee { unDebuggee :: DebugEnv DebugM }

-- | Bracketed version of @debuggeeRun@. Runs a debuggee, connects to it, runs
-- the action, kills the process, then closes the debuggee.
withDebuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
                -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                -> (Debuggee -> IO a)
                -> IO a
withDebuggeeRun exeName socketName action = GD.withDebuggeeRun exeName socketName (action . Debuggee)

-- | Bracketed version of @debuggeeConnect@. Connects to a debuggee, runs the
-- action, then closes the debuggee.
withDebuggeeConnect :: FilePath  -- ^ executable name of the debuggee
                   -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                   -> (Debuggee -> IO a)
                   -> IO a
withDebuggeeConnect exeName socketName action = GD.withDebuggeeConnect exeName socketName (action . Debuggee)

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
            -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
            -> IO Debuggee
debuggeeRun exeName socketName = Debuggee <$> GD.debuggeeRun exeName socketName

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeConnect :: FilePath  -- ^ path to executable to run as the debuggee
                -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                -> IO Debuggee
debuggeeConnect exeName socketName = Debuggee <$> GD.debuggeeConnect exeName socketName

-- | Close the connection to the debuggee.
debuggeeClose :: Debuggee -> IO ()
debuggeeClose = GD.debuggeeClose . unDebuggee

-- | Pause the debuggee
pause :: Debuggee -> IO ()
pause (Debuggee e) = run e $ request RequestPause

resume :: Debuggee -> IO ()
resume (Debuggee e) = run e $ request RequestResume

-- | Like pause, but wait for the debuggee to pause itself. It currently
-- impossible to resume after a pause caused by a poll.?????????? Is that true???? can we not just call resume????
pausePoll :: Debuggee -> IO ()
pausePoll (Debuggee e) = run e $ request RequestPoll

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
-- requestConstrDesc (Debuggee e) = run e $ request RequestConstrDesc

-- -- | Lookup source information of an info table
-- requestSourceInfo :: Debuggee -> InfoTablePtr -> IO [String]
-- requestSourceInfo (Debuggee e) = run e $ request RequestSourceInfo

-- -- | Request a set of closures.
-- requestClosures :: Debuggee -> [ClosurePtr] -> IO [RawClosure]
-- requestClosures (Debuggee e) = run e $ request RequestClosures

-- -- | Request a stack
-- requestStack :: Debuggee -> StackPtr -> IO RawStack
-- requestStack (Debuggee e) = run e $ request RequestStack

-- -- | Request a set of info tables.
-- requestInfoTables :: Debuggee -> [InfoTablePtr] -> IO [(StgInfoTableWithPtr, RawInfoTable)]
-- requestInfoTables (Debuggee e) = run e $ request RequestInfoTables

data Closure
  = Closure
    { closurePtr :: ClosurePtr
    , closureSized :: SizedClosure
    }
  | Stack
    { stackPtr :: StackCont
    , stackStack :: GD.Stack
    }

closureShowAddress :: Closure -> String
closureShowAddress (Closure c _) = show c
closureShowAddress (Stack   s _) = show s

-- | Get the exlusive size (not including referenced obejcts) of a closure.
closureExclusiveSize :: Debuggee -> Closure -> IO Int
closureExclusiveSize _dbg (Stack _ _stack) = return (-1)
  -- ^ TODO How should we handle stack size? only used space on the stack?
  -- Include underflow frames? Return Maybe?
closureExclusiveSize _dbg (Closure _ closure) = return $ getSize $ extraDCS closure

-- | Get the directly referenced closures (with a label) of a closure.
closureReferences :: Debuggee -> Closure -> IO [(String, Closure)]
closureReferences (Debuggee e) (Stack _ stack) = run e $ do
  let lblAndPtrs = [ ( "Frame " ++ show frameIx ++ " Pointer " ++ show ptrIx
                     , ptr
                     )
                      | (frameIx, frame) <- zip [(0::Int)..] (GD.frames stack)
                      , (ptrIx  , ptr  ) <- zip [(0::Int)..] [ptr | GD.SPtr ptr <- GD.values frame]
                   ]
  closures <- dereferenceClosures (snd <$> lblAndPtrs)
  return $ zipWith (\(lbl,ptr) c -> (lbl, Closure ptr c))
            lblAndPtrs
            closures
closureReferences (Debuggee e) (Closure _ closure) = run e $ do
  let
      withIxLables elements   = [("[" <> show i <> "]" , Left x) | (i, x) <- zip [(0::Int)..] elements]
      withArgLables ptrArgs   = [("Argument " <> show i, Left x) | (i, x) <- zip [(0::Int)..] ptrArgs]
      withFieldLables ptrArgs = [("Field " <> show i   , Left x) | (i, x) <- zip [(0::Int)..] ptrArgs]

      refPtrs = case unDCS closure of
          TSOClosure {..} ->
            [ ("Stack", Right tsoStack)
            , ("Link", Left _link)
            , ("Global Link", Left global_link)
            , ("TRec", Left trec)
            , ("Blocked Exceptions", Left blocked_exceptions)
            , ("Blocking Queue", Left bq)
            ]
          WeakClosure {..} -> [ ("Key", Left key)
                              , ("Value", Left value)
                              , ("C Finalizers", Left cfinalizers)
                              , ("Finalizer", Left finalizer)
                              ] ++
                              [ ("Link", Left link)
                              | Just link <- [mlink] -- TODO do we want to show NULL pointers some how?
                              ]
          IntClosure {} -> []
          WordClosure {} -> []
          Int64Closure {} -> []
          Word64Closure {} -> []
          AddrClosure {} -> []
          FloatClosure {} -> []
          DoubleClosure {} -> []
          ConstrClosure {..} -> withFieldLables ptrArgs
          ThunkClosure {..} -> withArgLables ptrArgs
          SelectorClosure {..} -> [("Selectee", Left selectee)]
          IndClosure {..} -> [("Indirectee", Left indirectee)]
          BlackholeClosure {..} -> [("Indirectee", Left indirectee)]
          APClosure {..} -> ("Function", Left fun) : withArgLables payload
          PAPClosure {..} -> ("Function", Left fun) : withArgLables payload
          APStackClosure {..} -> ("Function", Left fun) : withArgLables payload
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
          UnsupportedClosure {} -> []


  forM refPtrs $ \(label, ptr) -> case ptr of
    Left cPtr -> do
      refClosure' <- dereferenceSizedClosure cPtr
      return (label, Closure cPtr refClosure')
    Right sPtr -> do
      refStack' <- dereferenceStack sPtr
      return (label, Stack sPtr refStack')

-- | Pritty print a closure
closurePretty :: Debuggee -> Closure -> IO String
closurePretty _ (Stack _ _) = return "STACK"
closurePretty (Debuggee e) (Closure _ closure) = do
  closure' <- GD.tritraverse toConstrDesc pure pure (unDCS closure)
  return $ GD.ppClosure
    "??"
    (\_ refPtr -> show refPtr)
    0
    closure'
    where
    toConstrDesc = run e . dereferenceConDesc

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

dereferenceStack :: StackCont -> DebugM GD.Stack
dereferenceStack (StackCont sp) = do
  stack <- request (RequestStack sp)
  let get_bitmap p = request (RequestBitmap (getInfoTblPtr p))
      get_info_table rc = (\(a, _, _) -> a) <$> lookupInfoTable rc
  decoded_stack <- decodeStack get_info_table get_bitmap stack
  return decoded_stack


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
  MkFix1 <$> tritraverse dereferenceConDesc (fullStackTraversal derefClosure) (fullTraversalX derefClosure) dc

fullStackTraversal :: (ClosurePtr -> DebugM SizedClosure) -> StackCont -> DebugM UStack
fullStackTraversal k sc = do
  ds <- dereferenceStack sc
--  print ("FULL STACK", ds)
  MkFix2 <$> traverse (fullTraversalX k) ds

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
  | not (ptrInBlock cp) = dereferenceSizedClosure cp
  | otherwise = do
      rc <-  requestBlock (LookupClosure cp)
      let it = getInfoTblPtr rc
      [st_it] <- request (RequestInfoTables [it])
      return $ decodeClosureWithSize st_it (cp, rc)

precacheBlocks :: DebugM Int
precacheBlocks = requestBlock PopulateBlockCache

