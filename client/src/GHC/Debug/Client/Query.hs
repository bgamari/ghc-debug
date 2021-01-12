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
module GHC.Debug.Client.Query
  ( -- * Pause/Resume
    pause
  , resume
  , pausePoll
  , withPause

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
  ) where

import           Control.Exception
import           GHC.Debug.Types
import           GHC.Debug.Decode
import           GHC.Debug.Decode.Stack
import GHC.Debug.Client.Monad
import           GHC.Debug.Client.BlockCache
import qualified GHC.Debug.Types.Graph as HG
import Control.Monad.State

import Debug.Trace



-- | Pause the debuggee
pause :: Debuggee -> IO ()
pause e = do
  run e $ request RequestPause

resume :: Debuggee -> IO ()
resume e = run e $ request RequestResume

-- | Like pause, but wait for the debuggee to pause itself. It currently
-- impossible to resume after a pause caused by a poll.?????????? Is that true???? can we not just call resume????
pausePoll :: Debuggee -> IO ()
pausePoll e = do
  run e $ request RequestPoll

-- | Bracketed version of pause/resume.
withPause :: Debuggee -> IO a -> IO a
withPause dbg act = bracket_ (pause dbg) (resume dbg) act


lookupInfoTable :: RawClosure -> DebugM (StgInfoTableWithPtr, RawInfoTable, RawClosure)
lookupInfoTable rc = do
    let ptr = getInfoTblPtr rc
    (itbl, rit) <- request (RequestInfoTable ptr)
    return (itbl,rit, rc)

pauseThen :: Debuggee -> DebugM b -> IO b
pauseThen e d =
  pause e >> run e d

dereferenceClosure :: ClosurePtr -> DebugM Closure
dereferenceClosure c = noSize . head <$> dereferenceClosures [c]

dereferenceSizedClosure :: ClosurePtr -> DebugM SizedClosure
dereferenceSizedClosure c = do
    raw_c <- request (RequestClosure c)
    let it = getInfoTblPtr raw_c
    --print $ map (lookupDwarf d) its
    raw_it <- request (RequestInfoTable it)
    return $ decodeClosure raw_it (c, raw_c)

dereferenceClosures  :: [ClosurePtr] -> DebugM [SizedClosure]
dereferenceClosures cs = mapM dereferenceClosureFromBlock cs

dereferenceStack :: StackCont -> DebugM StackFrames
dereferenceStack (StackCont sp stack) = do
--  req_stack <- request (RequestStack (coerce cp))
  let get_bitmap o = request (RequestStackBitmap sp o)
      get_info_table rc = (\(a, _, _) -> a) <$> lookupInfoTable rc
--  traceShowM ("BAD", printStack stack, rawStackSize stack)
--  traceShowM ("GOOD", printStack req_stack, rawStackSize req_stack)
  decoded_stack <- decodeStack get_info_table get_bitmap stack
  return decoded_stack

dereferencePapPayload :: PayloadCont -> DebugM PapPayload
dereferencePapPayload (PayloadCont fp raw) = do
  bm <- request (RequestFunBitmap (fromIntegral $ length raw) fp)
  return $ GenPapPayload (evalState (traversePtrBitmap decodeField bm) raw)
  where
    getWord = do
      v <- gets head
      modify tail
      return v

    decodeField True  = SPtr . mkClosurePtr <$> getWord
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
      st_it <- request (RequestInfoTable it)
      return $ decodeClosure st_it (cp, rc)

precacheBlocks :: DebugM [RawBlock]
precacheBlocks = requestBlock PopulateBlockCache


