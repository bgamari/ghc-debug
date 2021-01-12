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
  , pauseThen
  , resume
  , pausePoll
  , withPause

  -- * General Requests
  , precacheBlocks
  , gcRoots
  , allBlocks
  , getSourceInfo
  , savedObjects

  -- * Dereferencing functions
  , dereferenceClosures
  , dereferenceClosure
  , dereferenceClosureDirect
  , dereferenceClosureC
  , dereferenceStack
  , dereferencePapPayload
  , dereferenceConDesc

  ) where

import           Control.Exception
import           GHC.Debug.Types
import           GHC.Debug.Decode
import           GHC.Debug.Decode.Stack
import GHC.Debug.Client.Monad
import           GHC.Debug.Client.BlockCache
import Control.Monad.State

import Debug.Trace

-- | Pause the debuggee
pause :: Debuggee -> IO ()
pause e = do
  run e $ request RequestPause

-- | Resume the debuggee
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


dereferenceClosureC :: ClosurePtr -> DebugM SizedClosureC
dereferenceClosureC cp = do
  c <- dereferenceClosure cp
  quadtraverse pure dereferenceConDesc pure pure c

-- | Decode a closure corresponding to the given 'ClosurePtr'
-- You should not use this function directly unless you know what you are
-- doing. 'dereferenceClosure' will be much faster in general.
dereferenceClosureDirect :: ClosurePtr -> DebugM SizedClosure
dereferenceClosureDirect c = do
    raw_c <- request (RequestClosure c)
    let it = getInfoTblPtr raw_c
    raw_it <- request (RequestInfoTable it)
    return $ decodeClosure raw_it (c, raw_c)

dereferenceClosures  :: [ClosurePtr] -> DebugM [SizedClosure]
dereferenceClosures cs = mapM dereferenceClosure cs

-- | Deference some StackFrames from a given 'StackCont'
dereferenceStack :: StackCont -> DebugM StackFrames
dereferenceStack (StackCont sp stack) = do
--  req_stack <- request (RequestStack (coerce cp))
  let get_bitmap o = request (RequestStackBitmap sp o)
      get_info_table rc = (\(a, _, _) -> a) <$> lookupInfoTable rc
--  traceShowM ("BAD", printStack stack, rawStackSize stack)
--  traceShowM ("GOOD", printStack req_stack, rawStackSize req_stack)
  decoded_stack <- decodeStack get_info_table get_bitmap stack
  return decoded_stack

-- | Derference the PapPayload from the 'PayloadCont'
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
dereferenceClosure :: ClosurePtr -> DebugM SizedClosure
dereferenceClosure cp
  | not (heapAlloced cp) = dereferenceClosureDirect cp
  | otherwise = do
      rc <-  requestBlock (LookupClosure cp)
      let it = getInfoTblPtr rc
      st_it <- request (RequestInfoTable it)
      return $ decodeClosure st_it (cp, rc)

-- | Fetch all the blocks from the debuggee and add them to the block cache
precacheBlocks :: DebugM [RawBlock]
precacheBlocks = requestBlock PopulateBlockCache

-- | Query the debuggee for the list of GC Roots
gcRoots :: DebugM [ClosurePtr]
gcRoots = request RequestRoots

-- | Query the debuggee for all the blocks it knows about
allBlocks :: DebugM [RawBlock]
allBlocks = request RequestAllBlocks

-- | Query the debuggee for source information about a specific info table.
-- This requires your executable to be built with @-finfo-table-map@.
getSourceInfo :: InfoTablePtr -> DebugM (Maybe SourceInformation)
getSourceInfo = request . RequestSourceInfo

-- | Query the debuggee for the list of saved objects.
savedObjects :: DebugM [ClosurePtr]
savedObjects = request RequestSavedObjects

