{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module GHC.Debug.Client
  ( -- * Running/Connecting to a debuggee
    withDebuggeeRun
  , withDebuggeeConnect
  , debuggeeRun
  , debuggeeConnect
  , debuggeeClose
    -- * Pause/Resume
  , pauseDebuggee
  , pause
  , pauseThen
  , resume
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
  , socketDirectory
  , DebugEnv
  , DebugM
  ) where

import Control.Exception
import GHC.Debug.Types
import GHC.Debug.Decode
import GHC.Debug.Decode.Stack

import GHC.Debug.Convention (socketDirectory)
import GHC.Debug.Client.Monad
import GHC.Debug.Client.BlockCache

import Debug.Trace


lookupInfoTable :: RawClosure -> DebugM (StgInfoTableWithPtr, RawInfoTable, RawClosure)
lookupInfoTable rc = do
    let ptr = getInfoTblPtr rc
    [(itbl, rit)] <- request (RequestInfoTables [ptr])
    return (itbl,rit, rc)

pause, resume :: DebugEnv DebugM -> IO ()
pauseThen :: DebugEnv DebugM -> DebugM b -> IO b
pause e = run e $ request RequestPause
pauseThen e d =
  pause e >> run e d
resume e = run e $ request RequestResume

pauseDebuggee :: DebugEnv DebugM -> IO a -> IO a
pauseDebuggee e act = bracket_ (pause e) (resume e) act

dereferenceClosure :: ClosurePtr -> DebugM Closure
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

dereferenceStack :: StackCont -> DebugM Stack
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

