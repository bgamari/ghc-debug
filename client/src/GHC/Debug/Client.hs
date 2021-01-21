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
module GHC.Debug.Client
  ( -- * Running/Connecting to a debuggee
    Debuggee
  , DebugM
  , debuggeeRun
  , debuggeeConnect
  , debuggeeClose
  , withDebuggeeRun
  , withDebuggeeConnect
  , socketDirectory
  , snapshotRun

    -- * Running DebugM
  , run
  , runTrace
  , runAnalysis

    -- * Pause/Resume
  , pause
  , pauseThen
  , resume
  , pausePoll
  , withPause

  -- * Basic Requests
  , version
  , gcRoots
  , allBlocks
  , getSourceInfo
  , savedObjects
  , precacheBlocks
  , dereferenceClosure
  , dereferenceClosures
  , dereferenceStack
  , dereferencePapPayload
  , dereferenceConDesc
  , dereferenceInfoTable

  , Quadtraversable(..)

  -- * Building a Heap Graph
  , buildHeapGraph
  , multiBuildHeapGraph
  , HG.HeapGraph(..)
  , HG.HeapGraphEntry(..)

  -- * Printing a heap graph
  , HG.ppHeapGraph

  -- * Tracing
  , traceWrite
  , traceMsg

  -- * Caching
  , saveCache
  , loadCache

  -- * Types
  , module GHC.Debug.Types.Closures
  , SourceInformation(..)
  , RawBlock(..)
  , BlockPtr
  , StackPtr
  , ClosurePtr
  , InfoTablePtr
  , HG.StackHI
  , HG.PapHI
  , HG.HeapGraphIndex
  ) where

import           GHC.Debug.Types
import           GHC.Debug.Types.Closures
import           GHC.Debug.Convention (socketDirectory)
import GHC.Debug.Client.Monad
import           GHC.Debug.Client.Query
import qualified GHC.Debug.Types.Graph as HG
import Data.List.NonEmpty (NonEmpty)

derefFuncM :: HG.DerefFunction DebugM Size
derefFuncM c = do
  c' <- dereferenceClosure c
  quadtraverse dereferencePapPayload dereferenceConDesc dereferenceStack pure c'

-- | Build a heap graph starting from the given root. The first argument
-- controls how many levels to recurse. You nearly always want to set this
-- to a small number ~ 10, as otherwise you can easily run out of memory.
buildHeapGraph :: Maybe Int -> ClosurePtr -> DebugM (HG.HeapGraph Size)
buildHeapGraph = HG.buildHeapGraph derefFuncM

-- | Build a heap graph starting from multiple roots. The first argument
-- controls how many levels to recurse. You nearly always want to set this
-- value to a small number ~ 10 as otherwise you can easily run out of
-- memory.
multiBuildHeapGraph :: Maybe Int -> NonEmpty ClosurePtr -> DebugM (HG.HeapGraph Size)
multiBuildHeapGraph = HG.multiBuildHeapGraph derefFuncM

-- | Perform the given analysis whilst the debuggee is paused, then resume
-- and apply the continuation to the result.
runAnalysis :: DebugM a -> (a -> IO r) -> Debuggee -> IO r
runAnalysis a k e = do
  pause e
  r <- run e $ a
  resume e
  k r

