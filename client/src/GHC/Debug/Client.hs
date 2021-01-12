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
  , debuggeeRun
  , debuggeeConnect
  , debuggeeClose
  , withDebuggeeRun
  , withDebuggeeConnect
  , socketDirectory
  , snapshotRun

    -- * Pause/Resume
  , pause
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
  , SourceInformation(..)
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
  , traceFromM
  , makeSnapshot
  , DebugEnv
  , DebugM
  ) where

import           GHC.Debug.Types hiding (Closure, DebugClosure)
import           GHC.Debug.Decode
import           GHC.Debug.Convention (socketDirectory)
import GHC.Debug.Client.Monad
import           GHC.Debug.Client.Query
import           GHC.Debug.Client.Snapshot
import           GHC.Debug.Client.Trace
import qualified GHC.Debug.Types.Graph as HG


