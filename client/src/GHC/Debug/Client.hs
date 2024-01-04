{- | The main API for creating debuggers. For example, this API can be used
to connect to an instrumented process, query the GC roots and then decode
the first root up to depth 10 and displayed to the user.

@
main = withDebuggeeConnect "\/tmp\/ghc-debug" p1

p1 :: Debuggee -> IO ()
p1 e = do
  pause e
  g <- run e $ do
        precacheBlocks
        (r:_) <- gcRoots
        buildHeapGraph (Just 10) r
  putStrLn (ppHeapGraph (const "") g)
@

-}
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
  , fork
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
  , dereferenceToClosurePtr
  , addConstrDesc
  , dereferenceClosures
  , dereferenceStack
  , dereferencePapPayload
  , dereferenceConDesc
  , dereferenceInfoTable
  , dereferenceSRT
  , dereferenceCCS
  , dereferenceCC

  , Hextraversable(..)

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
import Data.Bitraversable
import Control.Monad

derefFuncM :: HG.DerefFunction DebugM Size
derefFuncM c = do
  c' <- dereferenceClosure c
  hextraverse pure dereferenceSRT dereferencePapPayload dereferenceConDesc (bitraverse dereferenceSRT pure <=< dereferenceStack) pure c'

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
  r <- run e a
  resume e
  k r

