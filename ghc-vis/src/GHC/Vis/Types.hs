{-# LANGUAGE RankNTypes #-}
{- |
   Module      : GHC.Vis.Types
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.Types (
  Point,
  DrawFunction,
  Signal(..),
  View(..),
  ViewType(..),
  State(..),
  Identifier,
  NamedBox,
  Box,
  PState(..),
  PrintState,
  VisObject(..),

  Render

  )
  where

--import GHC.Heap.Graph
import qualified Control.Monad.State as MS
import Graphics.UI.Threepenny hiding (Point)

import GHC.Debug.Types.Graph
import GHC.Debug.Types.Ptr

type Render = UI

--import Graphics.UI.Gtk hiding (Box, Signal, Point)
--import Graphics.Rendering.Cairo hiding (x)

-- | A simple Point
type Point = (Double, Double)

-- | A function to draw a cairo drawing to a file.
--type DrawFunction = forall a. FilePath -> Double -> Double -> (Surface -> IO a) -> IO a
--
type DrawFunction = ()

-- | Signals that are sent to the GUI to signal changes
data Signal = NewSignal Box String -- ^ Add a new Box to be visualized
            | UpdateSignal         -- ^ Update the view
            | RedrawSignal         -- ^ Redraw
            | ClearSignal          -- ^ Remove all Boxes
            | RestoreSignal        -- ^ Reset all hidden boxes
            | SwitchSignal         -- ^ Switch to alternative view
            | HistorySignal (Int -> Int) -- ^ Change position in history

-- | All functions a view has to provide
data View = View
  { redraw        :: Canvas -> Render ()
  , click         :: IO ()
  , move          :: Canvas -> UI ()
  , updateObjects :: [NamedBox] -> IO ()
  }

-- | Visualization views
data ViewType = ListView
              | GraphView
              deriving (Enum, Show)

-- | The global state used for the visualizations.
data State = State
  { mousePos   :: Point    -- ^ Current position of the mouse, updated with every movement
  , view       :: ViewType -- ^ What kind of visualization is currently running
  , zoomRatio  :: Double   -- ^ How much the window is zoomed in
  , position   :: Point    -- ^ Current position in the zoom
  , dragging   :: Bool     -- ^ Whether the mouse is dragging
  , wasDragged :: Bool     -- ^ Whether the mouse was actually dragged
  , heapDepth  :: Int      -- ^ Maximum heap depth to follow
  } deriving Show

-- | Identifier of a closure
type Identifier = [String]

-- | A box and its identifier
type NamedBox = (Identifier, Box)

type Box = ClosurePtr

-- | Internal state of the list view generator
data PState = PState
  { tCounter' :: Integer
  , fCounter' :: Integer
  , xCounter' :: Integer
  , bindings  :: [HeapGraphIndex]
  , heapGraph :: HeapGraph String
  }

-- | The state of a printing operation
type PrintState = MS.State PState

-- | Simple representation of objects on the heap, so they can be arranged linearly
data VisObject = Unnamed String
               | Named String [VisObject]
               | Link String
               | Thunk String
               | Function String
               deriving (Eq, Show)
