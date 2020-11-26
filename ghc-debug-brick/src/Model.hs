{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}

module Model
  ( module Model
  , module Namespace
  ) where

-- import Control.Applicative
-- import Control.Monad (forever)
-- import Control.Monad.IO.Class
-- import Control.Concurrent
-- import qualified Data.List as List
import Data.Sequence as Seq
import Data.Text
-- import Graphics.Vty(defaultConfig, mkVty, defAttr)
-- import qualified Graphics.Vty.Input.Events as Vty
-- import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro.Platform
-- import System.Directory
-- import System.FilePath

-- import Brick
-- import Brick.BChan
-- import Brick.Widgets.Border
import Brick.Widgets.List

import GHC.Debug.Client

import Namespace

initialAppState :: AppState
initialAppState = AppState
  { _majorState = Setup
      { _knownDebuggees = list Setup_KnownDebuggeesList [] 1
      }
  }

data AppState = AppState
  { _majorState :: MajorState
  }


data MajorState
  -- | We have not yet connected to a debuggee.
  = Setup
    { _knownDebuggees :: GenericList Name Seq FilePath -- ^ File path to socket
    }

  -- | Connected to a debuggee
  | Connected
    { _debuggeeSocket :: FilePath
    , _debuggee :: Debuggee
    , _mode     :: ConnectedMode
    }

data ConnectedMode
  -- | Debuggee is running
  = RunningMode
  -- | Debuggee is paused
  | PausedMode
    { _savedClosures :: GenericList Name Seq Closure
    }

makeLenses ''AppState
makeLenses ''MajorState
makeLenses ''ConnectedMode
