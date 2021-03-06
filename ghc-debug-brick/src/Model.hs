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

import Data.Sequence as Seq
import Lens.Micro.Platform
import Data.Time
import System.Directory
import System.FilePath
import Data.Text(Text, pack)

import Brick.Widgets.List
import IOTree

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

mkSocketInfo :: FilePath -> IO SocketInfo
mkSocketInfo fp = SocketInfo fp <$> getModificationTime fp

socketName :: SocketInfo -> Text
socketName = pack . takeFileName . _socketLocation

renderSocketTime :: SocketInfo -> Text
renderSocketTime = pack . formatTime defaultTimeLocale "%c" . _socketCreated

data SocketInfo = SocketInfo
                    { _socketLocation :: FilePath -- ^ FilePath to socket, absolute path
                    , _socketCreated :: UTCTime  -- ^ Time of socket creation
                    } deriving Eq

instance Ord SocketInfo where
  compare (SocketInfo s1 t1) (SocketInfo s2 t2) =
    -- Compare time first
    compare t1 t2 <> compare s1 s2

data MajorState
  -- | We have not yet connected to a debuggee.
  = Setup
    { _knownDebuggees :: GenericList Name Seq SocketInfo
    }

  -- | Connected to a debuggee
  | Connected
    { _debuggeeSocket :: SocketInfo
    , _debuggee :: Debuggee
    , _mode     :: ConnectedMode
    }

data ClosureDetails = ClosureDetails
  { _closure :: Closure
  , _labelInParent :: Text -- ^ A label describing the relationship to the parent
  -- Stuff  that requires IO to calculate
  , _pretty :: Text
  , _sourceLocation :: Maybe Text
  , _closureType :: Maybe Text
  , _constructor :: Maybe Text
  , _excSize :: Size
  , _retainerSize :: Maybe RetainerSize
  }

data TreeMode = Dominator | SavedAndGCRoots

data ConnectedMode
  -- | Debuggee is running
  = RunningMode
  -- | Debuggee is paused and we're exploring the heap
  | PausedMode
    { _treeMode :: TreeMode
    , _treeDominator :: Maybe DominatorAnalysis
    -- ^ Tree corresponding to Dominator mode
    , _treeSavedAndGCRoots :: IOTree ClosureDetails Name
    -- ^ Tree corresponding to SavedAndGCRoots mode
    }

data DominatorAnalysis = DominatorAnalysis { _getDominatorAnalysis :: Analysis
                                           , _getDominatorTree :: IOTree ClosureDetails Name
                                           }

pauseModeTree :: ConnectedMode -> IOTree ClosureDetails Name
pauseModeTree RunningMode = error "Not Paused"
pauseModeTree (PausedMode mode dom roots) = case mode of
  Dominator -> maybe (error "DOMINATOR-DavidE is not ready") _getDominatorTree dom
  SavedAndGCRoots -> roots

makeLenses ''AppState
makeLenses ''MajorState
makeLenses ''ClosureDetails
makeLenses ''ConnectedMode
makeLenses ''SocketInfo
makeLenses ''DominatorAnalysis
