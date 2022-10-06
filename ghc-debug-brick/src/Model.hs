{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Model
  ( module Model
  , module Namespace
  , module Common
  ) where

import Data.Sequence as Seq
import Lens.Micro.Platform
import Data.Time
import System.Directory
import System.FilePath
import Data.Text(Text, pack)

import Brick.Forms
import Brick.Widgets.List
import IOTree

import Namespace
import Common
import Lib


initialAppState :: AppState
initialAppState = AppState
  { _majorState = Setup
      { _setupKind = Socket
      , _knownDebuggees = list Setup_KnownDebuggeesList [] 1
      , _knownSnapshots = list Setup_KnownSnapshotsList [] 1
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

type SnapshotInfo = SocketInfo

data SetupKind = Socket | Snapshot

toggleSetup :: SetupKind -> SetupKind
toggleSetup Socket = Snapshot
toggleSetup Snapshot = Socket

data MajorState
  -- | We have not yet connected to a debuggee.
  = Setup
    { _setupKind :: SetupKind
    , _knownDebuggees :: GenericList Name Seq SocketInfo
    , _knownSnapshots :: GenericList Name Seq SnapshotInfo
    }

  -- | Connected to a debuggee
  | Connected
    { _debuggeeSocket :: SocketInfo
    , _debuggee :: Debuggee
    , _mode     :: ConnectedMode
    }

data InfoInfo = InfoInfo
  {
    _labelInParent :: Text -- ^ A label describing the relationship to the parent
  -- Stuff  that requires IO to calculate
  , _pretty :: Text
  , _sourceLocation :: Maybe SourceInformation
  , _closureType :: Maybe Text
  , _constructor :: Maybe Text }

data ClosureDetails pap s c = ClosureDetails
  { _closure :: DebugClosure pap ConstrDesc s c
  , _retainerSize :: Maybe RetainerSize
  , _excSize :: Size
  , _info :: InfoInfo
  }
  | InfoDetails { _info :: InfoInfo }
  | LabelNode { _label :: Text }

data TreeMode = Dominator
              | SavedAndGCRoots
              | Reverse
              | Retainer (IOTree (ClosureDetails PayloadCont StackCont ClosurePtr) Name)
              | Searched (IOTree (ClosureDetails PayloadCont StackCont ClosurePtr) Name)

data FooterMode = FooterInfo
                | FooterMessage Text
                | FooterInput FooterInputMode (Form Text () Name)

data FooterInputMode = FAddress | FSearch | FProfile | FRetainer | FRetainerExact | FSnapshot

data KeybindingsMode = KeybindingsShown
                     | KeybindingsHidden

formatFooterMode :: FooterInputMode -> Text
formatFooterMode FAddress = "address (0x..): "
formatFooterMode FSearch = "search: "
formatFooterMode FProfile = "filename: "
formatFooterMode FRetainer = "constructor name: "
formatFooterMode FRetainerExact = "closure name: "
formatFooterMode FSnapshot = "snapshot name: "

data ConnectedMode
  -- | Debuggee is running
  = RunningMode
  -- | Debuggee is paused and we're exploring the heap
  | PausedMode { _pausedMode :: OperationalState }

data RootsOrigin = DefaultRoots [(Text, Ptr)]
                 | SearchedRoots [(Text, Ptr)]


currentRoots :: RootsOrigin -> [(Text, Ptr)]
currentRoots (DefaultRoots cp) = cp
currentRoots (SearchedRoots cp) = cp

data OperationalState = OperationalState
    { _treeMode :: TreeMode
    , _keybindingsMode :: KeybindingsMode
    , _footerMode :: FooterMode
    , _rootsFrom  :: RootsOrigin
    , _treeDominator :: Maybe DominatorAnalysis
    -- ^ Tree corresponding to Dominator mode
    , _treeSavedAndGCRoots :: IOTree (ClosureDetails PayloadCont StackCont ClosurePtr) Name
    -- ^ Tree corresponding to SavedAndGCRoots mode
    , _treeReverse :: Maybe ReverseAnalysis
    -- ^ Tree corresponding to Dominator mode
    , _heapGraph :: Maybe (HeapGraph Size)
    -- ^ Raw heap graph
    }

data DominatorAnalysis =
  DominatorAnalysis { _getDominatorAnalysis :: Analysis
                    , _getDominatorTree :: IOTree (ClosureDetails PayloadCont StackCont ClosurePtr) Name
                    }

data ReverseAnalysis = ReverseAnalysis { _reverseIOTree :: IOTree (ClosureDetails PapHI StackHI (Maybe HeapGraphIndex)) Name
                                          , _convertPtr :: ClosurePtr -> Maybe (DebugClosure PapHI ConstrDesc StackHI (Maybe HeapGraphIndex)) }

pauseModeTree :: (forall pap s c . IOTree (ClosureDetails pap s c) Name -> r) -> OperationalState -> r
pauseModeTree k (OperationalState mode _kb _footer _from dom roots reverseA _graph) = case mode of
  Dominator -> k $ maybe (error "DOMINATOR-DavidE is not ready") _getDominatorTree dom
  SavedAndGCRoots -> k roots
  Reverse -> k $ maybe (error "bop it, flip, reverse it, DavidE") _reverseIOTree reverseA
  Retainer r -> k r
  Searched r -> k r

makeLenses ''AppState
makeLenses ''MajorState
makeLenses ''ClosureDetails
makeLenses ''ConnectedMode
makeLenses ''OperationalState
makeLenses ''SocketInfo
makeLenses ''DominatorAnalysis
makeLenses ''ReverseAnalysis
