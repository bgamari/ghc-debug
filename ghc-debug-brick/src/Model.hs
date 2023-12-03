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
import Brick.BChan
import Brick (EventM)
import Brick.Widgets.List
import IOTree

import Namespace
import Common
import Lib
import Control.Concurrent
import qualified Graphics.Vty as Vty

data Event
  = PollTick  -- Used to perform arbitrary polling based tasks e.g. looking for new debuggees
  | ProgressMessage Text
  | ProgressFinished
  | AsyncFinished (EventM Name OperationalState ())


initialAppState :: BChan Event -> AppState
initialAppState event = AppState
  { _majorState = Setup
      { _setupKind = Socket
      , _knownDebuggees = list Setup_KnownDebuggeesList [] 1
      , _knownSnapshots = list Setup_KnownSnapshotsList [] 1
      },
    _appChan = event
  }

data AppState = AppState
  { _majorState :: MajorState
  , _appChan    :: BChan Event
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

data ClosureDetails = ClosureDetails
  { _closure :: DebugClosure SrtCont PayloadCont ConstrDesc StackCont ClosurePtr
  , _retainerSize :: Maybe RetainerSize
  , _excSize :: Size
  , _info :: InfoInfo
  }
  | InfoDetails { _info :: InfoInfo }
  | LabelNode { _label :: Text }

data TreeMode = SavedAndGCRoots
              | Retainer (IOTree (ClosureDetails) Name)
              | Searched (IOTree (ClosureDetails) Name)

data FooterMode = FooterInfo
                | FooterMessage Text
                | FooterInput FooterInputMode (Form Text () Name)

isFocusedFooter :: FooterMode -> Bool
isFocusedFooter (FooterInput {}) = True
isFocusedFooter _ = False

data FooterInputMode = FAddress
                     | FSearch
                     | FInfoTable
                     | FProfile
                     | FRetainer
                     | FRetainerExact
                     | FSnapshot
                     | FRetainerArrWords
                     | FDumpArrWords

data Command = Command { commandDescription :: Text
                       , commandKey :: Vty.Event
                       , dispatchCommand :: EventM Name OperationalState ()
                       }

data OverlayMode = KeybindingsShown
                 -- TODO: Abstract the "CommandPicker" into it's own module
                 | CommandPicker (Form Text () Name) (GenericList Name Seq Command)
                 | NoOverlay


formatFooterMode :: FooterInputMode -> Text
formatFooterMode FAddress = "address (0x..): "
formatFooterMode FSearch = "search: "
formatFooterMode FInfoTable = "info table pointer (0x..): "
formatFooterMode FProfile = "filename: "
formatFooterMode FRetainer = "constructor name: "
formatFooterMode FRetainerExact = "closure name: "
formatFooterMode FRetainerArrWords = "size (bytes): "
formatFooterMode FDumpArrWords = "dump payload to file: "
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
    { _running_task :: Maybe ThreadId
    , _treeMode :: TreeMode
    , _keybindingsMode :: OverlayMode
    , _footerMode :: FooterMode
    , _rootsFrom  :: RootsOrigin
    , _treeSavedAndGCRoots :: IOTree (ClosureDetails) Name
    -- ^ Tree corresponding to SavedAndGCRoots mode
    , _event_chan :: BChan Event
    }

pauseModeTree :: (IOTree ClosureDetails Name -> r) -> OperationalState -> r
pauseModeTree k (OperationalState _ mode _kb _footer _from roots _) = case mode of
  SavedAndGCRoots -> k roots
  Retainer r -> k r
  Searched r -> k r

makeLenses ''AppState
makeLenses ''MajorState
makeLenses ''ClosureDetails
makeLenses ''ConnectedMode
makeLenses ''OperationalState
makeLenses ''SocketInfo
makeLenses ''OverlayMode
