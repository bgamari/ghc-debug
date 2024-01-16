{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}

module Model
  ( module Model
  , module Namespace
  , module Common
  ) where

import Data.Maybe (fromMaybe)
import Data.Sequence as Seq
import Lens.Micro.Platform
import Data.Time
import System.Directory
import System.FilePath
import Data.Text(Text, pack)
import qualified Data.Text as T
import Text.Read

import Brick.Forms
import Brick.BChan
import Brick (EventM, Widget)
import Brick.Widgets.List

import Namespace
import Common
import Lib
import IOTree
import Control.Concurrent
import qualified Graphics.Vty as Vty
import Data.Word

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
  , _constructor :: Maybe Text
  , _profHeaderInfo :: !(Maybe ProfHeaderWord)
  }

data ClosureDetails = ClosureDetails
  { _closure :: DebugClosure CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr
  , _excSize :: Size
  , _info :: InfoInfo
  }
  | InfoDetails { _info :: InfoInfo }
  | LabelNode { _label :: Text }

data TreeMode = SavedAndGCRoots (ClosureDetails -> Widget Name)
              | Retainer (ClosureDetails -> Widget Name) (IOTree (ClosureDetails) Name)
              | forall a . Searched (a -> Widget Name) (IOTree a Name)

treeLength :: TreeMode -> Maybe Int
treeLength (SavedAndGCRoots {}) = Nothing
treeLength (Retainer _ tree) = Just $ Prelude.length $ getIOTreeRoots tree
treeLength (Searched _ tree) = Just $ Prelude.length $ getIOTreeRoots tree

data FooterMode = FooterInfo
                | FooterMessage Text
                | FooterInput FooterInputMode (Form Text () Name)

isFocusedFooter :: FooterMode -> Bool
isFocusedFooter (FooterInput {}) = True
isFocusedFooter _ = False

data FooterInputMode = FAddress Bool
                     | FInfoTable Bool
                     | FRetainer Bool
                     | FRetainerExact Bool
                     | FRetainerArrWords Bool
                     | FFilterEras Bool
                     | FFilterClosureType
                     | FFilterClosureSize
                     | FProfile
                     | FSnapshot
                     | FDumpArrWords
                     | FSetResultSize
                     deriving Show

data Command = Command { commandDescription :: Text
                       , commandKey :: Maybe Vty.Event
                       , dispatchCommand :: Debuggee -> EventM Name OperationalState ()
                       }

mkCommand :: Text -> Vty.Event -> EventM Name OperationalState () -> Command
mkCommand desc key dispatch = Command desc (Just key) (\_ -> dispatch)

data OverlayMode = KeybindingsShown
                 -- TODO: Abstract the "CommandPicker" into it's own module
                 | CommandPicker (Form Text () Name) (GenericList Name Seq Command) (Seq Command)
                 | NoOverlay


formatFooterMode :: FooterInputMode -> Text
formatFooterMode (FAddress _) = "address (0x..): "
formatFooterMode (FInfoTable _) = "info table pointer (0x..): "
formatFooterMode (FRetainer _) = "constructor name: "
formatFooterMode (FRetainerExact _) = "closure name: "
formatFooterMode (FRetainerArrWords _) = "size (bytes): "
formatFooterMode (FFilterEras _) = "era range (<era>/<start-era>-<end-era>): "
formatFooterMode FDumpArrWords = "dump payload to file: "
formatFooterMode FSetResultSize = "search result limit (0 for infinity): "
formatFooterMode FFilterClosureSize = "closure size (bytes): "
formatFooterMode FFilterClosureType = "closure type: "
formatFooterMode FSnapshot = "snapshot name: "
formatFooterMode FProfile = "filename: "

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
    , _resultSize :: Maybe Int
    , _filters :: [UIFilter]
    }

clearFilters :: OperationalState -> OperationalState
clearFilters os = os { _filters = [] }

setFilters :: [UIFilter] -> OperationalState -> OperationalState
setFilters fs os = os {_filters = fs}

addFilters :: [UIFilter] -> OperationalState -> OperationalState
addFilters fs os = os {_filters = fs ++ _filters os}

data UIFilter =
    UIAddressFilter ClosurePtr
  | UIInfoAddressFilter InfoTablePtr
  | UIConstructorFilter String
  | UIInfoNameFilter String
  | UIEraFilter EraRange
  | UISizeFilter Size
  | UIClosureTypeFilter ClosureType

uiFiltersToFilter :: [UIFilter] -> Filter
uiFiltersToFilter = foldr AndFilter (PureFilter True) . map uiFilterToFilter

uiFilterToFilter :: UIFilter -> Filter
uiFilterToFilter (UIAddressFilter x)     = AddressFilter (== x)
uiFilterToFilter (UIInfoAddressFilter x) = InfoPtrFilter (== x)
uiFilterToFilter (UIConstructorFilter x) = ConstructorDescFilter ((== x) . name)
uiFilterToFilter (UIInfoNameFilter x)    = InfoSourceFilter ((== x) . infoName)
uiFilterToFilter (UIEraFilter  x)        = ProfHeaderFilter (`profHeaderInEraRange` (Just x))
uiFilterToFilter (UISizeFilter x)        = SizeFilter (>= x)
uiFilterToFilter (UIClosureTypeFilter x)        = InfoFilter ((== x) . tipe)

parseEraRange :: Text -> Maybe EraRange
parseEraRange range = case T.splitOn "-" range of
  [nstr] -> case readMaybe (T.unpack nstr) of
    Just n -> Just $ EraRange n n
    Nothing -> Nothing
  [start,end] -> case (T.unpack start, T.unpack end) of
    ("", "") -> Just $ EraRange 0 maxBound
    ("", readMaybe -> Just e) -> Just $ EraRange 0 e
    (readMaybe -> Just s, "") -> Just $ EraRange s maxBound
    (readMaybe -> Just s, readMaybe -> Just e) -> Just $ EraRange s e
    _ -> Nothing
  _ -> Nothing

showEraRange :: EraRange -> String
showEraRange (EraRange s e)
  | s == e = show s
  | otherwise = "[" ++ show s ++ "," ++ go e
  where
    go n
      | n == maxBound = "∞)"
      | otherwise = show n ++ "]"

osSize :: OperationalState -> Int
osSize os = fromMaybe (Prelude.length (getIOTreeRoots $ _treeSavedAndGCRoots os)) $ treeLength (_treeMode os)

pauseModeTree :: (forall a . (a -> Widget Name) -> IOTree a Name -> r) -> OperationalState -> r
pauseModeTree k (OperationalState _ mode _kb _footer _from roots _ _ _) = case mode of
  SavedAndGCRoots render -> k render roots
  Retainer render r -> k render r
  Searched render r -> k render r

makeLenses ''AppState
makeLenses ''MajorState
makeLenses ''ClosureDetails
makeLenses ''ConnectedMode
makeLenses ''OperationalState
makeLenses ''SocketInfo
makeLenses ''OverlayMode
