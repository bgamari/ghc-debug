{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

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

data FooterInputMode = FClosureAddress {runNow :: Bool, invert :: Bool}
                     | FInfoTableAddress {runNow :: Bool, invert :: Bool}
                     | FConstructorName {runNow :: Bool, invert :: Bool}
                     | FClosureName {runNow :: Bool, invert :: Bool}
                     | FArrWordsSize
                     | FFilterEras {runNow :: Bool, invert :: Bool}
                     | FFilterClosureType {invert :: Bool}
                     | FFilterClosureSize {invert :: Bool}
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


invertInput :: FooterInputMode -> FooterInputMode
invertInput x@FClosureAddress{invert} = x{invert = not invert}
invertInput x@FInfoTableAddress{invert} = x{invert = not invert}
invertInput x@FConstructorName{invert} = x{invert = not invert}
invertInput x@FClosureName{invert} = x{invert = not invert}
invertInput x@FFilterEras{invert} = x{invert = not invert}
invertInput x@FFilterClosureSize{invert} = x{invert = not invert}
invertInput x@FFilterClosureType{invert} = x{invert = not invert}
invertInput x = x

formatFooterMode :: FooterInputMode -> Text
formatFooterMode FClosureAddress{invert} = (if invert then "!" else "") <> "address (0x..): "
formatFooterMode FInfoTableAddress{invert} = (if invert then "!" else "") <> "info table pointer (0x..): "
formatFooterMode FConstructorName{invert} = (if invert then "!" else "") <> "constructor name: "
formatFooterMode FClosureName{invert} = (if invert then "!" else "") <> "closure name: "
formatFooterMode FFilterEras{invert} = (if invert then "!" else "") <> "era range (<era>/<start-era>-<end-era>): "
formatFooterMode FFilterClosureSize{invert} = (if invert then "!" else "") <> "closure size (bytes): "
formatFooterMode FFilterClosureType{invert} = (if invert then "!" else "") <> "closure type: "
formatFooterMode FArrWordsSize = "size (bytes)>= "
formatFooterMode FDumpArrWords = "dump payload to file: "
formatFooterMode FSetResultSize = "search result limit (0 for infinity): "
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
    UIAddressFilter Bool ClosurePtr
  | UIInfoAddressFilter Bool InfoTablePtr
  | UIConstructorFilter Bool String
  | UIInfoNameFilter Bool String
  | UIEraFilter Bool EraRange
  | UISizeFilter Bool Size
  | UIClosureTypeFilter Bool ClosureType

uiFiltersToFilter :: [UIFilter] -> ClosureFilter
uiFiltersToFilter = foldr AndFilter (PureFilter True) . map uiFilterToFilter

uiFilterToFilter :: UIFilter -> ClosureFilter
uiFilterToFilter (UIAddressFilter invert x)     = AddressFilter (xor invert . (== x))
uiFilterToFilter (UIInfoAddressFilter invert x) = InfoPtrFilter (xor invert . (== x))
uiFilterToFilter (UIConstructorFilter invert x) = ConstructorDescFilter (xor invert . (== x) . name)
uiFilterToFilter (UIInfoNameFilter invert x)    = InfoSourceFilter (xor invert . (== x) . infoName)
uiFilterToFilter (UIEraFilter  invert x)        = ProfHeaderFilter (xor invert . (`profHeaderInEraRange` (Just x)))
uiFilterToFilter (UISizeFilter invert x)        = SizeFilter (xor invert . (>= x))
uiFilterToFilter (UIClosureTypeFilter invert x) = InfoFilter (xor invert . (== x) . tipe)

xor :: Bool -> Bool -> Bool
xor False False = False
xor True True = False
xor _ _ = True

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
