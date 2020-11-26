{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Data.List as List
import Data.Sequence as Seq
import Data.Text
import Graphics.Vty(defaultConfig, mkVty, defAttr)
import qualified Graphics.Vty.Input.Events as Vty
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro.Platform
import System.Directory
import System.FilePath

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.List

import GHC.Debug.Client

data Event
  = PollTick  -- Used to perform arbitrary polling based tasks e.g. looking for new debuggees

data Name
  = Main
  | Main_FileList
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _knownDebuggees :: GenericList Name Seq (Text, FilePath) -- ^ File name and full path
  }

makeLenses ''AppState

myAppDraw :: AppState -> [Widget Name]
myAppDraw appState =
  [ borderWithLabel (txt "ghc-debug")
  $ padAll 1
  $ vBox
    [ txt $ "Select a process to debug (" <> pack (show nKnownDebuggees) <> " found):"
    , renderList
        (\elIsSelected (el, _) -> hBox
            [ txt $ if elIsSelected then "*" else " "
            , txt " "
            , txt el
            ]
        )
        True
        (appState^.knownDebuggees)
    ]
  ]
  where
  nKnownDebuggees = Seq.length $ appState^.knownDebuggees.listElementsL

myAppHandleEvent :: AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
myAppHandleEvent appState brickEvent = case brickEvent of
  VtyEvent (Vty.EvKey KEsc []) -> halt appState
  VtyEvent event -> do
    handleListEvent event (appState^.knownDebuggees)
    newOptions <- handleListEvent event (appState^.knownDebuggees)
    continue $ appState & knownDebuggees .~ newOptions
  AppEvent event -> case event of
    PollTick -> do
      -- Poll for debuggees
      knownDebuggees' <- liftIO $ do
        dir :: FilePath <- socketDirectory
        debuggeeSockets :: [FilePath] <- listDirectory dir <|> return []

        let currentSelectedPathMay :: Maybe FilePath
            currentSelectedPathMay = fmap (snd . snd) (listSelectedElement (appState^.knownDebuggees))

            newSelection :: Maybe Int
            newSelection = do
              currentSelectedPath <- currentSelectedPathMay
              List.findIndex (currentSelectedPath ==) debuggeeSockets

        return $ listReplace
                  (fromList [(pack (dir </> socket), socket) | socket <- debuggeeSockets])
                  newSelection
                  (appState^.knownDebuggees)

      continue $ appState & knownDebuggees .~ knownDebuggees'

  _ -> do
    continue appState

myAppStartEvent :: AppState -> EventM Name AppState
myAppStartEvent = return

myAppAttrMap :: AppState -> AttrMap
myAppAttrMap _appState = attrMap defAttr []

main :: IO ()
main = do
  eventChan <- newBChan 10
  forkIO $ forever $ do
    writeBChan eventChan PollTick
    threadDelay 2000000
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  let app :: App AppState Event Name
      app = App
        { appDraw = myAppDraw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = myAppHandleEvent
        , appStartEvent = myAppStartEvent
        , appAttrMap = myAppAttrMap
        }
      initialState = AppState
        { _knownDebuggees = list Main_FileList [] 1
        }
  _finalState <- customMain initialVty buildVty
                    (Just eventChan) app initialState
  return ()
