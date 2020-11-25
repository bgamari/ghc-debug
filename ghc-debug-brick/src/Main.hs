{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Brick
import Brick.BChan
import Brick.Widgets.List
import Data.Sequence
import Data.Text
import Graphics.Vty(defaultConfig, mkVty, defAttr)
import qualified Graphics.Vty.Input.Events as Vty
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro.Platform
import Brick.Widgets.Border

import GHC.Debug.Client

-- data AppState = AppState
--   -- | Known debuggees that we could connect to.
--   { stateKnownDebuggees :: [Debuggee]
--   , selectedDebuggee :: Maybe Debuggee
--   }

-- data Debuggee = Debuggee FilePath

-- data Event
--   = Closed
--   | SetKnownDebuggees [Debuggee]
--   -- | SetSelectedDebuggee Debuggee
--   | NoOp

-- view' :: AppState -> AppView Window Event
-- view' state
--   = bin Window
--       [ #title := "ghc-debug"
--       , on #deleteEvent (const (True, Closed))
--       , #widthRequest := 400
--       , #heightRequest := 300
--       ]
--       $ container Box []
--         [ -- widget ComboBox [#model := [("Hello ghc-debug World!" :: String)]]
--           NoOp <$ BoxChild defaultBoxChildProperties (CB.comboBox [] [(1 :: Int, "adsfadsf1"), (2, "2")])
--         ]

-- update' :: AppState -> Event -> Transition AppState Event
-- -- update' AppState {..} (Greet who) =
-- --   Transition AppState { greetings = greetings <> [who] } (pure Nothing)
-- update' _ Closed = Exit

data Event

data Name
  = Main
  | Main_FileList
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _options :: GenericList Name Seq Text
  }

makeLenses ''AppState

myAppDraw :: AppState -> [Widget Name]
myAppDraw (AppState options) =
  [ borderWithLabel (txt "ghc-debug")
  $ padAll 1
  $ vBox
    [ txt "Hello Brick World!"
    , renderList
        (\elIsSelected el -> hBox
            [ txt $ if elIsSelected then "*" else " "
            , txt " "
            , txt el
            ]
        )
        True
        options
    ]
  ]

myAppHandleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
myAppHandleEvent appState brickEvent = case brickEvent of
  VtyEvent (Vty.EvKey KEsc []) -> halt appState
  VtyEvent event -> do
    handleListEvent event (appState^.options)
    newOptions <- handleListEvent event (appState^.options)
    continue $ appState & options .~ newOptions
  _ -> do
    continue appState

myAppStartEvent :: AppState -> EventM Name AppState
myAppStartEvent = return

myAppAttrMap :: AppState -> AttrMap
myAppAttrMap _appState = attrMap defAttr []

main :: IO ()
main = do
  eventChan <- newBChan 10
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
  _finalState <- customMain initialVty buildVty
                    (Just eventChan) app (initialState (list Main_FileList ["A", "BB", "CC"] 1))
  return ()
