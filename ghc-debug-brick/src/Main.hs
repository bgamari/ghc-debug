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
import Data.Maybe (maybeToList)
import qualified Data.Sequence as Seq
import Data.Text
import Graphics.Vty(defaultConfig, mkVty, defAttr)
import qualified Graphics.Vty.Input.Events as Vty
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import qualified Data.Text as T
import Data.Ord

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.List

import GHC.Debug.Client as GD

import Model

data Event
  = PollTick  -- Used to perform arbitrary polling based tasks e.g. looking for new debuggees

myAppDraw :: AppState -> [Widget Name]
myAppDraw (AppState majorState') =
  [ case majorState' of

    Setup knownDebuggees' -> let
      nKnownDebuggees = Seq.length $ majorState'^.knownDebuggees.listElementsL
      in mainBorder "ghc-debug" $ vBox
        [ txt $ "Select a process to debug (" <> pack (show nKnownDebuggees) <> " found):"
        , renderList
            (\elIsSelected socketPath -> hBox
                [ txt $ if elIsSelected then "*" else " "
                , txt " "
                , txt (socketName socketPath)
                , txt " - "
                , txt (renderSocketTime socketPath)
                ]
            )
            True
            knownDebuggees'
        ]

    Connected _socket _debuggee mode' -> case mode' of

      RunningMode -> mainBorder "ghc-debug - Running" $ vBox
        [ txt "Pause (p)"
        ]

      PausedMode parents' closureMay' references'  -> mainBorder "ghc-debug - Paused" $ vBox
        [ border $ vBox
          [ txt "Resume  (r)"
          , txt "Parent  (<-)"
          , txt "Child   (->)"
          ]
        , borderWithLabel (txt "Path") $ vBox $
            [txt (showClosure parent) | parent <- List.reverse parents']
            ++ [txt (maybe "<ROOT>" (\(c,_) -> showClosure c) closureMay')]
        -- Current closure
        , let
          refListWidget = borderWithLabel (txt "Children") $ renderList
                (\selected refClosure -> txt $
                  (if selected then "* " else "  ")
                  <> showClosure refClosure
                )
                True
                references'
          in case closureMay' of
            Nothing -> vBox
              [ refListWidget
              ]
            Just (_, closureExcSize) -> vBox
              -- Size
              [ str $ "exclusive size: " <> (show $ closureExcSize)
              -- References
              , refListWidget
              ]
        ]
  ]
  where
  mainBorder title = borderWithLabel (txt title) . padAll 1

  showClosure :: Closure -> Text
  showClosure closure' = pack $ show $ closurePtr closure'

myAppHandleEvent :: AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
myAppHandleEvent appState@(AppState majorState') brickEvent = case brickEvent of
  VtyEvent (Vty.EvKey KEsc []) -> halt appState
  _ -> case majorState' of
    Setup knownDebuggees' -> case brickEvent of

      VtyEvent event -> case event of
        -- Connect to the selected debuggee
        Vty.EvKey KEnter _
          | Just (_debuggeeIx, socket) <- listSelectedElement knownDebuggees'
          -> do
            debuggee' <- liftIO $ debuggeeConnect (T.unpack (socketName socket)) (view socketLocation socket)
            continue $ appState & majorState .~ Connected
                  { _debuggeeSocket = socket
                  , _debuggee = debuggee'
                  , _mode     = RunningMode  -- TODO should we query the debuggee for this?
                  }

        -- Navigate through the list.
        _ -> do
          newOptions <- handleListEventVi handleListEvent event knownDebuggees'
          continue $ appState & majorState . knownDebuggees .~ newOptions

      AppEvent event -> case event of
        PollTick -> do
          -- Poll for debuggees
          knownDebuggees'' <- liftIO $ do
            dir :: FilePath <- socketDirectory
            debuggeeSocketFiles :: [FilePath] <- listDirectory dir <|> return []

            -- Sort the sockets by the time they have been created, newest
            -- first.
            debuggeeSockets <- List.sortBy (comparing Data.Ord.Down)
                                  <$> mapM (mkSocketInfo . (dir </>)) debuggeeSocketFiles

            let currentSelectedPathMay :: Maybe SocketInfo
                currentSelectedPathMay = fmap snd (listSelectedElement knownDebuggees')

                newSelection :: Maybe Int
                newSelection = do
                  currentSelectedPath <- currentSelectedPathMay
                  List.findIndex ((currentSelectedPath ==)) debuggeeSockets

            return $ listReplace
                      (Seq.fromList debuggeeSockets)
                      (newSelection <|> (if Prelude.null debuggeeSockets then Nothing else Just 0))
                      knownDebuggees'

          continue $ appState & majorState . knownDebuggees .~ knownDebuggees''

      _ -> continue appState

    Connected _socket' debuggee' mode' -> case mode' of

      RunningMode -> case brickEvent of
        -- Pause the debuggee
        VtyEvent (Vty.EvKey (KChar 'p') []) -> do
          liftIO $ pause debuggee'
          continueWithRoot appState
        _ -> continue appState

      PausedMode parents' closureMay' refs' -> case brickEvent of

        -- Resume the debuggee
        VtyEvent (Vty.EvKey (KChar 'r') _) -> do
          liftIO $ do
            resume debuggee'
          continue (appState & majorState . mode .~ RunningMode)

        -- Goto Parent
        VtyEvent (Vty.EvKey KLeft _) -> case parents' of
          parent:ancestors -> continueWithClosure appState (Just parent) ancestors
          [] -> continueWithClosure appState Nothing []

        -- Goto Selected reference
        VtyEvent (Vty.EvKey KRight _)
          | Just (_, refClosure) <- listSelectedElement refs'
          -> continueWithClosure appState (Just refClosure) (maybeToList (fst <$> closureMay') ++ parents')

        -- Navigate the list of referenced closures
        VtyEvent event -> do
          newRefs <- handleListEventVi handleListEvent event refs'
          continue $ appState & majorState . mode . references .~ newRefs

        _ -> continue appState

        where
        continueWithClosure appState' closureMay' parents = case closureMay' of
          Nothing -> continueWithRoot appState'
          Just closure' -> do
            refsList <- liftIO $ closureReferences debuggee' closure'
            let newRefsList = listReplace
                        (Seq.fromList refsList)
                        (if Prelude.null refsList then Nothing else Just 0)
                        refs'
            closureExcSize <- liftIO $ closureExclusiveSize debuggee' closure'
            continue $ appState'
              & majorState
              . mode
              .~ PausedMode parents (Just (closure', closureExcSize)) newRefsList
      where
      continueWithRoot appState = do
          rootClosuresList <- liftIO $ GD.rootClosures debuggee'
          continue (appState & majorState . mode .~ PausedMode
            { _closurePath = []
            , _closure = Nothing
            , _references = list
                Connected_Paused_SavedClosuresList
                (Seq.fromList rootClosuresList)
                1
            })

myAppStartEvent :: AppState -> EventM Name AppState
myAppStartEvent = return

myAppAttrMap :: AppState -> AttrMap
myAppAttrMap _appState = attrMap defAttr []

main :: IO ()
main = do
  eventChan <- newBChan 10
  _ <- forkIO $ forever $ do
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
  _finalState <- customMain initialVty buildVty
                    (Just eventChan) app initialAppState
  return ()
