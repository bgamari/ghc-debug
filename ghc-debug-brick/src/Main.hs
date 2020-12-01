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
import Data.Maybe (fromMaybe)
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

import IOTree
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

      PausedMode tree'  -> mainBorder "ghc-debug - Paused" $ vBox
        [ hBox
          [ border $ vBox
            [ txt "Resume  (r)"
            , txt "Parent  (<-)"
            , txt "Child   (->)"
            ]
          , -- Current closure details
            borderWithLabel (txt "Closure Details") $ renderClosureDetails (ioTreeSelection tree')
          ]
        , -- Tree
          borderWithLabel (txt "Closures") $ renderIOTree tree'
        ]
  ]
  where
  mainBorder title = borderWithLabel (txt title) . padAll 1

  renderClosureDetails :: Maybe ClosureDetails -> Widget Name
  renderClosureDetails cd = vLimit 8 $ vBox $
      [ txt "SourceLocation   "
            <+> txt (fromMaybe "" (_sourceLocation =<< cd))
      -- TODO these aren't actually implemented yet
      -- , txt $ "Type             "
      --       <> fromMaybe "" (_closureType =<< cd)
      -- , txt $ "Constructor      "
      --       <> fromMaybe "" (_constructor =<< cd)
      , txt $ "Exclusive Size   "
            <> maybe "" (pack . show) (_excSize <$> cd) <> " bytes"
      , fill ' '
      ]

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
          rootClosures' <- liftIO $ mapM (getClosureDetails "GC Root") =<< GD.rootClosures debuggee'
          savedClosures' <- liftIO $ mapM (getClosureDetails "Saved Object") =<< GD.savedClosures debuggee'
          let tree' = ioTree Connected_Paused_ClosureTree
                        (savedClosures' ++ rootClosures')
                        (\c -> do
                            children <- closureReferences debuggee' (_closure c)
                            mapM (\(lbl, child) -> getClosureDetails (pack lbl) child) children
                        )
                        (\selected depth closureDesc -> hBox
                                [ txt (T.replicate depth "  ")
                                , (if selected then visible . txt else txt) $
                                    (if selected then "* " else "  ")
                                    <> _labelInParent closureDesc
                                    <> "   "
                                    <> pack (closureShowAddress (_closure closureDesc))
                                    <> "   "
                                    <> _pretty closureDesc
                                ]
                        )
                        (\depth _closureDesc children -> if List.null children
                            then txt $ T.replicate depth "  " <> "<Empty>"
                            else emptyWidget
                        )

          continue (appState & majorState . mode .~ PausedMode tree')

        _ -> continue appState

      PausedMode tree' -> case brickEvent of

        -- Navigate the tree of closures
        VtyEvent event -> do
          newTree <- handleIOTreeEvent event tree'
          continue (appState & majorState . mode . tree .~ newTree)

        _ -> continue appState

      where
      getClosureDetails :: Text -> Closure -> IO ClosureDetails
      getClosureDetails label c = do
        excSize' <- closureExclusiveSize debuggee' c
        sourceLoc <- closureSourceLocation debuggee' c
        pretty' <- closurePretty debuggee' c
        return ClosureDetails
          { _closure = c
          , _pretty = pack pretty'
          , _labelInParent = label
          , _sourceLocation = Just (pack $ Prelude.unlines sourceLoc)
          , _closureType = Nothing
          , _constructor = Nothing
          , _excSize = excSize'
          }

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
