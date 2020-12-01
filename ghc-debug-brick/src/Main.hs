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

      PausedMode path' currentCD selectedCD references'  -> mainBorder "ghc-debug - Paused" $ hBox
        [ hLimit 50 $ vBox
          [ border $ vBox
              [ txt "Resume  (r)"
              , txt "Parent  (<-)"
              , txt "Child   (->)"
              ]
          , borderWithLabel (txt "Path") $ vBox $
              [txt "<ROOT>"]
              ++ [txt (pack $ closureShowAddress closure') | (closure', _, _, _) <- List.reverse path']
          ]
        , padLeft (Pad 1) $ vBox
          [ -- Current closure details
            borderWithLabel (txt "Closure Details") $ renderClosureDetails currentCD
            -- Current closure's references
            , let
              refListWidget = borderWithLabel (txt "Children") $ vBox
                [ renderRow "  " "Pointer Field" "Pointer" "Value"
                , renderRow "  " "-------------" "-------" "-----"
                , renderList
                    renderClosureRow
                    True
                    references'
                ]
              in case path' of
                [] -> vBox
                  [ refListWidget
                  ]
                (_, prettyClosure, _, _):_ -> vBox
                  -- Pretty print current closure
                  [ borderWithLabel (txt "Closure") (txt prettyClosure)
                  -- References
                  , refListWidget
                  ]
            -- Selected closure details
            , borderWithLabel (txt "Child Closure Details") $ renderClosureDetails selectedCD
          ]
        ]
  ]
  where
  mainBorder title = borderWithLabel (txt title) . padAll 1

  renderClosureDetails :: Maybe ClosureDetails -> Widget Name
  renderClosureDetails cd = vBox $
      [ txt "SourceLocation   "
            <+> txt (fromMaybe "" (_sourceLocation =<< cd))
      -- TODO these aren't actually implemented yet
      -- , txt $ "Type             "
      --       <> fromMaybe "" (_closureType =<< cd)
      -- , txt $ "Constructor      "
      --       <> fromMaybe "" (_constructor =<< cd)
      , txt $ "Exclusive Size   "
            <> maybe "" (pack . show) (_excSize =<< cd) <> " bytes"
      ]

  renderClosureRow :: Bool -> (Closure, Text, Text) -> Widget Name
  renderClosureRow selected (closure', label, pretty) = renderRow
    (if selected then "* " else "  ")
    label
    (pack $ closureShowAddress closure')
    pretty

  renderRow :: Text -> Text -> Text -> Text -> Widget Name
  renderRow selected label address value = hBox
    [ vLimit 1 $ hLimit 2  (txt selected)
    , space
    , vLimit 1 $ hLimit 20 $ padRight Max (txt label)
    , space
    , vLimit 1 $ hLimit 15 $ padLeft  Max (txt address)
    , space
    , vLimit 1                            (txt value)
    ]
    where
    space = txt "  "

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
          continueWithRoot appState Nothing
        _ -> continue appState

      PausedMode path' _ _ refs' -> case brickEvent of

        -- Resume the debuggee
        VtyEvent (Vty.EvKey (KChar 'r') _) -> do
          liftIO $ do
            resume debuggee'
          continue (appState & majorState . mode .~ RunningMode)

        -- Goto Parent
        VtyEvent (Vty.EvKey KLeft _)
          | (_, _, ixInParentRefs, _):parents' <- path'
          -> continueWithClosure appState parents' (Just ixInParentRefs)

        -- Goto Selected reference
        VtyEvent (Vty.EvKey KRight _)
          | Just (refClosureIx, (refClosure, _, refClosurePretty)) <- listSelectedElement refs'
          -> do
            closureExcSize <- liftIO $ closureExclusiveSize debuggee' refClosure
            continueWithClosure appState ((refClosure, refClosurePretty, refClosureIx, closureExcSize):path') Nothing

        -- Navigate the list of referenced closures
        VtyEvent event -> do
          newRefs <- handleListEventVi handleListEvent event refs'
          appState' <- updateSelectedRefClosureDetails $ appState & majorState . mode . references .~ newRefs
          continue appState'

        _ -> continue appState

        where
        -- continueWithClosure :: AppState -> [(Closure, Int, Int)] -> Maybe Int -> _
        continueWithClosure appState' path'' ixMay = case path'' of
          [] -> continueWithRoot appState' ixMay
          (closure', _, _, _):_ -> do
            closureDetails <- liftIO $ getClosureDetails closure'
            refsList       <- liftIO $ closureReferences debuggee' closure'
            refPrettysList <- closuresToPretty (snd <$> refsList)
            let newRefsList = listReplace
                        (Seq.fromList [(c, pack lbl, pretty) | ((lbl, c), pretty) <- List.zip refsList refPrettysList])
                        (ixMay <|> if Prelude.null refsList then Nothing else Just 0)
                        refs'
            appState'' <- updateSelectedRefClosureDetails $ appState'
              & majorState
              . mode
              .~ PausedMode
                  { _closurePath = path''
                  , _currentClosureDetails = Just closureDetails
                  , _selectedClosureDetails = error "_selectedClosureDetails should be set by `updateSelectedRefClosureDetails`"
                  , _references = newRefsList
                  }
            continue appState''
      where
      continueWithRoot appState' ixMay = do
          rootClosuresList <- liftIO $ GD.rootClosures debuggee'
          rootRefPrettysList <- closuresToPretty rootClosuresList
          savedClosuresList <- liftIO $ GD.savedClosures debuggee'
          savedRefPrettysList <- closuresToPretty savedClosuresList
          let ix' = fromMaybe 0 ixMay
              refsSeq = Seq.fromList $
                  List.zipWith (\c pretty -> (c, "Saved Object", pretty)) savedClosuresList savedRefPrettysList
                  ++ List.zipWith (\c pretty -> (c, "GC Root", pretty)) rootClosuresList rootRefPrettysList
          continue =<< liftIO (updateSelectedRefClosureDetails (appState' & majorState . mode .~ PausedMode
            { _closurePath = []
            , _currentClosureDetails = Nothing
            , _selectedClosureDetails = error "_selectedClosureDetails should be set by `updateSelectedRefClosureDetails`"
            , _references = listMoveTo ix' $ list
                Connected_Paused_SavedClosuresList
                refsSeq
                1
            }))

      updateSelectedRefClosureDetails appState' = do
        let ixItemMay = listSelectedElement =<< (appState'^?majorState.mode.references)
        cdMay <- case ixItemMay of
                Nothing -> return Nothing
                Just (_, (c, _, _)) -> liftIO $ Just <$> getClosureDetails c
        return $ appState'
            & majorState
            . mode
            . selectedClosureDetails
            .~ cdMay

      getClosureDetails :: Closure -> IO ClosureDetails
      getClosureDetails c = do
        excSize' <- closureExclusiveSize debuggee' c
        sourceLoc <- closureSourceLocation debuggee' c
        return ClosureDetails
          { _sourceLocation = Just (pack $ Prelude.unlines sourceLoc)
          , _closureType = Nothing
          , _constructor = Nothing
          , _excSize = Just excSize'
          }

      closuresToPretty cs = liftIO $ mapM (fmap pack . closurePretty debuggee') cs

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
