{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where
import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Data.List as List
import Data.Maybe (fromMaybe, fromJust)
import Data.Ord (comparing)
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import Data.Text
import Graphics.Vty(defaultConfig, mkVty, defAttr)
import qualified Graphics.Vty.Input.Events as Vty
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import qualified Data.Text as T

import IOTree
import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.List

import GHC.Debug.Client as GD

import Model

data Event
  = PollTick  -- Used to perform arbitrary polling based tasks e.g. looking for new debuggees
  | DominatorTreeReady DominatorAnalysis -- A signal when the dominator tree has been computed

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

      pm@(PausedMode treeMode' dtree _) -> let
        tree' = pauseModeTree pm
        in mainBorder "ghc-debug - Paused" $ vBox
          [ hBox
            [ border $ vBox
              ([ txt "Resume          (r)"
              , txt "Parent          (<-)"
              , txt "Child           (->)"
              , txt "Saved/GC Roots  (F1)"
              ] ++
              [ txt "Dominator Tree  (F2)" | Just {} <- [dtree] ] )
            , -- Current closure details
              borderWithLabel (txt "Closure Details") $ renderClosureDetails (ioTreeSelection tree')
            ]
          , -- Tree
            borderWithLabel
              (txt $ case treeMode' of
                Dominator -> "Dominator Tree"
                SavedAndGCRoots -> "Root Closures"
              )
              (renderIOTree tree')
          ]
  ]
  where
  mainBorder title = borderWithLabel (txt title) . padAll 1

  renderClosureDetails :: Maybe ClosureDetails -> Widget Name
  renderClosureDetails cd = vLimit 9 $ vBox $
      [ txt "SourceLocation   "
            <+> txt (fromMaybe "" (_sourceLocation =<< cd))
      -- TODO these aren't actually implemented yet
      -- , txt $ "Type             "
      --       <> fromMaybe "" (_closureType =<< cd)
      -- , txt $ "Constructor      "
      --       <> fromMaybe "" (_constructor =<< cd)
      , txt $ "Exclusive Size   "
            <> maybe "" (pack . show @Int . GD.getSize) (_excSize <$> cd) <> " bytes"
      , txt $ "Retained Size    "
            <> maybe "" (pack . show @Int . GD.getRetainerSize) (_retainerSize =<< cd) <> " bytes"
      , fill ' '
      ]

myAppHandleEvent :: BChan Event -> AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
myAppHandleEvent eventChan appState@(AppState majorState') brickEvent = case brickEvent of
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
            debuggeeSockets <- List.sortBy (comparing Ord.Down)
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
          domTree <- liftIO $ mkDominatorIOTree
          rootsTree <- mkSavedAndGCRootsIOTree Nothing
          continue (appState & majorState . mode .~ PausedMode SavedAndGCRoots Nothing rootsTree)

        _ -> continue appState

      PausedMode treeMode' domTree rootsTree -> case brickEvent of

        -- Resume the debuggee
        VtyEvent (Vty.EvKey (KChar 'r') _) -> do
          liftIO $ resume debuggee'
          continue (appState & majorState . mode .~ RunningMode)

        -- Change Modes
        VtyEvent (Vty.EvKey (KFun 2) _)
          -- Only switch if the dominator view is ready
          | Just {} <- domTree -> continue $ appState
            & majorState
            . mode
            . treeMode
            .~ Dominator
        VtyEvent (Vty.EvKey (KFun 1) _) -> continue $ appState
            & majorState
            . mode
            . treeMode
            .~ SavedAndGCRoots

        -- Navigate the tree of closures
        VtyEvent event -> case treeMode' of
          Dominator -> do
            newTree <- traverseOf (_Just . getDominatorTree) (handleIOTreeEvent event) domTree

            continue (appState & majorState . mode . treeDominator .~ newTree)
          SavedAndGCRoots -> do
            newTree <- handleIOTreeEvent event rootsTree
            continue (appState & majorState . mode . treeSavedAndGCRoots .~ newTree)

          -- Once the computation is finished, store the result of the
          -- analysis in the state.
        AppEvent (DominatorTreeReady dt) -> do
          -- TODO: This should retain the state of the rootsTree, whilst
          -- adding the new information.
          rootsTree <- mkSavedAndGCRootsIOTree (Just (view getDominatorAnalysis dt))
          continue (appState & majorState . mode .~ PausedMode SavedAndGCRoots (Just dt) rootsTree)

        _ -> continue appState

      where

      mkDominatorIOTree = forkIO $ do
        analysis <- runAnalysis debuggee'
        rootClosures' <- liftIO $ mapM (getClosureDetails (Just analysis) "") =<< GD.dominatorRootClosures debuggee' analysis
        let ioTree = mkIOTree (Just analysis) rootClosures'
                      (\dbg c -> fmap ("",) <$> closureDominatees debuggee' analysis c)
                      (List.sortOn (Ord.Down . _retainerSize))
        writeBChan eventChan (DominatorTreeReady (DominatorAnalysis analysis ioTree))

      getClosureDetails :: Maybe Analysis -> Text -> Closure -> IO ClosureDetails
      getClosureDetails manalysis label c = do
        let excSize' = closureExclusiveSize c
            retSize' = closureRetainerSize <$> manalysis <*> pure c
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
          , _retainerSize = retSize'
          }

      mkSavedAndGCRootsIOTree manalysis = do
        rootClosures' <- liftIO $ mapM (getClosureDetails manalysis "GC Root") =<< GD.rootClosures debuggee'
        savedClosures' <- liftIO $ mapM (getClosureDetails manalysis "Saved Object") =<< GD.savedClosures debuggee'
        return $ mkIOTree manalysis (savedClosures' ++ rootClosures') closureReferences id

      mkIOTree :: Maybe Analysis -> [ClosureDetails] -> (Debuggee -> Closure -> IO [(String, Closure)]) -> ([ClosureDetails] -> [ClosureDetails]) -> IOTree ClosureDetails Name
      mkIOTree manalysis cs getChildren sort = ioTree Connected_Paused_ClosureTree
        (sort cs)
        (\c -> do
            children <- getChildren debuggee' (_closure c)
            cDets <- mapM (\(lbl, child) -> getClosureDetails manalysis (pack lbl) child) children
            return (sort cDets)
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
            then txt $ T.replicate (depth + 2) "  " <> "<Empty>"
            else emptyWidget
        )

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
        , appHandleEvent = (myAppHandleEvent eventChan)
        , appStartEvent = myAppStartEvent
        , appAttrMap = myAppAttrMap
        }
  _finalState <- customMain initialVty buildVty
                    (Just eventChan) app initialAppState
  return ()
