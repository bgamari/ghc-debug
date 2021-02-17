{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where
import Control.Applicative
import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import Graphics.Vty(defaultConfig, mkVty, defAttr)
import qualified Graphics.Vty.Input.Events as Vty
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import Data.Text (Text, pack)
import qualified Data.Text as T

import IOTree
import TextCursor
import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.List

import GHC.Debug.Client.Search as GD
import Lib as GD

import Model

data Event
  = PollTick  -- Used to perform arbitrary polling based tasks e.g. looking for new debuggees
  | DominatorTreeReady DominatorAnalysis -- A signal when the dominator tree has been computed
  | ReverseAnalysisReady ReverseAnalysis
  | HeapGraphReady (HeapGraph GD.Size)

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

      (PausedMode os@(OperationalState treeMode' fmode _ro dtree _ reverseTree hg)) -> let
        in mainBorder "ghc-debug - Paused" $ vBox
          [ hBox
            [ border $ vBox
              ([ txt "Resume          (F12)"
              , txt "Parent          (<-)"
              , txt "Child           (->)"
              , txt "Saved/GC Roots  (F1)"
              ] ++
              [ txt "Dominator Tree  (F2)" | Just {} <- [dtree] ]
                ++
              [ txt "Reverse Analysis (F3)" | Just {} <- [reverseTree] ]
                ++
              [ txt "Search for Constructor (F8)" | Just {} <- [hg] ] )
            , -- Current closure details
              borderWithLabel (txt "Closure Details") $ pauseModeTree (renderClosureDetails . ioTreeSelection) os
            ]
          , -- Tree
            borderWithLabel
              (txt $ case treeMode' of
                Dominator -> "Dominator Tree"
                SavedAndGCRoots -> "Root Closures"
                Reverse -> "Reverse Edges"
              )
              (pauseModeTree renderIOTree os)
          , hBorder
          , footer fmode
          ]
  ]
  where
  mainBorder title = borderWithLabel (txt title) . padAll 1

  renderClosureDetails :: Maybe (ClosureDetails pap s c) -> Widget Name
  renderClosureDetails cd = vLimit 9 $ vBox $
      [ txt "SourceLocation   "
            <+> txt (maybe "" renderSourceInformation (_sourceLocation =<< cd))
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
  renderSourceInformation :: SourceInformation -> T.Text
  renderSourceInformation (SourceInformation name cty ty label modu loc) =
      T.pack $ unlines [name, show cty, ty, label, modu, loc]

footer :: FooterMode -> Widget Name
footer m = vLimit 1 $
 case m of
   FooterMessage t -> txt t
   FooterInfo -> txt ""
   FooterInput im t -> txt (formatFooterMode im) <+> drawTextCursor t


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
            debuggee' <- liftIO $ debuggeeConnect (view socketLocation socket)
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
        DominatorTreeReady {} ->  continue appState
        ReverseAnalysisReady {} -> continue appState
        HeapGraphReady {} -> continue appState
      _ -> continue appState

    Connected _socket' debuggee' mode' -> case mode' of

      RunningMode -> case brickEvent of
        -- Pause the debuggee
        VtyEvent (Vty.EvKey (KChar 'p') []) -> do
          liftIO $ pause debuggee'
--          _ <- liftIO $ initialiseViews
          (rootsTree, initRoots) <- liftIO $ mkSavedAndGCRootsIOTree Nothing
          continue (appState & majorState . mode .~
                      PausedMode
                        (OperationalState SavedAndGCRoots
                                          FooterInfo
                                          (DefaultRoots initRoots)
                                          Nothing
                                          rootsTree
                                          Nothing
                                          Nothing))

        _ -> continue appState

      PausedMode os -> case brickEvent of

          -- Once the computation is finished, store the result of the
          -- analysis in the state.
        AppEvent (DominatorTreeReady dt) -> do
          -- TODO: This should retain the state of the rootsTree, whilst
          -- adding the new information.
          -- rootsTree <- mkSavedAndGCRootsIOTree (Just (view getDominatorAnalysis dt))
          continue (appState & majorState . mode . pausedMode . treeDominator .~ Just dt)

        AppEvent (ReverseAnalysisReady ra) -> do
          continue (appState & majorState . mode . pausedMode . treeReverse .~ Just ra)

        AppEvent (HeapGraphReady hg) -> do
          continue (appState & majorState . mode . pausedMode . heapGraph .~ Just hg)

        -- Resume the debuggee
        VtyEvent (Vty.EvKey (KFun 12) _) -> do
          liftIO $ resume debuggee'
          continue (appState & majorState . mode .~ RunningMode)

        _ -> liftHandler (majorState . mode) os PausedMode (handleMain debuggee')
              appState (() <$ brickEvent)



      where

      initialiseViews = forkIO $ do
        !hg <- initialTraversal debuggee'
        writeBChan eventChan (HeapGraphReady hg)
        _ <- mkDominatorTreeIO hg
        _ <- mkReversalTreeIO hg
        return ()

      -- This is really slow on big heaps, needs to be made more efficient
      -- or some progress/timeout indicator
      mkDominatorTreeIO hg = forkIO $ do
        !analysis <- runAnalysis debuggee' hg
        !rootClosures' <- liftIO $ mapM (getClosureDetails debuggee' (Just analysis) "" <=< fillConstrDesc debuggee') =<< GD.dominatorRootClosures debuggee' analysis
        let domIoTree = mkIOTree (Just analysis) rootClosures'
                      (getChildren analysis)

                      (List.sortOn (Ord.Down . _retainerSize))
        writeBChan eventChan (DominatorTreeReady (DominatorAnalysis analysis domIoTree))
        where
          getChildren analysis _dbg c = do
            cs <- closureDominatees debuggee' analysis c
            fmap (("",)) <$> mapM (fillConstrDesc debuggee') cs


      mkReversalTreeIO hg = forkIO $ do
        let !revg = mkReverseGraph hg
        let revIoTree = mkIOTree Nothing [] (reverseClosureReferences hg revg) id
        writeBChan eventChan (ReverseAnalysisReady (ReverseAnalysis revIoTree (lookupHeapGraph hg)))


      mkSavedAndGCRootsIOTree manalysis = do
        raw_roots <- map ("GC Roots",) <$> GD.rootClosures debuggee'
        rootClosures' <- liftIO $ mapM (completeClosureDetails debuggee' manalysis) raw_roots
        raw_saved <- map ("Saved Object",) <$> GD.savedClosures debuggee'
        savedClosures' <- liftIO $ mapM (completeClosureDetails debuggee' manalysis) raw_saved
        return $ (mkIOTree manalysis (savedClosures' ++ rootClosures') getChildren id
                 , fmap toPtr <$> (raw_roots ++ raw_saved))
        where
          getChildren d c = do
            children <- closureReferences d c
            mapM (mapM (fillConstrDesc d)) children


      mkIOTree :: Show c => Maybe Analysis -> [ClosureDetails pap s c] -> (Debuggee -> DebugClosure pap ConstrDesc s c -> IO [(String, DebugClosure pap ConstrDesc s c)]) -> ([ClosureDetails pap s c] -> [ClosureDetails pap s c]) -> IOTree (ClosureDetails pap s c) Name
      mkIOTree manalysis cs getChildren sort = ioTree Connected_Paused_ClosureTree
        (sort cs)
        (\c -> do
            children <- getChildren debuggee' (_closure c)
            cDets <- mapM (\(lbl, child) -> getClosureDetails debuggee' manalysis (pack lbl) child) children
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
completeClosureDetails :: Show c => Debuggee -> Maybe Analysis
                                            -> (Text, DebugClosure pap ConstrDescCont s c)
                                            -> IO (ClosureDetails pap s c)

completeClosureDetails dbg manalysis (label, clos)  =
  getClosureDetails dbg manalysis label =<< fillConstrDesc dbg clos



getClosureDetails :: Show c => Debuggee
                            -> Maybe Analysis
                            -> Text
                            -> DebugClosure pap ConstrDesc s c
                            -> IO (ClosureDetails pap s c)
getClosureDetails debuggee' manalysis label c = do
  let excSize' = closureExclusiveSize c
      retSize' = closureRetainerSize <$> manalysis <*> pure c
  sourceLoc <- closureSourceLocation debuggee' c
  let pretty' = closurePretty c
  return ClosureDetails
    { _closure = c
    , _pretty = pack pretty'
    , _labelInParent = label
    , _sourceLocation = sourceLoc
    , _closureType = Nothing
    , _constructor = Nothing
    , _excSize = excSize'
    , _retainerSize = retSize'
    }


-- Event handling when the main window has focus

handleMain :: Debuggee -> Handler OperationalState
handleMain dbg os e =
  case view footerMode os of
    FooterInput fm tc ->  inputFooterHandler dbg fm tc (handleMainWindowEvent dbg) os e
    _ -> handleMainWindowEvent dbg os e

handleMainWindowEvent :: Debuggee
                      -> Handler OperationalState
handleMainWindowEvent _dbg os@(OperationalState treeMode'  _footerMode _curRoots domTree rootsTree reverseA _hg)
  brickEvent =
      case brickEvent of

        -- Change Modes
        VtyEvent (Vty.EvKey (KFun 1) _) -> continue $ os & treeMode .~ SavedAndGCRoots
        VtyEvent (Vty.EvKey (KFun 2) _)
          -- Only switch if the dominator view is ready
          | Just {} <- domTree -> continue $ os & treeMode .~ Dominator
        VtyEvent (Vty.EvKey (KFun 3) _)
          -- Only switch if the reverse view is ready
          | Just ra <- reverseA -> do
            -- Get roots from rootTree and use those for the reverse view
            let rs = getIOTreeRoots rootsTree
                convert cd = cd & closure %~ do_one
                do_one cd  = fromJust (view convertPtr ra $ _closurePtr cd)
                rs' = map convert rs
            continue $ os & treeMode .~ Reverse
                          & treeReverse . _Just . reverseIOTree %~ setIOTreeRoots rs'
        VtyEvent (Vty.EvKey (KFun 8) _) ->
          continue $ os & footerMode .~ (FooterInput FSearch emptyTextCursor)

        -- Navigate the tree of closures
        VtyEvent event -> case treeMode' of
          Dominator -> do
            newTree <- traverseOf (_Just . getDominatorTree) (handleIOTreeEvent event) domTree
            continue (os & treeDominator .~ newTree)
          SavedAndGCRoots -> do
            newTree <- handleIOTreeEvent event rootsTree
            continue (os & treeSavedAndGCRoots .~ newTree)
          Reverse -> do
            newTree <- traverseOf (_Just . reverseIOTree) (handleIOTreeEvent event) reverseA
            continue (os & treeReverse .~ newTree)
        _ -> continue os

inputFooterHandler :: Debuggee
                   -> FooterInputMode
                   -> TextCursor
                   -> Handler OperationalState
                   -> Handler OperationalState
inputFooterHandler dbg m tc _k l re@(VtyEvent e) =
  case e of
    Vty.EvKey KEsc [] -> continue (resetFooter l)
    Vty.EvKey KEnter [] -> dispatchFooterInput dbg m tc l
    _ ->
      handleTextCursorEvent
        (\tc' -> continue (set footerMode (FooterInput m tc') l))
        tc re
inputFooterHandler _ _ _ k l re = k l re

-- | What happens when we press enter in footer input mode
dispatchFooterInput :: Debuggee
                    -> FooterInputMode
                    -> TextCursor
                    -> OperationalState
                    -> EventM n (Next OperationalState)
dispatchFooterInput dbg FSearch tc os = do
  case view heapGraph os of
    Just hg -> do
      -- limit to 100 results
      let new_roots = take 100 $ map (\cp -> ("Searched", CP (hgeClosurePtr cp)))
                        (findConstructors (T.unpack (rebuildTextCursor tc)) hg)
      let manalysis = view getDominatorAnalysis <$> view treeDominator os
      raw_roots <- liftIO $ mapM (traverse (dereferencePtr dbg)) new_roots
      root_details <-
        liftIO $ mapM (completeClosureDetails dbg manalysis) raw_roots
      continue (os & resetFooter
                   & rootsFrom .~ SearchedRoots new_roots
                   & treeMode .~ SavedAndGCRoots
                   & treeSavedAndGCRoots %~ setIOTreeRoots root_details)
    -- Should never happen
    Nothing -> continue os

resetFooter :: OperationalState -> OperationalState
resetFooter l = (set footerMode FooterInfo l)

myAppStartEvent :: AppState -> EventM Name AppState
myAppStartEvent = return

myAppAttrMap :: AppState -> AttrMap
myAppAttrMap _appState = attrMap defAttr []

main :: IO ()
main = do
  eventChan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan eventChan PollTick
    -- 2s
    threadDelay 2_000_000
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
