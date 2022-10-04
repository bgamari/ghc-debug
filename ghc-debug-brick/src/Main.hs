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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Main where
import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Catch (bracket)
import Control.Concurrent
import qualified Data.List as List
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
import qualified Data.Map as M
import Data.Bifunctor
import Data.Maybe

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


drawSetup :: Text -> Text -> GenericList Name Seq.Seq SocketInfo -> Widget Name
drawSetup herald other_herald vals =
      let nKnownDebuggees = Seq.length $ (vals ^. listElementsL)
      in mainBorder "ghc-debug" $ vBox
        [ hBox
          [ txt $ "Select a " <> herald <> " to debug (" <> pack (show nKnownDebuggees) <> " found):"
          , padLeft Max (txt $ "Toggle " <> other_herald <> " view with <TAB>")
          ]
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
            vals
        , txt "Exit with <ESC>"
        ]

mainBorder :: Text -> Widget a -> Widget a
mainBorder title = borderWithLabel (txt title) . padAll 1

myAppDraw :: AppState -> [Widget Name]
myAppDraw (AppState majorState') =
  [ case majorState' of

    Setup setupKind' dbgs snaps ->
      case setupKind' of
        Socket -> drawSetup "process" "snapshots" dbgs
        Snapshot   -> drawSetup "snapshot" "processes" snaps


    Connected _socket _debuggee mode' -> case mode' of

      RunningMode -> mainBorder "ghc-debug - Running" $ vBox
        [ txtWrap "Pause (p)"
        , txtWrap "Exit  (ESC)"
        ]

      (PausedMode os@(OperationalState treeMode' fmode _ro _dtree _ _reverseTree _hg)) -> let
        in mainBorder "ghc-debug - Paused" $ vBox
          [ hBox
            [ border $ vBox
              ([ txt "Resume                  (^r)"
               , txt "Tree                    (^t)"
               , txt "Parent                  (<-)"
               , txt "Child                   (->)"
               , txt "Saved/GC Roots          (^s)"
               , txt "Write Profile           (^w)"
               , txt "Find Retainers          (^f)"
               , txt "Find Retainers (Exact)  (^e)"
               , txt "Find Closures (Exact)   (^c)"
               , txt "Find Address            (^p)"
               , txt "Take Snapshot           (^x)"
               , txt "Exit                    (ESC)"
               ])
            , -- Current closure details
              borderWithLabel (txt "Closure Details") $ pauseModeTree (renderClosureDetails . ioTreeSelection) os
            ]
          , -- Tree
            borderWithLabel
              (txt $ case treeMode' of
                Dominator -> "Dominator Tree"
                SavedAndGCRoots -> "Root Closures"
                Reverse -> "Reverse Edges"
                Retainer {} -> "Retainers"
                Searched {} -> "Search Results"
              )
              (pauseModeTree renderIOTree os)
          , hBorder
          , footer fmode
          ]
  ]
  where

  renderClosureDetails :: Maybe (ClosureDetails pap s c) -> Widget Name
  renderClosureDetails (Just cd@(ClosureDetails {})) =
    vLimit 9 $ vBox $
      renderInfoInfo (_info cd)
      ++
      [ txt $ "Exclusive Size   "
            <> maybe "" (pack . show @Int . GD.getSize) (Just $ _excSize cd) <> " bytes"
      , fill ' '
      ]
  renderClosureDetails Nothing = emptyWidget
  renderClosureDetails (Just (LabelNode n)) = txt n
  renderClosureDetails (Just (InfoDetails info')) = vLimit 9 $ vBox $ renderInfoInfo info'

  renderInfoInfo info' =
      [ txt "SourceLocation   "
            <+> txt (maybe "" renderSourceInformation (_sourceLocation info'))
      -- TODO these aren't actually implemented yet
      -- , txt $ "Type             "
      --       <> fromMaybe "" (_closureType =<< cd)
      -- , txt $ "Constructor      "
      --       <> fromMaybe "" (_constructor =<< cd)
      ]

  renderSourceInformation :: SourceInformation -> T.Text
  renderSourceInformation (SourceInformation name cty ty label' modu loc) =
      T.pack $ unlines [name, show cty, ty, label', modu, loc]

footer :: FooterMode -> Widget Name
footer m = vLimit 1 $
 case m of
   FooterMessage t -> txt t
   FooterInfo -> txt ""
   FooterInput im t -> txt (formatFooterMode im) <+> drawTextCursor t

updateListFrom :: MonadIO m =>
                        IO FilePath
                        -> GenericList n Seq.Seq SocketInfo
                        -> m (GenericList n Seq.Seq SocketInfo)
updateListFrom dirIO llist = liftIO $ do
            dir :: FilePath <- dirIO
            debuggeeSocketFiles :: [FilePath] <- listDirectory dir <|> return []

            -- Sort the sockets by the time they have been created, newest
            -- first.
            debuggeeSockets <- List.sortBy (comparing Ord.Down)
                                  <$> mapM (mkSocketInfo . (dir </>)) debuggeeSocketFiles

            let currentSelectedPathMay :: Maybe SocketInfo
                currentSelectedPathMay = fmap snd (listSelectedElement llist)

                newSelection :: Maybe Int
                newSelection = do
                  currentSelectedPath <- currentSelectedPathMay
                  List.findIndex ((currentSelectedPath ==)) debuggeeSockets

            return $ listReplace
                      (Seq.fromList debuggeeSockets)
                      (newSelection <|> (if Prelude.null debuggeeSockets then Nothing else Just 0))
                      llist


myAppHandleEvent :: BChan Event -> AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
myAppHandleEvent eventChan appState@(AppState majorState') brickEvent = case brickEvent of
  _ -> case majorState' of
    Setup st knownDebuggees' knownSnapshots' -> case brickEvent of

      VtyEvent (Vty.EvKey KEsc _) -> halt appState
      VtyEvent event -> case event of
        -- Connect to the selected debuggee
        Vty.EvKey (KChar '\t') [] -> do
          continue $ appState & majorState . setupKind %~ toggleSetup
        Vty.EvKey KEnter _ ->
          case st of
            Snapshot
              | Just (_debuggeeIx, socket) <- listSelectedElement knownSnapshots'
              -> do
                debuggee' <- liftIO $ snapshotConnect (view socketLocation socket)
                continue $ appState & majorState .~ Connected
                      { _debuggeeSocket = socket
                      , _debuggee = debuggee'
                      , _mode     = RunningMode  -- TODO should we query the debuggee for this?
                  }
            Socket
              | Just (_debuggeeIx, socket) <- listSelectedElement knownDebuggees'
              -> do
                bracket
                  (liftIO $ debuggeeConnect (view socketLocation socket))
                  (\debuggee' -> liftIO $ resume debuggee')
                  (\debuggee' ->
                    continue $ appState & majorState .~ Connected
                      { _debuggeeSocket = socket
                      , _debuggee = debuggee'
                      , _mode     = RunningMode  -- TODO should we query the debuggee for this?
                      })
            _ -> continue appState

        -- Navigate through the list.
        _ -> do
          case st of
            Snapshot -> do
              newOptions <- handleListEventVi handleListEvent event knownSnapshots'
              continue $ appState & majorState . knownSnapshots .~ newOptions
            Socket -> do
              newOptions <- handleListEventVi handleListEvent event knownDebuggees'
              continue $ appState & majorState . knownDebuggees .~ newOptions

      AppEvent event -> case event of
        PollTick -> do
          -- Poll for debuggees
          knownDebuggees'' <- updateListFrom socketDirectory knownDebuggees'
          knownSnapshots'' <- updateListFrom snapshotDirectory knownSnapshots'
          continue $ appState & majorState . knownDebuggees .~ knownDebuggees''
                              & majorState . knownSnapshots .~ knownSnapshots''
        DominatorTreeReady {} ->  continue appState
        ReverseAnalysisReady {} -> continue appState
        HeapGraphReady {} -> continue appState
      _ -> continue appState

    Connected _socket' debuggee' mode' -> case mode' of

      RunningMode -> case brickEvent of
        -- Exit
        VtyEvent (Vty.EvKey KEsc _) ->
          halt appState
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

        -- Resume the debuggee if '^r', exit if ESC
        VtyEvent (Vty.EvKey (KChar 'r') [Vty.MCtrl]) -> do
            liftIO $ resume debuggee'
            continue (appState & majorState . mode .~ RunningMode)
        VtyEvent (Vty.EvKey (KEsc) _) -> do
            liftIO $ resume debuggee'
            continue $ initialAppState

        _ -> liftHandler (majorState . mode) os PausedMode (handleMain debuggee')
              appState (() <$ brickEvent)



      where

      _initialiseViews = forkIO $ do
        !hg <- initialTraversal debuggee'
        writeBChan eventChan (HeapGraphReady hg)
--        _ <- mkDominatorTreeIO hg
--        _ <- mkReversalTreeIO hg
        return ()

      -- This is really slow on big heaps, needs to be made more efficient
      -- or some progress/timeout indicator
      {-
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
            -}


--      mkReversalTreeIO hg = forkIO $ do
--        let !revg = mkReverseGraph hg
--        let revIoTree = mkIOTree Nothing [] (reverseClosureReferences hg revg) id
--        writeBChan eventChan (ReverseAnalysisReady (ReverseAnalysis revIoTree (lookupHeapGraph hg)))


      mkSavedAndGCRootsIOTree manalysis = do
        raw_roots <- take 1000 . map ("GC Roots",) <$> GD.rootClosures debuggee'
        rootClosures' <- liftIO $ mapM (completeClosureDetails debuggee' manalysis) raw_roots
        raw_saved <- map ("Saved Object",) <$> GD.savedClosures debuggee'
        savedClosures' <- liftIO $ mapM (completeClosureDetails debuggee' manalysis) raw_saved
        return $ (mkIOTree debuggee' manalysis (savedClosures' ++ rootClosures') getChildren id
                 , fmap toPtr <$> (raw_roots ++ raw_saved))
        where


getChildren :: Debuggee -> DebugClosure PayloadCont ConstrDesc StackCont ClosurePtr
            -> IO
                 [(String, ListItem PayloadCont ConstrDesc StackCont ClosurePtr)]
getChildren d c = do
  children <- closureReferences d c
  traverse (traverse (fillListItem d)) children

fillListItem :: Debuggee
             -> ListItem PayloadCont ConstrDescCont StackCont ClosurePtr
             -> IO (ListItem PayloadCont ConstrDesc StackCont ClosurePtr)
fillListItem _ (ListOnlyInfo x) = return $ ListOnlyInfo x
fillListItem d(ListFullClosure cd) = ListFullClosure <$> fillConstrDesc d cd
fillListItem _ ListData = return ListData


mkIOTree :: Show c => Debuggee
         -> Maybe Analysis
         -> [ClosureDetails pap s c]
         -> (Debuggee -> DebugClosure pap ConstrDesc s c -> IO [(String, ListItem pap ConstrDesc s c)])
         -> ([ClosureDetails pap s c] -> [ClosureDetails pap s c])
         -> IOTree (ClosureDetails pap s c) Name
mkIOTree debuggee' manalysis cs getChildren sort = ioTree Connected_Paused_ClosureTree
        (sort cs)
        (\c -> do
            case c of
              LabelNode {} -> return []
              InfoDetails {} -> return []
              _ -> do
                children <- getChildren debuggee' (_closure c)
                cDets <- mapM (\(lbl, child) -> getClosureDetails debuggee' manalysis (pack lbl) child) children
                return (sort cDets)
        )
        (\selected depth closureDesc -> hBox
                [ txt (T.replicate depth "  ")
                , (if selected then visible . txt else txt) $
                    (if selected then "* " else "  ")
                    <> renderInlineClosureDesc closureDesc
                ]
        )
        (\depth _closureDesc children -> if List.null children
            then txt $ T.replicate (depth + 2) "  " <> "<Empty>"
            else emptyWidget)

renderInlineClosureDesc :: ClosureDetails pap s c -> Text
renderInlineClosureDesc (LabelNode t) = t
renderInlineClosureDesc (InfoDetails info') =
  _labelInParent info' <> "   " <> _pretty info'
renderInlineClosureDesc closureDesc =
                      _labelInParent (_info closureDesc)
                    <> "   "
                    <> pack (closureShowAddress (_closure closureDesc))
                    <> "   "
                    <> _pretty (_info closureDesc)
completeClosureDetails :: Show c => Debuggee -> Maybe Analysis
                                            -> (Text, DebugClosure pap ConstrDescCont s c)
                                            -> IO (ClosureDetails pap s c)

completeClosureDetails dbg manalysis (label', clos)  =
  getClosureDetails dbg manalysis label' . ListFullClosure  =<< fillConstrDesc dbg clos



getClosureDetails :: Show c => Debuggee
                            -> Maybe Analysis
                            -> Text
                            -> ListItem pap ConstrDesc s c
                            -> IO (ClosureDetails pap s c)
getClosureDetails debuggee' _ t (ListOnlyInfo info_ptr) = do
  info' <- getInfoInfo debuggee' t info_ptr
  return $ InfoDetails info'
getClosureDetails _ _ t ListData = return $ LabelNode t
getClosureDetails debuggee' manalysis label' (ListFullClosure c) = do
  let excSize' = closureExclusiveSize c
      retSize' = closureRetainerSize <$> manalysis <*> pure c
  sourceLoc <- maybe (return Nothing) (infoSourceLocation debuggee') (closureInfoPtr c)
  let pretty' = closurePretty c
  return ClosureDetails
    { _closure = c
    , _info = InfoInfo {
       _pretty = pack pretty'
      , _labelInParent = label'
      , _sourceLocation = sourceLoc
      , _closureType = Nothing
      , _constructor = Nothing
      }
    , _excSize = excSize'
    , _retainerSize = retSize'
    }

getInfoInfo :: Debuggee -> Text -> InfoTablePtr -> IO InfoInfo
getInfoInfo debuggee' label' infoPtr = do

  sourceLoc <- infoSourceLocation debuggee' infoPtr
  let pretty' = case sourceLoc of
                  Just loc -> pack (infoPosition loc)
                  Nothing -> ""
  return $ InfoInfo {
       _pretty = pretty'
      , _labelInParent = label'
      , _sourceLocation = sourceLoc
      , _closureType = Nothing
      , _constructor = Nothing
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
        VtyEvent (Vty.EvKey (KChar 's') [Vty.MCtrl]) -> continue $ os & treeMode .~ SavedAndGCRoots
        VtyEvent (Vty.EvKey (KChar 't') [Vty.MCtrl])
          -- Only switch if the dominator view is ready
          | Just {} <- domTree -> continue $ os & treeMode .~ Dominator
{-        VtyEvent (Vty.EvKey (KFun 3) _)
          -- Only switch if the reverse view is ready
          | Just ra <- reverseA -> do
            -- Get roots from rootTree and use those for the reverse view
            let rs = getIOTreeRoots rootsTree
                convert cd = cd & closure %~ do_one
                do_one cd  = fromJust (view convertPtr ra $ _closurePtr cd)
                rs' = map convert rs
            continue $ os & treeMode .~ Reverse
                          & treeReverse . _Just . reverseIOTree %~ setIOTreeRoots rs'
                          -}
        VtyEvent (Vty.EvKey (KChar 'c') [Vty.MCtrl]) ->
          continue $ os & footerMode .~ (FooterInput FSearch emptyTextCursor)

        VtyEvent (Vty.EvKey (KChar 'p') [Vty.MCtrl]) ->
          continue $ os & footerMode .~ (FooterInput FAddress emptyTextCursor)

        VtyEvent (Vty.EvKey (KChar 'w') [Vty.MCtrl]) ->
          continue $ os & footerMode .~ (FooterInput FProfile emptyTextCursor)

        VtyEvent (Vty.EvKey (KChar 'f') [Vty.MCtrl]) ->
          continue $ os & footerMode .~ (FooterInput FRetainer emptyTextCursor)

        VtyEvent (Vty.EvKey (KChar 'e') [Vty.MCtrl]) ->
          continue $ os & footerMode .~ (FooterInput FRetainerExact emptyTextCursor)

        VtyEvent (Vty.EvKey (KChar 'x') [Vty.MCtrl]) ->
          continue $ os & footerMode .~ (FooterInput FSnapshot emptyTextCursor)

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

          Retainer t -> do
            newTree <- handleIOTreeEvent event t
            continue (os & treeMode .~ Retainer newTree)

          Searched t -> do
            newTree <- handleIOTreeEvent event t
            continue (os & treeMode .~ Searched newTree)

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
   cps <- map head <$> (liftIO $ retainersOfConstructor Nothing dbg (T.unpack (rebuildTextCursor tc)))
   let cps' = (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
   res <- liftIO $ mapM (completeClosureDetails dbg Nothing) cps'
   let tree = mkIOTree dbg Nothing res getChildren id
   continue (os & resetFooter
                & treeMode .~ Searched tree
                )
dispatchFooterInput dbg FAddress tc os = do
   let address = T.unpack (rebuildTextCursor tc)
   case readClosurePtr address of
    Just cp -> do
      cps <- map head <$> (liftIO $ retainersOfAddress Nothing dbg [cp])
      let cps' = (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
      res <- liftIO $ mapM (completeClosureDetails dbg Nothing) cps'
      let tree = mkIOTree dbg Nothing res getChildren id
      continue (os & resetFooter
                   & treeMode .~ Searched tree
               )
    Nothing -> continue (os & resetFooter)

dispatchFooterInput dbg FProfile tc os = do
   liftIO $ profile dbg (T.unpack (rebuildTextCursor tc))
   continue (os & resetFooter)
dispatchFooterInput dbg FRetainer tc os = do
   let roots = mapMaybe go (map snd (currentRoots (view rootsFrom os)))
       go (CP p) = Just p
       go (SP _)   = Nothing
   cps <- liftIO $ retainersOfConstructor (Just roots) dbg (T.unpack (rebuildTextCursor tc))
   let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
   res <- liftIO $ mapM (mapM (completeClosureDetails dbg Nothing)) cps'
   let tree = mkRetainerTree dbg res
   continue (os & resetFooter
                & treeMode .~ Retainer tree)
dispatchFooterInput dbg FRetainerExact tc os = do
   cps <- liftIO $ retainersOfConstructorExact dbg (T.unpack (rebuildTextCursor tc))
   let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
   res <- liftIO $ mapM (mapM (completeClosureDetails dbg Nothing)) cps'
   let tree = mkRetainerTree dbg res
   continue (os & resetFooter
                & treeMode .~ Retainer tree)
dispatchFooterInput dbg FSnapshot tc os = do
   liftIO $ snapshot dbg (T.unpack (rebuildTextCursor tc))
   continue (os & resetFooter)

mkRetainerTree :: Debuggee -> [[ClosureDetails PayloadCont StackCont ClosurePtr]] -> IOTree (ClosureDetails PayloadCont StackCont ClosurePtr) Name
mkRetainerTree dbg stacks = do
  let stack_map = [ (cp, rest) | stack <- stacks, Just (cp, rest) <- [List.uncons stack]]
      roots = map fst stack_map
      info_map = M.fromList [(toPtr (_closure k), zipWith (\n cp -> ((show n), ListFullClosure (_closure cp))) [0 :: Int ..] v) | (k, v) <- stack_map]

      lookup_c _dbg dc = let ptr = toPtr dc
                       in case M.lookup ptr info_map of
                            Nothing -> return []
                            Just ss -> return ss

  mkIOTree dbg Nothing roots lookup_c id

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
