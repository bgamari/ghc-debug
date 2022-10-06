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

import Brick
import Brick.BChan
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.List
import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Catch (bracket)
import Control.Concurrent
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Bifunctor
import Data.Maybe

import GHC.Debug.Client.Search as GD
import IOTree
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
          ]
        , renderList
            (\elIsSelected socketPath -> (if elIsSelected then highlighted else id) $ hBox
                [ txt (socketName socketPath)
                , txt " - "
                , txt (renderSocketTime socketPath)
                ]
            )
            True
            vals
        , vLimit 1 $ withAttr menuAttr $ hBox [txt $ "(ESC): exit | (TAB): toggle " <> other_herald <> " view", fill ' ']
        ]

mainBorder :: Text -> Widget a -> Widget a
mainBorder title w = -- borderWithLabel (txt title) . padAll 1
  vLimit 1 (withAttr menuAttr $ hCenter $ fill ' ' <+> txt title <+> fill ' ') <=> w

myAppDraw :: AppState -> [Widget Name]
myAppDraw (AppState majorState') =
    case majorState' of

    Setup setupKind' dbgs snaps ->
      case setupKind' of
        Socket -> [drawSetup "process" "snapshots" dbgs]
        Snapshot -> [drawSetup "snapshot" "processes" snaps]


    Connected _socket _debuggee mode' -> case mode' of

      RunningMode -> [mainBorder "ghc-debug - Running" $ vBox
        [ txtWrap "There is nothing you can do until the process is paused by pressing (p) ..."
        , fill ' '
        , withAttr menuAttr $ vLimit 1 $ hBox [txt "(p): Pause | (ESC): Exit", fill ' ']
        ]]

      (PausedMode os@(OperationalState treeMode' kbmode fmode _ro _dtree _ _reverseTree _hg)) -> let
        in kbOverlay kbmode $ [mainBorder "ghc-debug - Paused" $ vBox
          [ -- Current closure details
              joinBorders $ borderWithLabel (txt "Closure Details") $
              vLimit 9 $
              pauseModeTree (renderClosureDetails . ioTreeSelection) os
              <=> fill ' '
          , -- Tree
            joinBorders $ borderWithLabel
              (txt $ case treeMode' of
                Dominator -> "Dominator Tree"
                SavedAndGCRoots -> "Root Closures"
                Reverse -> "Reverse Edges"
                Retainer {} -> "Retainers"
                Searched {} -> "Search Results"
              )
              (pauseModeTree renderIOTree os)
          , footer fmode
          ]]

  where

  kbOverlay :: KeybindingsMode -> [Widget Name] -> [Widget Name]
  kbOverlay KeybindingsShown ws = centerLayer kbWindow : ws
  kbOverlay KeybindingsHidden ws = ws

  kbWindow :: Widget Name
  kbWindow =
    withAttr menuAttr $
    borderWithLabel (txt "Keybindings") $ vBox $
      [ txt "Resume                  (^r)"
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
      ]

  renderClosureDetails :: Maybe (ClosureDetails pap s c) -> Widget Name
  renderClosureDetails (Just cd@(ClosureDetails {})) =
    vLimit 9 $
    -- viewport Connected_Paused_ClosureDetails Both $
    vBox $
      renderInfoInfo (_info cd)
      ++
      [ hBox [
        txtLabel $ "Exclusive Size   "
        <> maybe "" (pack . show @Int . GD.getSize) (Just $ _excSize cd) <> " bytes"
        ]
      ]
  renderClosureDetails Nothing = emptyWidget
  renderClosureDetails (Just (LabelNode n)) = txt n
  renderClosureDetails (Just (InfoDetails info')) = vLimit 9 $ vBox $ renderInfoInfo info'

  renderInfoInfo :: InfoInfo -> [Widget Name]
  renderInfoInfo info' =
    maybe [] renderSourceInformation (_sourceLocation info')
      -- TODO these aren't actually implemented yet
      -- , txt $ "Type             "
      --       <> fromMaybe "" (_closureType =<< cd)
      -- , txt $ "Constructor      "
      --       <> fromMaybe "" (_constructor =<< cd)

  renderSourceInformation :: SourceInformation -> [Widget Name]
  renderSourceInformation (SourceInformation name cty ty label' modu loc) =
      [ labelled "Name" $ vLimit 1 (str name)
      , labelled "Closure type" $ vLimit 1 (str (show cty))
      , labelled "Type" $ vLimit 3 (str ty)
      , labelled "Label" $ vLimit 1 (str label')
      , labelled "Module" $ vLimit 1 (str modu)
      , labelled "Location" $ vLimit 1 (str loc)
      ]

  labelled :: Text -> Widget Name -> Widget Name
  labelled lbl w =
    hLimit 17 (txtLabel lbl <+> vLimit 1 (fill ' ')) <+> w <+> vLimit 1 (fill ' ')

footer :: FooterMode -> Widget Name
footer m = vLimit 1 $
 case m of
   FooterMessage t -> txt t
   FooterInfo -> withAttr menuAttr $ hBox [txt "(↑↓): select item | (→): expand | (←): collapse | (?): full keybindings", fill ' ']
   FooterInput _im form -> renderForm form

footerInput :: FooterInputMode -> FooterMode
footerInput im =
  FooterInput im (footerInputForm im)

footerInputForm :: FooterInputMode -> Form Text e Name
footerInputForm im =
  newForm [(\w -> txtLabel (formatFooterMode im) <+> forceAttr inputAttr w) @@= editTextField id Footer (Just 1)] ""

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


myAppHandleEvent :: BChan Event -> BrickEvent Name Event -> EventM Name AppState ()
myAppHandleEvent eventChan brickEvent = do
  appState@(AppState majorState') <- get
  case brickEvent of
    _ -> case majorState' of
      Setup st knownDebuggees' knownSnapshots' -> case brickEvent of

        VtyEvent (Vty.EvKey KEsc _) -> halt
        VtyEvent event -> case event of
          -- Connect to the selected debuggee
          Vty.EvKey (KChar '\t') [] -> do
            put $ appState & majorState . setupKind %~ toggleSetup
          Vty.EvKey KEnter _ ->
            case st of
              Snapshot
                | Just (_debuggeeIx, socket) <- listSelectedElement knownSnapshots'
                -> do
                  debuggee' <- liftIO $ snapshotConnect (view socketLocation socket)
                  put $ appState & majorState .~ Connected
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
                      put $ appState & majorState .~ Connected
                        { _debuggeeSocket = socket
                        , _debuggee = debuggee'
                        , _mode     = RunningMode  -- TODO should we query the debuggee for this?
                        })
              _ -> return ()

          -- Navigate through the list.
          _ -> do
            case st of
              Snapshot -> do
                zoom (majorState . knownSnapshots) (handleListEventVi handleListEvent event)
              Socket -> do
                zoom (majorState . knownDebuggees) (handleListEventVi handleListEvent event)

        AppEvent event -> case event of
          PollTick -> do
            -- Poll for debuggees
            knownDebuggees'' <- updateListFrom socketDirectory knownDebuggees'
            knownSnapshots'' <- updateListFrom snapshotDirectory knownSnapshots'
            put $ appState & majorState . knownDebuggees .~ knownDebuggees''
                                & majorState . knownSnapshots .~ knownSnapshots''
          DominatorTreeReady {} ->  return ()
          ReverseAnalysisReady {} -> return ()
          HeapGraphReady {} -> return ()
        _ -> return ()

      Connected _socket' debuggee' mode' -> case mode' of

        RunningMode -> case brickEvent of
          -- Exit
          VtyEvent (Vty.EvKey KEsc _) ->
            halt
          -- Pause the debuggee
          VtyEvent (Vty.EvKey (KChar 'p') []) -> do
            liftIO $ pause debuggee'
  --          _ <- liftIO $ initialiseViews
            (rootsTree, initRoots) <- liftIO $ mkSavedAndGCRootsIOTree Nothing
            put (appState & majorState . mode .~
                        PausedMode
                          (OperationalState SavedAndGCRoots
                                            KeybindingsHidden
                                            FooterInfo
                                            (DefaultRoots initRoots)
                                            Nothing
                                            rootsTree
                                            Nothing
                                            Nothing))

          _ -> return ()

        PausedMode os -> case brickEvent of

            -- Once the computation is finished, store the result of the
            -- analysis in the state.
          AppEvent (DominatorTreeReady dt) -> do
            -- TODO: This should retain the state of the rootsTree, whilst
            -- adding the new information.
            -- rootsTree <- mkSavedAndGCRootsIOTree (Just (view getDominatorAnalysis dt))
            put (appState & majorState . mode . pausedMode . treeDominator .~ Just dt)

          AppEvent (ReverseAnalysisReady ra) -> do
            put (appState & majorState . mode . pausedMode . treeReverse .~ Just ra)

          AppEvent (HeapGraphReady hg) -> do
            put (appState & majorState . mode . pausedMode . heapGraph .~ Just hg)

          -- Resume the debuggee if '^r', exit if ESC
          VtyEvent (Vty.EvKey (KChar 'r') [Vty.MCtrl]) -> do
              liftIO $ resume debuggee'
              put (appState & majorState . mode .~ RunningMode)
          VtyEvent (Vty.EvKey (KEsc) _) -> do
              liftIO $ resume debuggee'
              put $ initialAppState

          -- handle any other more local events; mostly key events
          _ -> liftHandler (majorState . mode) os PausedMode (handleMain debuggee')
                 (() <$ brickEvent)



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
        -- rendering the row
        (\state selected ctx depth closureDesc ->
          let
            body =
              (if selected then visible . highlighted else id) $
                hBox $
                renderInlineClosureDesc closureDesc
          in
            vdecorate state ctx depth body -- body (T.concat context)
        )

-- | Draw the tree structure around the row item. Inspired by the
-- 'border' functions in brick.
--
vdecorate :: RowState -> RowCtx -> [RowCtx] -> Widget n -> Widget n
vdecorate state ctx depth body =
  Widget Fixed Fixed $ do
    c <- getContext

    let decorationWidth = 2 * length depth + 4

    bodyResult <-
      render $
      hLimit (c ^. availWidthL - decorationWidth) $
      vLimit (c ^. availHeightL) $
      body

    let leftTxt =
          T.concat $
          map
            (\ x -> case x of
              LastRow -> "  "
              NotLastRow -> "│ "
            )
          (List.reverse depth)
        leftPart = withAttr treeAttr (vreplicate leftTxt)
        middleTxt1 =
          case ctx of
            LastRow -> "└─"
            NotLastRow -> "├─"
        middleTxt1' =
          case ctx of
            LastRow -> "  "
            NotLastRow -> "│ "
        middleTxt2 =
          case state of
            Expanded True -> "● " -- "⋅"
            Expanded False -> "┐ "
            Collapsed -> "┄ "
        middleTxt2' =
          case state of
            Expanded True -> "  "
            Expanded False -> "│ "
            Collapsed -> "  "
        middlePart =
          withAttr treeAttr $
            (txt middleTxt1 <=> vreplicate middleTxt1')
            <+> (txt middleTxt2 <=> vreplicate middleTxt2')
        rightPart = Widget Fixed Fixed $ return bodyResult
        total = leftPart <+> middlePart <+> rightPart

    render $
      hLimit (bodyResult ^. imageL . to Vty.imageWidth + decorationWidth) $
      vLimit (bodyResult ^. imageL . to Vty.imageHeight) $
      total

vreplicate :: Text -> Widget n
vreplicate t =
  Widget Fixed Greedy $ do
    c <- getContext
    return $ emptyResult & imageL .~ Vty.vertCat (replicate (c ^. availHeightL) (Vty.text' (c ^. attrL) t))
{-
  hBox
    [ withAttr treeAttr $ Widget Fixed Fixed $ do
        c <- getContext
        limitedResult <- render (hLimit (c ^. availWidthL - T.length t) $ vLimit (c ^. availHeightL) $ body)
        return $ emptyResult & imageL .~ vertCat (replicate (limitedResult ^. imageL . to imageHeight) (text' (c ^. attrL) t))
    , body
    ]
  where
    bodyWidth =
      render (hLimit (c ^. availWidthL - (length depth * 2 + 4)) $ vLimit (c ^. availHeightL) $ body)
-}

renderInlineClosureDesc :: ClosureDetails pap s c -> [Widget n]
renderInlineClosureDesc (LabelNode t) = [txtLabel t]
renderInlineClosureDesc (InfoDetails info') =
  [txtLabel (_labelInParent info'), txt "   ", txt (_pretty info')]
renderInlineClosureDesc closureDesc =
                    [ txtLabel (_labelInParent (_info closureDesc))
                    , txt "   "
                    , txtWrap $
                        pack (closureShowAddress (_closure closureDesc))
                        <> "   "
                        <> _pretty (_info closureDesc)
                    ]
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
handleMain dbg e = do
  os <- get
  case view keybindingsMode os of
    KeybindingsShown ->
      case e of
        VtyEvent (Vty.EvKey _ _) -> put $ os & keybindingsMode .~ KeybindingsHidden
        _ -> put os
    _ -> case view footerMode os of
      FooterInput fm form -> inputFooterHandler dbg fm form (handleMainWindowEvent dbg) e
      _ -> handleMainWindowEvent dbg e

handleMainWindowEvent :: Debuggee
                      -> Handler OperationalState
handleMainWindowEvent _dbg brickEvent = do
      os@(OperationalState treeMode' _kbMode _footerMode _curRoots domTree rootsTree reverseA _hg) <- get
      case brickEvent of
        -- Change Modes
        VtyEvent (Vty.EvKey (KChar '?') []) -> put $ os & keybindingsMode .~ KeybindingsShown
        VtyEvent (Vty.EvKey (KChar 's') [Vty.MCtrl]) -> put $ os & treeMode .~ SavedAndGCRoots
        VtyEvent (Vty.EvKey (KChar 't') [Vty.MCtrl])
          -- Only switch if the dominator view is ready
          | Just {} <- domTree -> put $ os & treeMode .~ Dominator
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
          put $ os & footerMode .~ footerInput FSearch

        VtyEvent (Vty.EvKey (KChar 'p') [Vty.MCtrl]) ->
          put $ os & footerMode .~ footerInput FAddress

        VtyEvent (Vty.EvKey (KChar 'w') [Vty.MCtrl]) ->
          put $ os & footerMode .~ footerInput FProfile

        VtyEvent (Vty.EvKey (KChar 'f') [Vty.MCtrl]) ->
          put $ os & footerMode .~ footerInput FRetainer

        VtyEvent (Vty.EvKey (KChar 'e') [Vty.MCtrl]) ->
          put $ os & footerMode .~ footerInput FRetainerExact

        VtyEvent (Vty.EvKey (KChar 'x') [Vty.MCtrl]) ->
          put $ os & footerMode .~ footerInput FSnapshot

        -- Navigate the tree of closures
        VtyEvent event -> case treeMode' of
          Dominator -> do
            newTree <- traverseOf (_Just . getDominatorTree) (handleIOTreeEvent event) domTree
            put (os & treeDominator .~ newTree)
          SavedAndGCRoots -> do
            newTree <- handleIOTreeEvent event rootsTree
            put (os & treeSavedAndGCRoots .~ newTree)
          Reverse -> do
            newTree <- traverseOf (_Just . reverseIOTree) (handleIOTreeEvent event) reverseA
            put (os & treeReverse .~ newTree)

          Retainer t -> do
            newTree <- handleIOTreeEvent event t
            put (os & treeMode .~ Retainer newTree)

          Searched t -> do
            newTree <- handleIOTreeEvent event t
            put (os & treeMode .~ Searched newTree)

        _ -> return ()

inputFooterHandler :: Debuggee
                   -> FooterInputMode
                   -> Form Text () Name
                   -> Handler OperationalState
                   -> Handler OperationalState
inputFooterHandler dbg m form _k re@(VtyEvent e) =
  case e of
    Vty.EvKey KEsc [] -> modify resetFooter
    Vty.EvKey KEnter [] -> dispatchFooterInput dbg m form
    _ -> do
      zoom (lens (const form) (\ os form' -> set footerMode (FooterInput m form') os)) (handleFormEvent re)
inputFooterHandler _ _ _ k re = k re

-- | What happens when we press enter in footer input mode
dispatchFooterInput :: Debuggee
                    -> FooterInputMode
                    -> Form Text () Name
                    -> EventM n OperationalState ()
dispatchFooterInput dbg FSearch form = do
   os <- get
   cps <- map head <$> (liftIO $ retainersOfConstructor Nothing dbg (T.unpack (formState form)))
   let cps' = (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
   res <- liftIO $ mapM (completeClosureDetails dbg Nothing) cps'
   let tree = mkIOTree dbg Nothing res getChildren id
   put (os & resetFooter
           & treeMode .~ Searched tree
       )
dispatchFooterInput dbg FAddress form = do
   os <- get
   let address = T.unpack (formState form)
   case readClosurePtr address of
    Just cp -> do
      cps <- map head <$> (liftIO $ retainersOfAddress Nothing dbg [cp])
      let cps' = (zipWith (\n cp' -> (T.pack (show n),cp')) [0 :: Int ..]) cps
      res <- liftIO $ mapM (completeClosureDetails dbg Nothing) cps'
      let tree = mkIOTree dbg Nothing res getChildren id
      put (os & resetFooter
                   & treeMode .~ Searched tree
               )
    Nothing -> put (os & resetFooter)

dispatchFooterInput dbg FProfile form = do
   os <- get
   liftIO $ profile dbg (T.unpack (formState form))
   put (os & resetFooter)
dispatchFooterInput dbg FRetainer form = do
   os <- get
   let roots = mapMaybe go (map snd (currentRoots (view rootsFrom os)))
       go (CP p) = Just p
       go (SP _)   = Nothing
   cps <- liftIO $ retainersOfConstructor (Just roots) dbg (T.unpack (formState form))
   let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
   res <- liftIO $ mapM (mapM (completeClosureDetails dbg Nothing)) cps'
   let tree = mkRetainerTree dbg res
   put (os & resetFooter
                & treeMode .~ Retainer tree)
dispatchFooterInput dbg FRetainerExact form = do
   os <- get
   cps <- liftIO $ retainersOfConstructorExact dbg (T.unpack (formState form))
   let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
   res <- liftIO $ mapM (mapM (completeClosureDetails dbg Nothing)) cps'
   let tree = mkRetainerTree dbg res
   put (os & resetFooter
                & treeMode .~ Retainer tree)
dispatchFooterInput dbg FSnapshot form = do
   os <- get
   liftIO $ snapshot dbg (T.unpack (formState form))
   put (os & resetFooter)

mkRetainerTree :: Debuggee -> [[ClosureDetails PayloadCont StackCont ClosurePtr]] -> IOTree (ClosureDetails PayloadCont StackCont ClosurePtr) Name
mkRetainerTree dbg stacks = do
  let stack_map = [ (cp, rest) | stack <- stacks, Just (cp, rest) <- [List.uncons stack]]
      roots = map fst stack_map
      info_map :: M.Map Ptr [(String, ListItem PayloadCont ConstrDesc StackCont ClosurePtr)]
      info_map = M.fromList [(toPtr (_closure k), zipWith (\n cp -> ((show n), ListFullClosure (_closure cp))) [0 :: Int ..] v) | (k, v) <- stack_map]

      lookup_c dbg' dc = do
        let ptr = toPtr dc
            results = M.findWithDefault [] ptr info_map
        -- We are also looking up the children of the object we are retaining,
        -- and displaying them prior to the retainer stack
        cs <- getChildren dbg' dc
        return (cs ++ results)

  mkIOTree dbg Nothing roots lookup_c id

resetFooter :: OperationalState -> OperationalState
resetFooter l = (set footerMode FooterInfo l)

myAppStartEvent :: EventM Name AppState ()
myAppStartEvent = return ()

myAppAttrMap :: AppState -> AttrMap
myAppAttrMap _appState =
  attrMap (Vty.withStyle (Vty.white `on` Vty.black) Vty.dim)
    [ (menuAttr, Vty.withStyle (Vty.white `on` Vty.blue) Vty.bold)
    , (inputAttr, Vty.black `on` Vty.green)
    , (labelAttr, Vty.withStyle (fg Vty.white) Vty.bold)
    , (highlightAttr, Vty.black `on` Vty.yellow)
    , (treeAttr, fg Vty.red)
    ]

menuAttr :: AttrName
menuAttr = attrName "menu"

inputAttr :: AttrName
inputAttr = attrName "input"

labelAttr :: AttrName
labelAttr = attrName "label"

treeAttr :: AttrName
treeAttr = attrName "tree"

highlightAttr :: AttrName
highlightAttr = attrName "highlighted"

txtLabel :: Text -> Widget n
txtLabel = withAttr labelAttr . txt

highlighted :: Widget n -> Widget n
highlighted = forceAttr highlightAttr

main :: IO ()
main = do
  eventChan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan eventChan PollTick
    -- 2s
    threadDelay 2_000_000
  let buildVty = Vty.mkVty Vty.defaultConfig
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
