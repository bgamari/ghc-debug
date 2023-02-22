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
import Data.Maybe
import qualified Data.Foldable as F

import IOTree
import Lib as GD
import Model


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
myAppDraw (AppState majorState' _) =
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

      (PausedMode os@(OperationalState _ treeMode' kbmode fmode _ _ _)) -> let
        in kbOverlay kbmode $ [mainBorder "ghc-debug - Paused" $ vBox
          [ -- Current closure details
              joinBorders $ borderWithLabel (txt "Closure Details") $
              vLimit 9 $
              pauseModeTree (renderClosureDetails . ioTreeSelection) os
              <=> fill ' '
          , -- Tree
            joinBorders $ borderWithLabel
              (txt $ case treeMode' of
                SavedAndGCRoots -> "Root Closures"
                Retainer {} -> "Retainers"
                Searched {} -> "Search Results"
              )
              (pauseModeTree renderIOTree os)
          , footer fmode
          ]]

  where

  kbOverlay :: OverlayMode -> [Widget Name] -> [Widget Name]
  kbOverlay KeybindingsShown ws = centerLayer kbWindow : ws
  kbOverlay (CommandPicker inp cmd_list) ws  = centerLayer (cpWindow inp cmd_list) : ws
  kbOverlay NoOverlay ws = ws

  cpWindow :: Form Text () Name -> GenericList Name Seq.Seq Command -> Widget Name
  cpWindow input cmd_list = hLimit (actual_width + 2) $ vLimit (length commandList + 4) $
    withAttr menuAttr $
    borderWithLabel (txt "Command Picker") $ vBox $
      [ renderForm input
      , renderList (\elIsSelected -> if elIsSelected then highlighted . renderCommand else renderCommand) False cmd_list]

  kbWindow :: Widget Name
  kbWindow =
    withAttr menuAttr $
    borderWithLabel (txt "Keybindings") $ vBox $
      map renderCommandDesc all_keys

  all_keys =
    [ ("Resume", Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl])
    , ("Parent", Vty.EvKey KLeft [])
    , ("Child", Vty.EvKey KRight [])
    , ("Command Picker", Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) ]
    ++ [(commandDescription cmd, commandKey cmd) | cmd <- F.toList commandList ]
    ++ [ ("Exit", Vty.EvKey KEsc []) ]

  maximum_size = maximum (map (T.length . fst) all_keys)

  actual_width = maximum_size + 5  -- 5, maximum width of rendering a key
                              + 1  -- 1, at least one padding

  renderKey :: Vty.Event -> Text
  renderKey (Vty.EvKey k [Vty.MCtrl]) = "(^" <> renderNormalKey k <> ")"
  renderKey (Vty.EvKey k [])       = "(" <> renderNormalKey k <> ")"
  renderKey _k = "()"

  renderNormalKey (KChar c) = T.pack [c]
  renderNormalKey KEsc = "ESC"
  renderNormalKey KLeft = "←"
  renderNormalKey KRight = "→"
  renderNormalKey _k = "�"

  renderCommand cmd = renderCommandDesc (commandDescription cmd, commandKey cmd)


  renderCommandDesc :: (Text, Vty.Event) -> Widget Name
  renderCommandDesc (desc, k) = txt (desc <> T.replicate padding " " <> renderKey k)
    where
      key = renderKey k
      padding = (actual_width - T.length desc - T.length key)

  renderClosureDetails :: Maybe ClosureDetails -> Widget Name
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
   FooterMessage t -> withAttr menuAttr $ hBox [txt t, fill ' ']
   FooterInfo -> withAttr menuAttr $ hBox [txt "(↑↓): select item | (→): expand | (←): collapse | (^p): command picker | (?): full keybindings", fill ' ']
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


myAppHandleEvent :: BrickEvent Name Event -> EventM Name AppState ()
myAppHandleEvent brickEvent = do
  appState@(AppState majorState' eventChan) <- get
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
                  debuggee' <- liftIO $ snapshotConnect (writeBChan eventChan . ProgressMessage) (view socketLocation socket)
                  put $ appState & majorState .~ Connected
                        { _debuggeeSocket = socket
                        , _debuggee = debuggee'
                        , _mode     = RunningMode  -- TODO should we query the debuggee for this?
                    }
              Socket
                | Just (_debuggeeIx, socket) <- listSelectedElement knownDebuggees'
                -> do
                  bracket
                    (liftIO $ debuggeeConnect (writeBChan eventChan . ProgressMessage) (view socketLocation socket))
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
          _ -> return ()
        _ -> return ()

      Connected _socket' debuggee' mode' -> case mode' of

        RunningMode -> case brickEvent of
          -- Exit
          VtyEvent (Vty.EvKey KEsc _) ->
            halt
          -- Pause the debuggee
          VtyEvent (Vty.EvKey (KChar 'p') []) -> do
            liftIO $ pause debuggee'
            (rootsTree, initRoots) <- liftIO $ mkSavedAndGCRootsIOTree Nothing
            put (appState & majorState . mode .~
                        PausedMode
                          (OperationalState Nothing
                                            SavedAndGCRoots
                                            NoOverlay
                                            FooterInfo
                                            (DefaultRoots initRoots)
                                            rootsTree
                                            eventChan ))



          _ -> return ()

        PausedMode os -> case brickEvent of
          _ -> case brickEvent of
              -- Resume the debuggee if '^r', exit if ESC
              VtyEvent (Vty.EvKey (KChar 'r') [Vty.MCtrl]) -> do
                  liftIO $ resume debuggee'
                  put (appState & majorState . mode .~ RunningMode)
              VtyEvent (Vty.EvKey (KEsc) _) | NoOverlay <- view keybindingsMode os
                                            , not (isFocusedFooter (view footerMode os)) -> do
                  case view running_task os of
                    Just tid -> do
                      liftIO $ killThread tid
                      put $ appState & majorState . mode . pausedMode . running_task .~ Nothing
                                     & majorState . mode . pausedMode %~ resetFooter
                    Nothing -> do
                      liftIO $ resume debuggee'
                      put $ initialAppState (_appChan appState)

              -- handle any other more local events; mostly key events
              _ -> liftHandler (majorState . mode) os PausedMode (handleMain debuggee')
                     (brickEvent)



        where


        mkSavedAndGCRootsIOTree manalysis = do
          raw_roots <- take 1000 . map ("GC Roots",) <$> GD.rootClosures debuggee'
          rootClosures' <- liftIO $ mapM (completeClosureDetails debuggee' manalysis) raw_roots
          raw_saved <- map ("Saved Object",) <$> GD.savedClosures debuggee'
          savedClosures' <- liftIO $ mapM (completeClosureDetails debuggee' manalysis) raw_saved
          return $ (mkIOTree debuggee' manalysis (savedClosures' ++ rootClosures') getChildren id
                   , fmap toPtr <$> (raw_roots ++ raw_saved))
          where


getChildren :: Debuggee -> DebugClosure SrtCont PayloadCont ConstrDesc StackCont ClosurePtr
            -> IO
                 [(String, ListItem SrtCont PayloadCont ConstrDesc StackCont ClosurePtr)]
getChildren d c = do
  children <- closureReferences d c
  traverse (traverse (fillListItem d)) children

fillListItem :: Debuggee
             -> ListItem SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr
             -> IO (ListItem SrtCont PayloadCont ConstrDesc StackCont ClosurePtr)
fillListItem _ (ListOnlyInfo x) = return $ ListOnlyInfo x
fillListItem d(ListFullClosure cd) = ListFullClosure <$> fillConstrDesc d cd
fillListItem _ ListData = return ListData


mkIOTree :: Debuggee
         -> Maybe Analysis
         -> [ClosureDetails]
         -> (Debuggee -> DebugClosure
                  SrtCont PayloadCont ConstrDesc StackCont ClosurePtr
 -> IO [(String, ListItem SrtCont PayloadCont ConstrDesc StackCont ClosurePtr)])
         -> ([ClosureDetails] -> [ClosureDetails])
         -> IOTree ClosureDetails Name
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

renderInlineClosureDesc :: ClosureDetails -> [Widget n]
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
completeClosureDetails :: Debuggee -> Maybe Analysis
                                            -> (Text, DebugClosure SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr)
                                            -> IO ClosureDetails

completeClosureDetails dbg manalysis (label', clos)  =
  getClosureDetails dbg manalysis label' . ListFullClosure  =<< fillConstrDesc dbg clos



getClosureDetails :: Debuggee
                            -> Maybe Analysis
                            -> Text
                            -> ListItem SrtCont PayloadCont ConstrDesc StackCont ClosurePtr
                            -> IO ClosureDetails
getClosureDetails debuggee' _ t (ListOnlyInfo info_ptr) = do
  info' <- getInfoInfo debuggee' t info_ptr
  return $ InfoDetails info'
getClosureDetails _ _ t ListData = return $ LabelNode t
getClosureDetails debuggee' manalysis label' (ListFullClosure c) = do
  let excSize' = closureExclusiveSize c
      retSize' = closureRetainerSize <$> manalysis <*> pure c
  sourceLoc <- maybe (return Nothing) (infoSourceLocation debuggee') (closureInfoPtr c)
  pretty' <- closurePretty debuggee' c
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

handleMain :: Debuggee -> Handler Event OperationalState
handleMain dbg e = do
  os <- get
  case e of
    AppEvent event -> case event of
            PollTick -> return ()
            ProgressMessage t -> do
              put $ footerMessage t os
            ProgressFinished  ->
              put $ os
                    & running_task .~ Nothing
                    & resetFooter
            AsyncFinished action -> action
    _ | Nothing <- view running_task os ->
      case view keybindingsMode os of
        KeybindingsShown ->
          case e of
            VtyEvent (Vty.EvKey _ _) -> put $ os & keybindingsMode .~ NoOverlay
            _ -> put os
        CommandPicker form cmd_list -> do
          -- Overlapping commands are up/down so handle those just via list, otherwise both
          let handle_form = nestEventM' form (handleFormEvent (() <$ e))
              handle_list =
                case e of
                  VtyEvent vty_e -> nestEventM' cmd_list (handleListEvent vty_e)
                  _ -> return cmd_list
              k form' cmd_list' =
                if (formState form /= formState form') then do
                    let filter_string = formState form'
                        new_elems = Seq.filter (\cmd -> T.toLower filter_string `T.isInfixOf` T.toLower (commandDescription cmd )) commandList
                        cmd_list'' = cmd_list' & listElementsL .~ new_elems
                                           & listSelectedL .~ if Seq.null new_elems then Nothing else Just 0
                    modify $ keybindingsMode .~ (CommandPicker form' cmd_list'')
                  else
                    modify $ keybindingsMode .~ (CommandPicker form' cmd_list')


          case e of
              VtyEvent (Vty.EvKey Vty.KUp _) -> do
                list' <- handle_list
                k form list'
              VtyEvent (Vty.EvKey Vty.KDown _) -> do
                list' <- handle_list
                k form list'
              VtyEvent (Vty.EvKey Vty.KEsc _) ->
                put $ os & keybindingsMode .~ NoOverlay
              VtyEvent (Vty.EvKey Vty.KEnter _) -> do
                case listSelectedElement cmd_list of
                  Just (_, cmd) -> do
                    modify $ keybindingsMode .~ NoOverlay
                    dispatchCommand cmd
                  Nothing  -> return ()
              _ -> do
                form' <- handle_form
                list' <- handle_list
                k form' list'


        NoOverlay -> case view footerMode os of
          FooterInput fm form -> inputFooterHandler dbg fm form (handleMainWindowEvent dbg) (() <$ e)
          _ -> handleMainWindowEvent dbg (() <$ e)
    _ -> return ()

commandPickerMode :: OverlayMode
commandPickerMode =
  CommandPicker
    (newForm [(\w -> forceAttr inputAttr w) @@= editTextField id Overlay (Just 1)] "")
    (list CommandPicker_List commandList 1)


-- All the commands which we support, these show up in keybindings and also the command picker
commandList :: Seq.Seq Command
commandList =
  [ Command "Show key bindings" (Vty.EvKey (KChar '?') [])
            (modify $ keybindingsMode .~ KeybindingsShown)
  , Command "Saved/GC Roots" (Vty.EvKey (KChar 's') [Vty.MCtrl])
            (modify $ treeMode .~ SavedAndGCRoots)
  , Command "Find Closures (Exact)" (Vty.EvKey (KChar 'c') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FSearch)
  , Command "Find Address" (Vty.EvKey (KChar 'a') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FAddress)
  , Command "Write Profile" (Vty.EvKey (KChar 'w') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FProfile)
  , Command "Find Retainers" (Vty.EvKey (KChar 'f') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FRetainer)
  , Command "Find Retainers (Exact)" (Vty.EvKey (KChar 'e') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FRetainerExact)
  , Command "Take Snapshot" (Vty.EvKey (KChar 'x') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FSnapshot) ]


findCommand :: Vty.Event -> Maybe Command
findCommand event = do
  i <- Seq.findIndexL (\cmd -> commandKey cmd == event) commandList
  Seq.lookup i commandList

handleMainWindowEvent :: Debuggee
                      -> Handler () OperationalState
handleMainWindowEvent _dbg brickEvent = do
      os@(OperationalState _ treeMode' _kbMode _footerMode _curRoots rootsTree _) <- get
      case brickEvent of
        VtyEvent (Vty.EvKey (KChar 'p') [Vty.MCtrl]) ->
          put $ os & keybindingsMode .~ commandPickerMode

        -- A generic event
        VtyEvent event | Just cmd <- findCommand event -> dispatchCommand cmd
        -- Navigate the tree of closures
        VtyEvent event -> case treeMode' of
          SavedAndGCRoots -> do
            newTree <- handleIOTreeEvent event rootsTree
            put (os & treeSavedAndGCRoots .~ newTree)
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
                   -> Handler () OperationalState
                   -> Handler () OperationalState
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
   asyncAction "Searching for closures" os (map head <$> (liftIO $ retainersOfConstructor Nothing dbg (T.unpack (formState form)))) $ \cps -> do
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
      asyncAction "Finding address" os (map head <$> (liftIO $ retainersOfAddress Nothing dbg [cp])) $ \cps -> do
        let cps' = (zipWith (\n cp' -> (T.pack (show n),cp')) [0 :: Int ..]) cps
        res <- liftIO $ mapM (completeClosureDetails dbg Nothing) cps'
        let tree = mkIOTree dbg Nothing res getChildren id
        put (os & resetFooter
                & treeMode .~ Searched tree
            )
    Nothing -> put (os & resetFooter)

dispatchFooterInput dbg FProfile form = do
   os <- get
   asyncAction_ "Writing profile" os $ profile dbg (T.unpack (formState form))
dispatchFooterInput dbg FRetainer form = do
   os <- get
   let roots = mapMaybe go (map snd (currentRoots (view rootsFrom os)))
       go (CP p) = Just p
       go (SP _)   = Nothing
   asyncAction "Finding retainers" os (retainersOfConstructor (Just roots) dbg (T.unpack (formState form))) $ \cps -> do
      let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
      res <- liftIO $ mapM (mapM (completeClosureDetails dbg Nothing)) cps'
      let tree = mkRetainerTree dbg res
      put (os & resetFooter
              & treeMode .~ Retainer tree)
dispatchFooterInput dbg FRetainerExact form = do
   os <- get
   asyncAction "Finding exact retainers" os (retainersOfConstructorExact dbg (T.unpack (formState form))) $ \cps -> do
    let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
    res <- liftIO $ mapM (mapM (completeClosureDetails dbg Nothing)) cps'
    let tree = mkRetainerTree dbg res
    put (os & resetFooter
            & treeMode .~ Retainer tree)
dispatchFooterInput dbg FSnapshot form = do
   os <- get
   asyncAction_ "Taking snapshot" os $ snapshot dbg (T.unpack (formState form))

asyncAction_ :: Text -> OperationalState -> IO a -> EventM n OperationalState ()
asyncAction_ desc  os action = asyncAction desc os action (\_ -> return ())

asyncAction :: Text -> OperationalState -> IO a -> (a -> EventM Name OperationalState ()) -> EventM n OperationalState ()
asyncAction desc os action final = do
  tid <- (liftIO $ forkIO $ do
    writeBChan eventChan (ProgressMessage desc)
    res <- action
    writeBChan eventChan (AsyncFinished (final res))
    writeBChan eventChan ProgressFinished)
  put $ os & running_task .~ Just tid
           & resetFooter
  where
    eventChan = view event_chan os



mkRetainerTree :: Debuggee -> [[ClosureDetails]] -> IOTree ClosureDetails Name
mkRetainerTree dbg stacks = do
  let stack_map = [ (cp, rest) | stack <- stacks, Just (cp, rest) <- [List.uncons stack]]
      roots = map fst stack_map
      info_map :: M.Map Ptr [(String, ListItem SrtCont PayloadCont ConstrDesc StackCont ClosurePtr)]
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

footerMessage :: Text -> OperationalState -> OperationalState
footerMessage t l = (set footerMode (FooterMessage t) l)

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
        , appHandleEvent = myAppHandleEvent
        , appStartEvent = myAppStartEvent
        , appAttrMap = myAppAttrMap
        }
  _finalState <- customMain initialVty buildVty
                    (Just eventChan) app (initialAppState eventChan)
  return ()
