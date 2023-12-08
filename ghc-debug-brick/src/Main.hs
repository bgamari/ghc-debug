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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Brick
import Brick.BChan
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.List
import Control.Applicative
import Control.Monad (forever, forM)
import Control.Monad.IO.Class
import Control.Monad.Catch (bracket)
import Control.Concurrent
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.CrossPlatform as Vty
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Foldable as F
import Text.Read (readMaybe)

import GHC.Debug.Types.Ptr(readInfoTablePtr, arrWordsBS)
import qualified GHC.Debug.Types.Closures as Debug
import IOTree
import Lib as GD
import Model
import Data.ByteString.Lazy (ByteString)


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


    Connected socket _debuggee mode' -> case mode' of

      RunningMode -> [mainBorder ("ghc-debug - Running - " <> socketName socket) $ vBox
        [ txtWrap "There is nothing you can do until the process is paused by pressing (p) ..."
        , fill ' '
        , withAttr menuAttr $ vLimit 1 $ hBox [txt "(p): Pause | (ESC): Exit", fill ' ']
        ]]

      (PausedMode os@(OperationalState _ treeMode' kbmode fmode _ _ _ _)) -> let
        in kbOverlay kbmode $ [mainBorder ("ghc-debug - Paused - " <> socketName socket) $ vBox
          [ -- Current closure details
              joinBorders $ borderWithLabel (txt "Closure Details") $
              vLimit 9 $
              pauseModeTree (\r io -> maybe emptyWidget r (ioTreeSelection io)) os
              <=> fill ' '
          , -- Tree
            joinBorders $ borderWithLabel
              (txt $ case treeMode' of
                SavedAndGCRoots {} -> "Root Closures"
                Retainer {} -> "Retainers"
                Searched {} -> "Search Results"
              )
              (pauseModeTree (\_ -> renderIOTree) os)
          , footer (osSize os) (_resultSize os) fmode
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
    [ ("Resume", Just (Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]))
    , ("Parent", Just (Vty.EvKey KLeft []))
    , ("Child", Just (Vty.EvKey KRight []))
    , ("Command Picker", Just (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]))]
    ++ [(commandDescription cmd, commandKey cmd) | cmd <- F.toList commandList ]
    ++ [ ("Exit", Just (Vty.EvKey KEsc [])) ]

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


  renderCommandDesc :: (Text, Maybe Vty.Event) -> Widget Name
  renderCommandDesc (desc, k) = txt (desc <> T.replicate padding " " <> key)
    where
      key = maybe mempty renderKey k
      padding = (actual_width - T.length desc - T.length key)

renderInfoInfo :: InfoInfo -> [Widget Name]
renderInfoInfo info' =
  maybe [] renderSourceInformation (_sourceLocation info')
    ++ [labelled "Era" $ vLimit 1 (str $ show $ _era info')
       ,labelled "type" $ vLimit 1 (str $ show $ _closureType info')]
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


renderClosureDetails :: ClosureDetails -> Widget Name
renderClosureDetails (cd@(ClosureDetails {})) =
  vLimit 8 $
  -- viewport Connected_Paused_ClosureDetails Both $
  vBox $
    renderInfoInfo (_info cd)
    ++
    [ hBox [
      txtLabel $ "Exclusive Size   "
      <> maybe "" (pack . show @Int . GD.getSize) (Just $ _excSize cd) <> " bytes"
      ]
    ]
renderClosureDetails ((LabelNode n)) = txt n
renderClosureDetails ((InfoDetails info')) = vLimit 8 $ vBox $ renderInfoInfo info'

footer :: Int -> Maybe Int -> FooterMode -> Widget Name
footer n m mode = vLimit 1 $
 case mode of
   FooterMessage t -> withAttr menuAttr $ hBox [txt t, fill ' ']
   FooterInfo -> withAttr menuAttr $ hBox $ [padRight Max $ txt "(↑↓): select item | (→): expand | (←): collapse | (^p): command picker | (?): full keybindings"]
                                         ++ [padLeft (Pad 1) $ txt (T.pack (show n) <> " items/" <> maybe "∞" (T.pack . show) m <> " max")]
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
            (rootsTree, initRoots) <- liftIO $ mkSavedAndGCRootsIOTree
            put (appState & majorState . mode .~
                        PausedMode
                          (OperationalState Nothing
                                            savedAndGCRoots
                                            NoOverlay
                                            FooterInfo
                                            (DefaultRoots initRoots)
                                            rootsTree
                                            eventChan
                                            (Just 100)))



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


        mkSavedAndGCRootsIOTree = do
          raw_roots <- take 1000 . map ("GC Roots",) <$> GD.rootClosures debuggee'
          rootClosures' <- liftIO $ mapM (completeClosureDetails debuggee') raw_roots
          raw_saved <- map ("Saved Object",) <$> GD.savedClosures debuggee'
          savedClosures' <- liftIO $ mapM (completeClosureDetails debuggee') raw_saved
          return $ (mkIOTree debuggee' (savedClosures' ++ rootClosures') getChildren renderInlineClosureDesc id
                   , fmap toPtr <$> (raw_roots ++ raw_saved))
          where


getChildren :: Debuggee -> ClosureDetails
            -> IO [ClosureDetails]
getChildren d (ClosureDetails c _ _) = do
  children <- closureReferences d c
  children' <- traverse (traverse (fillListItem d)) children
  mapM (\(lbl, child) -> getClosureDetails d (pack lbl) child) children'
getChildren _ _ = return []

fillListItem :: Debuggee
             -> ListItem CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr
             -> IO (ListItem CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr)
fillListItem _ (ListOnlyInfo x) = return $ ListOnlyInfo x
fillListItem d(ListFullClosure cd) = ListFullClosure <$> fillConstrDesc d cd
fillListItem _ ListData = return ListData

mkIOTree :: Debuggee
         -> [a]
         -> (Debuggee -> a -> IO [a])
         -> (a -> [Widget Name])
-- -> IO [(String, ListItem SrtCont PayloadCont ConstrDesc StackCont ClosurePtr)])
         -> ([a] -> [a])
         -> IOTree a Name
mkIOTree debuggee' cs getChildren renderNode sort = ioTree Connected_Paused_ClosureTree
        (sort cs)
        (\c -> sort <$> getChildren debuggee' c
--            cDets <- mapM (\(lbl, child) -> getClosureDetails debuggee' manalysis (pack lbl) child) children
--            return (sort cDets)
        )
        -- rendering the row
        (\state selected ctx depth closureDesc ->
          let
            -- colorId = _era $ _info closureDesc
            colorEra = {- case colorId of
              _ -> -} id
              -- Nothing -> id
              -- Just i -> modifyDefAttr (flip Vty.withBackColor (era_colors !! (1 + (fromIntegral $ abs i) `mod` (length era_colors - 1))))
            body =
              (if selected then visible . highlighted else id) $
                colorEra $
                hBox $
                renderNode closureDesc
          in
            vdecorate state ctx depth body -- body (T.concat context)
        )

era_colors :: [Vty.Color]
era_colors = [Vty.black, Vty.green, Vty.magenta, Vty.cyan]

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
completeClosureDetails :: Debuggee -> (Text, DebugClosure CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr)
                                            -> IO ClosureDetails

completeClosureDetails dbg (label', clos)  =
  getClosureDetails dbg label' . ListFullClosure  =<< fillConstrDesc dbg clos



getClosureDetails :: Debuggee
                            -> Text
                            -> ListItem CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr
                            -> IO ClosureDetails
getClosureDetails debuggee' t (ListOnlyInfo info_ptr) = do
  info' <- getInfoInfo debuggee' t info_ptr
  return $ InfoDetails info'
getClosureDetails _ t ListData = return $ LabelNode t
getClosureDetails debuggee' label' (ListFullClosure c) = do
  let excSize' = closureExclusiveSize c
  sourceLoc <- maybe (return Nothing) (infoSourceLocation debuggee') (closureInfoPtr c)
  pretty' <- closurePretty debuggee' c
  return ClosureDetails
    { _closure = c
    , _info = InfoInfo {
       _pretty = pack pretty'
      , _labelInParent = label'
      , _sourceLocation = sourceLoc
      , _closureType = Just (T.pack $ show c)
      , _constructor = Nothing
      , _era = case c of
          Closure{_closureSized=c1} -> case Debug.unDCS c1 of
            Debug.ConstrClosure{profHeader} -> Debug.hp <$> profHeader
            Debug.FunClosure{profHeader} -> Debug.hp <$> profHeader
            Debug.ThunkClosure{profHeader} -> Debug.hp <$> profHeader
            _ -> Nothing
          _ -> Nothing
      }
    , _excSize = excSize'
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
      , _era = Nothing
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
            ProgressFinished ->
              put $ os
                    & running_task .~ Nothing
                    & footerMode .~ FooterInfo
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
                    dispatchCommand cmd dbg
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


savedAndGCRoots :: TreeMode
savedAndGCRoots = SavedAndGCRoots renderClosureDetails

-- All the commands which we support, these show up in keybindings and also the command picker
commandList :: Seq.Seq Command
commandList =
  [ mkCommand "Show key bindings" (Vty.EvKey (KChar '?') [])
            (modify $ keybindingsMode .~ KeybindingsShown)
  , mkCommand "Saved/GC Roots" (Vty.EvKey (KChar 's') [Vty.MCtrl])
            (modify $ treeMode .~ savedAndGCRoots)
  , mkCommand "Find Closures (Exact)" (Vty.EvKey (KChar 'c') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FSearch)
  , mkCommand "Find Address" (Vty.EvKey (KChar 'a') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FAddress)
  , mkCommand "Find Info Table" (Vty.EvKey (KChar 'i') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FInfoTable)
  , mkCommand "Write Profile" (Vty.EvKey (KChar 'w') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FProfile)
  , mkCommand "Find Retainers" (Vty.EvKey (KChar 'f') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FRetainer)
  , mkCommand "Find Retainers (Exact)" (Vty.EvKey (KChar 'e') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FRetainerExact)
  , mkCommand "Find Retainers of large ARR_WORDS" (Vty.EvKey (KChar 'g') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FRetainerArrWords)
  , mkCommand "Dump ARR_WORDS payload" (Vty.EvKey (KChar 'd') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FDumpArrWords)
  , mkCommand "Set search limit (default 100)" (Vty.EvKey (KChar 'l') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FSetResultSize)
  , mkCommand "Take Snapshot" (Vty.EvKey (KChar 'x') [Vty.MCtrl])
            (modify $ footerMode .~ footerInput FSnapshot)
  , Command "Find Retainers (Address)" Nothing
            (\_ -> modify $ footerMode .~ footerInput FRetainerAddress)
  , Command "ARR_WORDS Count" Nothing arrWordsAction
   ]


findCommand :: Vty.Event -> Maybe Command
findCommand event = do
  i <- Seq.findIndexL (\cmd -> commandKey cmd == Just event) commandList
  Seq.lookup i commandList

handleMainWindowEvent :: Debuggee
                      -> Handler () OperationalState
handleMainWindowEvent dbg brickEvent = do
      os@(OperationalState _ treeMode' _kbMode _footerMode _curRoots rootsTree _ _) <- get
      case brickEvent of
        VtyEvent (Vty.EvKey (KChar 'p') [Vty.MCtrl]) ->
          put $ os & keybindingsMode .~ commandPickerMode

        -- A generic event
        VtyEvent event | Just cmd <- findCommand event -> dispatchCommand cmd dbg
        -- Navigate the tree of closures
        VtyEvent event -> case treeMode' of
          SavedAndGCRoots {} -> do
            newTree <- handleIOTreeEvent event rootsTree
            put (os & treeSavedAndGCRoots .~ newTree)
          Retainer r t -> do
            newTree <- handleIOTreeEvent event t
            put (os & treeMode .~ Retainer r newTree)

          Searched r t -> do
            newTree <- handleIOTreeEvent event t
            put (os & treeMode .~ Searched r newTree)

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


data ArrWordsLine = CountLine ByteString Int | FieldLine ClosureDetails

renderArrWordsLines :: ArrWordsLine -> [Widget n]
renderArrWordsLines (CountLine k n) = [txtLabel (T.pack (show n)), txtWrap (T.pack (show k))]
renderArrWordsLines (FieldLine cd) = renderInlineClosureDesc cd

arrWordsAction :: Debuggee -> EventM n OperationalState ()
arrWordsAction dbg = do
  os <- get
  -- TODO: Does not honour search limit at all
  asyncAction "Counting ARR_WORDS" os (arrWordsAnalysis Nothing dbg) $ \res -> do
    let sorted_res = take 100 $ Prelude.reverse [(k, S.toList v ) | (k, v) <- (List.sortBy (comparing (S.size . snd)) (M.toList res))]

        top_closure = [CountLine k (length v) | (k, v) <- sorted_res]

        g_children d (CountLine b _) = do
          let Just cs = M.lookup b res
          cs' <- run dbg $ forM (S.toList cs) $ \c -> do
            c' <- GD.dereferenceClosure c
            return $ ListFullClosure $ Closure c c'
          children' <- traverse (traverse (fillListItem d)) $ zipWith (\n c -> (show n, c)) [0..] cs'
          mapM (\(lbl, child) -> FieldLine <$> getClosureDetails d (pack lbl) child) children'
        g_children d (FieldLine c) = map FieldLine <$> getChildren d c

        renderHeaderPane (CountLine b k) = txtWrap (T.pack (show b))
        renderHeaderPane (FieldLine c) = renderClosureDetails c

        tree = mkIOTree dbg top_closure g_children renderArrWordsLines id
    put (os & resetFooter
            & treeMode .~ Searched renderHeaderPane tree
        )


-- | What happens when we press enter in footer input mode
dispatchFooterInput :: Debuggee
                    -> FooterInputMode
                    -> Form Text () Name
                    -> EventM n OperationalState ()
dispatchFooterInput dbg FSearch form = do
   os <- get
   asyncAction "Searching for closures" os (map head <$> (liftIO $ retainersOfConstructor (_resultSize os) Nothing dbg (T.unpack (formState form)))) $ \cps -> do
     let cps' = (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
     res <- liftIO $ mapM (completeClosureDetails dbg) cps'
     let tree = mkIOTree dbg res getChildren renderInlineClosureDesc id
     put (os & resetFooter
             & treeMode .~ Searched renderClosureDetails tree
         )
dispatchFooterInput dbg FAddress form = do
   os <- get
   let address = T.unpack (formState form)
   case readClosurePtr address of
    Just cp -> do
      asyncAction "Finding address" os (map head <$> (liftIO $ retainersOfAddress (_resultSize os) Nothing dbg [cp])) $ \cps -> do
        let cps' = (zipWith (\n cp' -> (T.pack (show n),cp')) [0 :: Int ..]) cps
        res <- liftIO $ mapM (completeClosureDetails dbg) cps'
        let tree = mkIOTree dbg res getChildren renderInlineClosureDesc id
        put (os & resetFooter
                & treeMode .~ Searched renderClosureDetails tree
            )
    Nothing -> put (os & resetFooter)

dispatchFooterInput dbg FInfoTable form = do
   os <- get
   let address = T.unpack (formState form)
   case readInfoTablePtr address of
    Just info_ptr -> do
      mb_src <- liftIO $ infoSourceLocation dbg info_ptr
      asyncAction ("Finding info table " <> T.pack (show info_ptr ++ maybe "" ((" " ++) . show) mb_src)) os (map head <$> (liftIO $ retainersOfInfoTable (_resultSize os) Nothing dbg info_ptr)) $ \cps -> do
        let cps' = (zipWith (\n cp' -> (T.pack (show n),cp')) [0 :: Int ..]) cps
        res <- liftIO $ mapM (completeClosureDetails dbg) cps'
        let tree = mkIOTree dbg res getChildren renderInlineClosureDesc id
        put (os & resetFooter
                & treeMode .~ Searched renderClosureDetails tree
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
   asyncAction "Finding retainers" os (retainersOfConstructor (_resultSize os) (Just roots) dbg (T.unpack (formState form))) $ \cps -> do
      let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
      res <- liftIO $ mapM (mapM (completeClosureDetails dbg)) cps'
      let tree = mkRetainerTree dbg res
      put (os & resetFooter
              & treeMode .~ Retainer renderClosureDetails tree)
dispatchFooterInput dbg FRetainerExact form = do
   os <- get
   asyncAction "Finding exact retainers" os (retainersOfConstructorExact (_resultSize os) dbg (T.unpack (formState form))) $ \cps -> do
    let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
    res <- liftIO $ mapM (mapM (completeClosureDetails dbg)) cps'
    let tree = mkRetainerTree dbg res
    put (os & resetFooter
            & treeMode .~ Retainer renderClosureDetails tree)
dispatchFooterInput dbg FRetainerArrWords form = do
   os <- get
   case readMaybe  $ T.unpack (formState form) of
     Nothing -> pure ()
     Just size -> asyncAction "Finding ArrWords retainers" os (retainersOfArrWords (_resultSize os) dbg size) $ \cps -> do
       let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
       res <- liftIO $ mapM (mapM (completeClosureDetails dbg)) cps'
       let tree = mkRetainerTree dbg res
       put (os & resetFooter
               & treeMode .~ Retainer renderClosureDetails tree)
dispatchFooterInput _ FDumpArrWords form = do
   os <- get
   let act node = asyncAction_ "dumping ARR_WORDS payload" os $
        case node of
          Just ClosureDetails{_closure = Closure{_closureSized = Debug.unDCS -> Debug.ArrWordsClosure{..}}} ->
              BS.writeFile (T.unpack $ formState form) $ arrWordsBS (take (fromIntegral bytes) arrWords)
          _ -> pure ()
   case view treeMode os of
      Retainer _ iotree -> act (ioTreeSelection iotree)
      SavedAndGCRoots _ -> act (ioTreeSelection (view treeSavedAndGCRoots os))
      Searched {} -> put (os & footerMessage "Dump for search mode not implemented yet")


dispatchFooterInput _ FSetResultSize form = do
   os <- get
   asyncAction "setting result size" os (pure ()) $ \() -> case readMaybe $ T.unpack (formState form) of
     Just n
       | n <= 0 -> put (os & resultSize .~ Nothing)
       | otherwise -> put (os & resultSize .~ (Just n))
     Nothing -> pure ()
dispatchFooterInput dbg FRetainerAddress form = do
   os <- get
   let address = T.unpack (formState form)
   case readClosurePtr address of
    Just cp -> do
     asyncAction "Finding address retainers" os (retainersOfAddress (view resultSize os) Nothing dbg [cp]) $ \cps -> do
      let cps' = map (zipWith (\n cp -> (T.pack (show n),cp)) [0 :: Int ..]) cps
      res <- liftIO $ mapM (mapM (completeClosureDetails dbg)) cps'
      let tree = mkRetainerTree dbg res
      put (os & resetFooter
              & treeMode .~ Retainer renderClosureDetails tree)
    Nothing -> put (os & resetFooter)
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
      info_map :: M.Map Ptr [(Text, (DebugClosure CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr))]
      info_map = M.fromList [(toPtr (_closure k), zipWith (\n cp -> ((T.pack (show n)), (_closure cp))) [0 :: Int ..] v) | (k, v) <- stack_map]

      lookup_c dbg' dc'@(ClosureDetails dc _ _) = do
        let ptr = toPtr dc
            results = M.findWithDefault [] ptr info_map
        -- We are also looking up the children of the object we are retaining,
        -- and displaying them prior to the retainer stack
        cs <- getChildren dbg' dc'
        results' <- liftIO $ mapM (\(l, c) -> getClosureDetails dbg' l (ListFullClosure c)) results
        return (cs ++ results')
      lookup_c _ _ = return []

  mkIOTree dbg roots lookup_c renderInlineClosureDesc id

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
