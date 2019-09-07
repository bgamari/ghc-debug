{-# LANGUAGE CPP, RankNTypes, ImpredicativeTypes, OverloadedStrings,
  PartialTypeSignatures, ViewPatterns #-}
{- |
   Module      : GHC.Vis
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

Although ghc-vis is meant to be used in GHCi it can also be used as a library
in regular Haskell programs which are run or compiled by GHC. You can run those
programs using \"runghc example.hs\" or \"ghc -threaded example.hs && ./example\".
Without the \"-threaded\"-Flag ghc-vis does not work correctly. This is an
example using ghc-vis outside of GHCi:

> import GHC.Vis
>
> main = do
>   putStrLn "Start"
>   let a = "teeest"
>   let b = [1..3]
>   let c = b ++ b
>   let d = [1..]
>   putStrLn $ show $ d !! 1
>
>   visualization
>   view a "a"
>   view b "b"
>   view c "c"
>   view d "d"
>
>   getChar
>   switch
>
>   getChar
>   putStrLn "End"
 -}
module GHC.Vis (
  vis,
  mvis,
  view,
  eval,
  switch,
  update,
  clear,
  restore,
  history,
  setDepth,
  derefBox
  )
  where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#else
import Prelude
#endif

--import Graphics.UI.Gtk hiding (Box, Signal)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Concurrent
import Control.Monad
import Data.IORef

import Data.Text ()
import qualified Data.IntMap as M

import System.Timeout
import System.Mem

import GHC.Debug.Types.Graph hiding (name)
import GHC.Debug.Client

import GHC.Vis.Types hiding (view)
import qualified GHC.Vis.Types as T
import GHC.Vis.View.Common
import qualified GHC.Vis.View.List as List

import Web.KeyCode


#ifdef GRAPH_VIEW
import Data.GraphViz.Commands
import qualified GHC.Vis.View.Graph as Graph
#endif

views :: [View]
views =
  View List.redraw List.click List.move List.updateObjects :
#ifdef GRAPH_VIEW
  View Graph.redraw Graph.click Graph.move Graph.updateObjects :
#endif
  []

title :: String
title = "ghc-vis"

zoomIncrement :: Double
zoomIncrement = 2

positionIncrement :: Double
positionIncrement = 50

bigPositionIncrement :: Double
bigPositionIncrement = 200

signalTimeout :: Int
signalTimeout = 1000000


-- | This is the main function. It's to be called from GHCi and launches a
--   graphical window in a new thread.
vis :: Debuggee -> IO ()
vis = mvis

-- | A minimalistic version of ghc-vis, without window decorations, help and
--   all that other stuff.
mvis :: Debuggee -> IO ()
mvis dbg = do
  vr <- swapMVar visRunning True
  unless vr $ void $ forkIO (mVisMainThread dbg)

-- | Add expressions with a name to the visualization window.
view :: Box -> String -> IO ()
view a name = put $ NewSignal a name

-- | Evaluate an object that is shown in the visualization. (Names start with 't')
eval :: String -> IO ()
eval t = evaluate t >> update

-- | Switch between the list view and the graph view
switch :: IO ()
switch = put SwitchSignal

-- | When an object is updated by accessing it, you have to call this to
--   refresh the visualization window. You can also click on an object to force
--   an update.
update :: IO ()
update = put UpdateSignal

-- | Clear the visualization window, removing all expressions from it
clear :: IO ()
clear = put ClearSignal

-- | Reset the hidden boxes
restore :: IO ()
restore = put RestoreSignal

-- | Change position in history
history :: (Int -> Int) -> IO ()
history = put . HistorySignal

-- | Set the maximum depth for following closures on the heap
setDepth :: Int -> IO ()
setDepth newDepth
  | newDepth > 0 = modifyIORef visState (\s -> s {heapDepth = newDepth})
  | otherwise    = error "Heap depth has to be positive"

zoom :: UI.Canvas -> (Double -> Double) -> UI ()
zoom canvas f = do
  liftIO $ do
      state <- readIORef visState

      let newZoomRatio = f $ zoomRatio state
      newPos <- undefined --zoomImage canvas state newZoomRatio (mousePos state)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

  rd canvas

rd :: UI.Canvas -> UI ()
rd canvas = do
  runCorrect redraw >>= \f -> f canvas

movePos :: UI.Canvas -> (T.Point -> T.Point) -> UI ()
movePos canvas f = do
  liftIO $ modifyIORef visState (\s ->
    let newPosition = f $ position s
    in s {position = newPosition})
  rd canvas


put :: Signal -> IO ()
put s = void $ timeout signalTimeout $ putMVar visSignal s

mVisMainThread :: Debuggee -> IO ()
mVisMainThread dbg = do
  startGUI (defaultConfig { jsPort = Just 8080 }) (setup dbg)

canvasSize :: Int
canvasSize = 1000

setup :: Debuggee -> Window -> UI ()
setup dbg window = do
  return window # set UI.title GHC.Vis.title

  canvas <- UI.canvas
              # set UI.height canvasSize
              # set UI.width  canvasSize
              # set style [("border", "solid black 1px"), ("background", "#eee")]
              # set UI.textFont "10px monospace"

  body <- getBody window #+
    [ column [element canvas] ]

  return ()

  setupGUI dbg window body canvas

setupGUI :: Debuggee -> Window -> Element -> UI.Canvas -> UI ()
setupGUI dbg window body canvas = do
  on UI.mousemove canvas $ \(fromIntegral -> x, fromIntegral -> y) -> do
    do
      state <- liftIO $ readIORef visState
      liftIO $ modifyIORef visState (\s -> s {mousePos = (x, y)})
      if dragging state
      then do
        let (oldX, oldY) = mousePos state
            (deltaX, deltaY) = (x - oldX, y - oldY)
            (oldPosX, oldPosY) = position state
        liftIO $ modifyIORef visState (\s -> s {position = (oldPosX + deltaX, oldPosY + deltaY)})
      else
        runCorrect move >>= \f -> f canvas
      return True

  on UI.keydown body $ \button -> (liftIO $ do
    state <- readIORef visState
    let key = keyCodeLookup button
    when (key `elem` [PageUp]) $ do
      let newZoomRatio = zoomRatio state * zoomIncrement
          (oldX, oldY) = position state
          newPos = (oldX*zoomIncrement, oldY*zoomIncrement)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    when (key `elem` [PageDown]) $ do
      let newZoomRatio = zoomRatio state / zoomIncrement
          (oldX, oldY) = position state
          newPos = (oldX/zoomIncrement, oldY/zoomIncrement)
      modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

    when (key `elem` [Digit0]) $
      modifyIORef visState (\s -> s {zoomRatio = 1, position = (0, 0)})

    when (key `elem` [ArrowLeft]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x + positionIncrement
        in s {position = (newX, y)})

    when (key `elem` [ArrowRight]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newX  = x - positionIncrement
        in s {position = (newX, y)})

    when (key `elem` [ArrowUp]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y + positionIncrement
        in s {position = (x, newY)})

    when (key `elem` [ArrowDown]) $
      modifyIORef visState (\s ->
        let (x,y) = position s
            newY  = y - positionIncrement
        in s {position = (x, newY)})) >> rd canvas

  on UI.click canvas $ \_ -> do
    liftIO (join (runCorrect click))
    return True
{-
  on canvas buttonReleaseEvent $ do
    button <- eventButton
    lift $ do
      when (button == RightButton) $
        modifyIORef visState (\s -> s {dragging = False})

      return True
-}

    {-
  on wheel body $ \e -> do
    traceShowM ("scroll", e)
    direction <- eventScrollDirection
    lift $ do
      state <- readIORef visState


      when (direction == ScrollUp) $ do
        let newZoomRatio = zoomRatio state * zoomIncrement
        newPos <- zoomImage canvas state newZoomRatio (mousePos state)
        modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

      when (direction == ScrollDown) $ do
        let newZoomRatio = zoomRatio state / zoomIncrement
        newPos <- zoomImage canvas state newZoomRatio (mousePos state)
        modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

      widgetQueueDraw canvas
      return True
      -}
{-
  on window keyPressEvent $ do
    eKeyName <- eventKeyName
    lift $ do
      state <- readIORef visState


      when (eKeyName `elem` ["plus", "Page_Up", "KP_Add"]) $ do
        let newZoomRatio = zoomRatio state * zoomIncrement
            (oldX, oldY) = position state
            newPos = (oldX*zoomIncrement, oldY*zoomIncrement)
        modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

      when (eKeyName `elem` ["minus", "Page_Down", "KP_Subtract"]) $ do
        let newZoomRatio = zoomRatio state / zoomIncrement
            (oldX, oldY) = position state
            newPos = (oldX/zoomIncrement, oldY/zoomIncrement)
        modifyIORef visState (\s -> s {zoomRatio = newZoomRatio, position = newPos})

      when (eKeyName `elem` ["0", "equal"]) $
        modifyIORef visState (\s -> s {zoomRatio = 1, position = (0, 0)})

      when (eKeyName `elem` ["Left", "h", "a"]) $
        modifyIORef visState (\s ->
          let (x,y) = position s
              newX  = x + positionIncrement
          in s {position = (newX, y)})

      when (eKeyName `elem` ["Right", "l", "d"]) $
        modifyIORef visState (\s ->
          let (x,y) = position s
              newX  = x - positionIncrement
          in s {position = (newX, y)})

      when (eKeyName `elem` ["Up", "k", "w"]) $
        modifyIORef visState (\s ->
          let (x,y) = position s
              newY  = y + positionIncrement
          in s {position = (x, newY)})

      when (eKeyName `elem` ["Down", "j", "s"]) $
        modifyIORef visState (\s ->
          let (x,y) = position s
              newY  = y - positionIncrement
          in s {position = (x, newY)})

      when (eKeyName `elem` ["H", "A"]) $
        modifyIORef visState (\s ->
          let (x,y) = position s
              newX  = x + bigPositionIncrement
          in s {position = (newX, y)})

      when (eKeyName `elem` ["L", "D"]) $
        modifyIORef visState (\s ->
          let (x,y) = position s
              newX  = x - bigPositionIncrement
          in s {position = (newX, y)})

      when (eKeyName `elem` ["K", "W"]) $
        modifyIORef visState (\s ->
          let (x,y) = position s
              newY  = y + bigPositionIncrement
          in s {position = (x, newY)})

      when (eKeyName `elem` ["J", "S"]) $
        modifyIORef visState (\s ->
          let (x,y) = position s
              newY  = y - bigPositionIncrement
          in s {position = (x, newY)})

      when (eKeyName `elem` ["space", "Return", "KP_Enter"]) $
        join $ runCorrect click

      when (eKeyName `elem` ["v"]) $
        put SwitchSignal

      when (eKeyName `elem` ["c"]) $
        put ClearSignal

      when (eKeyName `elem` ["C"]) $
        put RestoreSignal

      when (eKeyName `elem` ["u"]) $
        put UpdateSignal

      when (eKeyName `elem` ["comma", "bracketleft"]) $
        put $ HistorySignal (+1)

      when (eKeyName `elem` ["period", "bracketright"]) $
        put $ HistorySignal (\x -> x - 1)

      widgetQueueDraw canvas
      return True

  widgetShowAll window
  -}

  liftIO $ forkIO $ react dbg window canvas
  return ()

react :: Debuggee -> Window -> UI.Canvas -> IO b
react dbg window canvas = do
  -- Timeout used to handle ghci reloads (:r)
  -- Reloads cause the visSignal to be reinitialized, but takeMVar is still
  -- waiting for the old one.  This solution is not perfect, but it works for
  -- now.
  mbSignal <- timeout signalTimeout (takeMVar visSignal)
  case mbSignal of
    Nothing -> do
      running <- readMVar visRunning
      if running then react dbg window canvas else
        -- :r caused visRunning to be reset
        (do swapMVar visRunning True
            timeout signalTimeout (putMVar visSignal UpdateSignal)
            react dbg window canvas )
    Just signal -> do
      doUpdate <- case signal of
        NewSignal x n  -> do
          modifyMVar_ visBoxes (\y -> return $ if ([n], x) `elem` y then y else y ++ [([n], x)])
          return True
        ClearSignal    -> do
          modifyMVar_ visBoxes $ const $ return []
          modifyMVar_ visHidden $ const $ return []
          modifyMVar_ visHeapHistory $ const $ return (0, [(HeapGraph M.empty, [])])
          return False
        RestoreSignal -> do
          modifyMVar_ visHidden $ const $ return []
          return False
        RedrawSignal   -> return False
        UpdateSignal   -> return True
        SwitchSignal   -> doSwitch >> return False
        HistorySignal f -> do
          modifyMVar_ visHeapHistory (\(i,xs) -> return (max 0 (min (length xs - 1) (f i)), xs))
          return False
        {-
        ExportSignal d f -> do
          catch (runCorrect (exportView :: View -> (forall a. FilePath -> Double -> Double -> (Surface -> IO a) -> IO a) -> String -> IO ()) >>= \e -> e d f)
            (\e -> do let err = show (e :: IOException)
                      hPutStrLn stderr $ "Couldn't export to file \"" ++ f ++ "\": " ++ err
                      return ())
          return False
-}
      boxes <- readMVar visBoxes

      when doUpdate $ do
        performGC -- Else Blackholes appear. Do we want this?
                  -- Blackholes stop our current thread and only resume after
                  -- they have been replaced with their result, thereby leading
                  -- to an additional element in the HeapMap we don't want.
                  -- Example for bad behaviour that would happen then:
                  -- λ> let xs = [1..42] :: [Int]
                  -- λ> let x = 17 :: Int
                  -- λ> let ys = [ y | y <- xs, y >= x ]

        s <- readIORef visState
        x <- multiBuildHeapGraph (derefBox dbg) (heapDepth s) boxes
        modifyMVar_ visHeapHistory (\(i,xs) -> return (i,x:xs))

      runCorrect updateObjects >>= \f -> f boxes
      runUI window (runCorrect redraw >>= \f -> f canvas)

      react dbg window canvas

#ifdef GRAPH_VIEW
  where doSwitch = isGraphvizInstalled >>= \gvi -> if gvi
          then modifyIORef visState (\s -> s {T.view = succN (T.view s), zoomRatio = 1, position = (0, 0)})
          else putStrLn "Cannot switch view: The Graphviz binary (dot) is not installed"

        succN GraphView = ListView
        succN ListView = GraphView
#else
  where doSwitch = putStrLn "Cannot switch view: Graph view disabled at build"
#endif

derefBox :: Debuggee -> DerefFunction
derefBox dbg cp = do
  c <- dereferenceClosure dbg cp
  tritraverse (dereferenceConDesc dbg) pure pure c

runCorrect :: MonadIO m => (View -> f) -> m f
runCorrect f = do
  s <- liftIO $ readIORef visState
  return $ f $ views !! fromEnum (T.view s)

{-
zoomImage :: UI.Canvas -> State -> Double -> T.Point -> IO T.Point
zoomImage _canvas s newZoomRatio _mousePos@(_x', _y') = do
  let (oldPosX, oldPosY) = position s
      newZoom = newZoomRatio / zoomRatio s
      newPos = (oldPosX * newZoom, oldPosY * newZoom)

  return newPos
  -}
