{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables, PartialTypeSignatures #-}
{- |
   Module      : GHC.Vis.View.Graph
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.View.Graph (
  redraw,
  click,
  move,
  updateObjects
  )
  where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Canvas.Utils

import Control.Concurrent
import Control.Monad
import Control.Exception

import Data.Maybe
import Data.IORef
import System.IO.Unsafe

import GHC.Vis.View.Graph.Parser
import GHC.Vis.Types hiding (State, View(..))
import GHC.Vis.View.Common

import GHC.Heap.Graph

import Graphics.XDot.Viewer
import Graphics.XDot.Types hiding (size, w, h)


hoverIconWidth :: Double
hoverIconWidth = 35

hoverIconHeight :: Double
hoverIconHeight = 17.5

hoverIconSpace :: Double
hoverIconSpace = 5

data Icon = EvaluateIcon
          | CollapseIcon
          deriving (Show, Eq)

data State = State
  { boxes      :: [Box]
  , operations :: [(Object Int, Operation)]
  , totalSize  :: Rectangle
  , bounds     :: [(Object Int, Rectangle)]
  , hoverIconBounds :: [(Object Int, [(Icon, Rectangle)])]
  , hover      :: Object Int
  , iconHover  :: Maybe (Object Int, Icon)
  }

state :: IORef State
state = unsafePerformIO $ newIORef $ State [] [] (0, 0, 1, 1) [] [] None Nothing

-- | Draw visualization to screen, called on every update or when it's
--   requested from outside the program.
redraw :: UI.Canvas -> Render ()
redraw canvas = do
  s <- liftIO $ readIORef state
  let (rw2, rh2) = (1000,1000)

  save canvas
  canvas # UI.clearCanvas
  (bbs, hibbs) <- draw canvas s rw2 rh2
  restore canvas
  liftIO $ modifyIORef state (\s' -> s' {bounds = bbs, hoverIconBounds = hibbs})

draw :: UI.Canvas -> State -> Int -> Int -> Render ([(Object Int, Rectangle)], [(Object Int, [(Icon, Rectangle)])])
draw c s rw2 rh2 = do
  if null $ boxes s then return ([], [])
  else do
    vS <- liftIO $ readIORef visState

    -- Line widths don't count to size, let's add a bit
    let rw = 0.97 * fromIntegral rw2
        rh = 0.97 * fromIntegral rh2

        ops = operations s
        bsize@(_,_,sw,sh) = totalSize s

    -- Proportional scaling
        (sx,sy) = (min 1000 $ zoomRatio vS * minimum [2, rw / sw, rh / sh], sx)
        (ox1,oy1) = (0.5 * fromIntegral rw2, 0.5 * fromIntegral rh2)
        (ox2,oy2) = position vS
        (ox,oy) = (ox1 + ox2, oy1 + oy2)

    translate ox oy c
    scale sx sy c

    result <- drawAll c (hover s) bsize ops

{-
    case hover s of
      Node n -> do
        let Just (x,y,w,_h) = lookup (Node n) result

        translate (x + w + hoverIconSpace) y c
        drawHoverMenu c $ iconHover s
      _      -> return True
      -}

    let trafo (o, (x,y,w,h)) = (o,
          ( x * sx + ox -- Transformations to correct scaling and offset
          , y * sy + oy
          , w * sx
          , h * sy
          ))

    let toHoverIconBounds (o, (x,y,w,_h)) = (o, map trafo
          [ (EvaluateIcon, (x+w, y, hoverIconWidth + hoverIconSpace, hoverIconHeight))
          , (CollapseIcon, (x+w, y+hoverIconHeight, hoverIconWidth + hoverIconSpace, hoverIconHeight))
          ])

    return (map trafo result, map toHoverIconBounds result)

-- | Handle a mouse click. If an object was clicked an 'UpdateSignal' is sent
--   that causes the object to be evaluated and the screen to be updated.
click :: IO ()
click = do
  s <- readIORef state

  hm <- inHistoryMode
  when (not hm) $ case iconHover s of
    Nothing -> case hover s of
      -- This might fail when a click occurs during an update
      Node t -> evaluateClick s t
      _ -> return ()

    Just (Node t, EvaluateIcon) -> evaluateClick s t
    Just (Node t, CollapseIcon) -> collapseClick s t
    _ -> return ()
  when hm $ case iconHover s of
    -- Don't evaluate when we're back in time, but allow collapsing
    Just (Node t, CollapseIcon) -> collapseClick s t
    _ -> return ()

evaluateClick :: State -> Int -> IO ()
evaluateClick s t = unless (length (boxes s) <= t) $ do
  evaluate2 $ boxes s !! t
  -- Without forkIO it would hang indefinitely if some action is currently
  -- executed
  void $ forkIO $ putMVar visSignal UpdateSignal

collapseClick :: State -> Int -> IO ()
collapseClick s t = unless (length (boxes s) <= t) $ do
  hide $ boxes s !! t
  void $ forkIO $ putMVar visSignal RedrawSignal

evaluate2 :: Box -> IO ()
evaluate2 b@(Box a) = do
  c <- getBoxedClosureData b
  case c of
    -- ghc: internal error: MUT_ARR_PTRS_FROZEN object entered!
    -- (GHC version 7.4.2 for x86_64_unknown_linux)
    -- Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
    --ArrWordsClosure _ _ _ -> return () -- Don't inspect ArrWords
    --MutArrClosure _ _ _ _ -> return ()
    --MVarClosure _ _ _ _ -> return ()
    --_ -> a `seq` return ()
    IndClosure{} -> a `seq` return ()
    BlackholeClosure{} -> a `seq` return ()
    FunClosure{} -> a `seq` return ()
    ThunkClosure{} -> a `seq` return ()
    APClosure{} -> a `seq` return ()
    PAPClosure{} -> a `seq` return ()
    _ -> return ()
  `catch`
    \(e :: SomeException) -> putStrLn $ "Caught exception while evaluating: " ++ show e

hide :: Box -> IO ()
hide b = modifyMVar_ visHidden (\hs -> return $ b : hs)

-- | Handle a mouse move. Causes an 'UpdateSignal' if the mouse is hovering a
--   different object now, so the object gets highlighted and the screen
--   updated.
move :: UI.Canvas -> UI ()
move canvas = do
  vs <- liftIO $ readIORef visState
  oldS <- liftIO $ readIORef state
  let oldHover = hover oldS

      (mx, my) = mousePos vs

      check (o, (x,y,w,h)) =
        if x <= mx && mx <= x + w &&
           y <= my && my <= y + h
        then o else None

      check2 (o, (x,y,w,h)) =
        if x <= mx && mx <= x + w &&
           y <= my && my <= y + h
        then Just o else Nothing

      validOne (None:xs) = validOne xs
      validOne (x:_) = x
      validOne _ = None

      validOne2 (Nothing:xs) = validOne2 xs
      validOne2 (Just x:_) = Just x
      validOne2 _ = Nothing

  let iconHov = case oldHover of
        Node n -> validOne2 $ map check2 $ fromJust $ lookup (Node n) $ hoverIconBounds oldS
        _      -> Nothing

  case iconHov of
    Just i -> do
      let ih = Just (oldHover, i)
      liftIO $ modifyIORef state $ \s' -> s' {iconHover = ih}
      unless (iconHover oldS == ih) $ redraw canvas

    Nothing -> do
      let h = validOne $ map check $ bounds oldS
      liftIO $ modifyIORef state $ \s' -> s' {hover = h, iconHover = Nothing}
      unless (oldHover == h && iconHover oldS == Nothing) $ redraw canvas

-- | Something might have changed on the heap, update the view.
updateObjects :: [NamedBox] -> IO ()
updateObjects _boxes = do
  hidden <- readMVar visHidden
  (ops, bs', _ , dsize) <- xDotParse $ hidden

  modifyIORef state (\s -> s {operations = ops, boxes = bs', totalSize = dsize, hover = None})
