{-# LANGUAGE CPP, RankNTypes, NoMonomorphismRestriction #-}
{- |
   Module      : GHC.Vis.View.List
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.View.List (
  redraw,
  click,
  move,
  updateObjects
  )
  where

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Canvas.Utils hiding (measureText)
import Graphics.UI.Threepenny.Canvas

import Control.Concurrent
import Control.Monad

import Data.IORef
import System.IO.Unsafe

import GHC.Vis.Types hiding (State, View(..))
import GHC.Vis.View.Common


import Data.List(intercalate)

import Debug.Trace


setSourceRGB :: Double -> Double -> Double -> Canvas -> UI ()
setSourceRGB r g b u =
  u # set' fillStyle (solidColor (RGB (round r) (round g) (round b)))

type Rectangle = (Double, Double, Double, Double)

data State = State
  { objects :: [(Box, String, [VisObject])]
  , bounds  :: [(String, Rectangle)]
  , hover   :: !(Maybe String)
  , totalSize :: Rectangle
  , curPos :: (Double, Double)
  }

type RGB = (Double, Double, Double)

state :: IORef State
state = unsafePerformIO $ newIORef $ State [] [] Nothing (0, 0, 1, 1) (0,0)

colorName :: RGB
colorName = (0.5,1,0.5)

colorNameHighlighted :: RGB
colorNameHighlighted = (0,1,0)

colorLink :: RGB
colorLink = (0.5,0.5,1)

colorLinkHighlighted :: RGB
colorLinkHighlighted = (0.25,0.25,1)

colorThunk :: RGB
colorThunk = (1,0.5,0.5)

colorThunkHighlighted :: RGB
colorThunkHighlighted = (1,0,0)

colorFunction :: RGB
colorFunction = (1,1,0.5)

colorFunctionHighlighted :: RGB
colorFunctionHighlighted = (1,1,0)

padding :: Double
padding = 5

-- | Draw visualization to screen, called on every update or when it's
--   requested from outside the program.
redraw :: Canvas -> UI ()
redraw canvas = do
  s <- liftIO $ readIORef state
  let rw2 = 1000
      rh2 = 1000
  save canvas
  canvas # clearCanvas
  setSourceRGB 255 255 255 canvas
  (size, boundingBoxes) <- draw canvas s rw2 rh2
  liftIO $ modifyIORef state (\s' -> s' {totalSize = size, bounds = boundingBoxes})
  restore canvas


draw :: Canvas -> State -> Int -> Int -> UI (Rectangle, [(String, Rectangle)])
draw canvas s rw2 rh2 = do
  let os = objects s
      objs  = map (\(_,_,x) -> x) os
      --boxes = map (\(x,_,_) -> x) os
      names = map ((++ ": ") . (\(_,x,_) -> x)) os
  --layout <- pangoEmptyLayout
  --liftIO $ writeIORef layout' $ Just layout
  nameWidths <- mapM (width canvas. Unnamed) names
  pos <- mapM (height) objs
  widths <- mapM (mapM (width canvas)) objs
  vS <- liftIO $ readIORef visState

  let rw = 0.98 * fromIntegral rw2
      rh = fromIntegral rh2

      maxNameWidth = maximum nameWidths
      widths2 = 1 : map (\ws -> maxNameWidth + sum ws) widths

      sw = maximum widths2
      sh = sum (map (+ 30) pos) - 15

      (sx,sy) = (zoomRatio vS * min (rw / sw) (rh / sh), sx)
      (ox2,oy2) = position vS
      (ox,oy) = (ox2 * (zoomRatio vS - 1), oy2 * (zoomRatio vS - 1))

  traceShowM (ox, oy, ox2, oy2, zoomRatio vS)
--  let (ox, oy) = (0,0)

  translate ox oy canvas
  unless (rw2 == 0 || rh2 == 0) $
    scale sx sy canvas

  let rpos = scanl (\a b -> a + b + 30) 30 pos
  result <- mapM (drawEntry canvas s maxNameWidth 0) (zip3 objs rpos names)
  return ((0, 0, sw, sh), map (\(o, (x,y,w,h)) -> (o, (x*sx+ox,y*sy+oy,w*sx,h*sy))) $ concat result)

-- | Handle a mouse click. If an object was clicked an 'UpdateSignal' is sent
--   that causes the object to be evaluated and the screen to be updated.
click :: IO ()
click = do
  s <- readIORef state

  hm <- inHistoryMode
  when (not hm) $ case hover s of
     Just t -> do
       evaluate t
       -- Without forkIO it would hang indefinitely if some action is currently
       -- executed
       void $ forkIO $ putMVar visSignal UpdateSignal
     _ -> return ()

-- | Handle a mouse move. Causes an 'UpdateSignal' if the mouse is hovering a
--   different object now, so the object gets highlighted and the screen
--   updated.
move :: Canvas -> UI ()
move c = do
  r <- liftIO $ do
    vS <- readIORef visState
    oldHover <- hover <$> readIORef state
    modifyIORef state $ \s' -> (
      let (mx, my) = mousePos vS
          check (o, (x,y,w,h)) =
            if x <= mx && mx <= x + w &&
               y <= my && my <= y + h
            then Just o else Nothing
      in  s' { hover = msum $ map check (bounds s') }
      )
    newHover <- hover <$> readIORef state
    return (oldHover == newHover)
  unless r (redraw c)


-- | Something might have changed on the heap, update the view.
updateObjects :: [NamedBox] -> IO ()
updateObjects boxes = do
  os <- parseBoxes
  --(h, is) <- multiBuildHeapGraph 100 $ map fst boxes
  -- This is wrong
  --let os = visHeapGraph (zipWith (\(b,i) (b',n) -> (i,n)) is boxes) h
  let objs = zipWith (\(y,x) z -> (x,intercalate ", " y,z)) boxes os
  modifyIORef state (\s -> s {objects = objs, hover = Nothing})



drawEntry :: Canvas -> State -> Double -> Double -> ([VisObject], Double, String) -> UI [(String, Rectangle)]
--drawEntry c s nw x t | traceShow (nw, x, t) False = undefined
drawEntry c s nameWidth xPos (obj, pos, name) = do
  save c
  translate xPos pos c
  moveTo (0, 0) c
  drawBox c s (0,[]) $ Unnamed name
  translate nameWidth 0 c
  moveTo (0, 0) c
  (_, boundingBoxes) <- foldM (drawBox c s) (0, []) obj
  restore c
  return $ map (\(o, (x,y,w,h)) -> (o, (x+nameWidth,y+pos,w,h))) $ boundingBoxes

drawBox :: Canvas -> State -> (Double, [(String, Rectangle)]) -> VisObject
        -> UI (Double, [(String, Rectangle)])
drawBox c _ (x, rs) o@(Unnamed content) = do
  wc <- width c o

  --(layout, metrics) <- pangoLayout content
  let fa = 5 + padding / 2

  --moveTo ((x + padding/2), (-fa)) c
  c # setSourceRGB 0 0 0
  fillText content (x + padding / 2, -fa)  c
  moveTo ((x + wc), 0) c

  return (x + wc, rs)

drawBox c s acc o@(Thunk target) =
  drawFunctionLink acc c s o target colorThunk colorThunkHighlighted

drawBox c s acc o@(Function target) =
  drawFunctionLink acc c s o target colorFunction colorFunctionHighlighted

drawBox c s acc o@(Link target) =
  drawFunctionLink acc c s o target colorLink colorLinkHighlighted

drawBox c s (x, rs) o@(Named box_name box_content) = do

  hc <- height box_content
  wc <- width c o

  let fa = 13 + padding
      hn = 10

  let (ux, uy, uw, uh) =
        ( x
        , -fa
        , wc
        , hc + hn + 10 + padding
        )


  --setLineCap LineCapRound
  c # setColor s box_name colorName colorNameHighlighted
  roundedRect c ux uy uw uh

  fillAndSurround c

  let mid = uy + (uh - 15)

  moveTo (ux, mid) c
  lineTo (ux + uw, mid) c
  stroke c

  save c
  translate 0 (padding/2) c
  (_, bb) <- foldM (drawBox c s) (x + padding , []) box_content
  restore c

  xa <- measureText c box_name
  fillText box_name (x + uw/2 - xa/2 + padding / 2, mid + hn) c
  moveTo ((x + wc), 0) c

  return $ (x + wc,  bb ++ [(box_name, (ux, uy, uw, uh))] ++ rs)



drawFunctionLink :: (Double, [(String, Rectangle)])
                 -> Canvas -> State -> VisObject
                 -> String -> RGB -> RGB -> UI (Double, [(String, Rectangle)])
drawFunctionLink (x, rs) c s o target_text color1 color2 = do
  let fa = 13 + padding
      fh = 10

  wc <- width c o


  let (ux, uy, uw, uh) =
        (  x
        ,  (-fa)
        ,  wc
        ,  fh   +  padding
        )

  --setLineCap LineCapRound
  c # setColor s target_text color1 color2
  roundedRect c ux uy uw uh

  fillAndSurround c

  c # setSourceRGB 0 0 0
  fillText target_text (x + padding , - (5 + padding/2) ) c
--  showLayout layout
  moveTo ((x + wc), 0) c

  return (x + wc, (target_text, (ux, uy, uw, uh)) : rs)



setColor :: State -> String -> RGB -> RGB -> Canvas -> UI ()
setColor s n (r,g,b) (r', g', b')  =
  case hover s of
    Just t -> if t == n then setSourceRGB (r' * 255) (g' * 255) (b' * 255)
                        else setSourceRGB (r * 255) (g * 255) (b * 255)
    Nothing -> setSourceRGB (r * 255) (g * 255) (b * 255)

fillAndSurround :: Canvas -> UI ()
fillAndSurround c = do
  fill c
  c # setSourceRGB 0 0 0
  stroke c

roundedRect :: Canvas -> Double -> Double -> Double -> Double -> UI ()
roundedRect c x y w h = do
  beginPath c
  moveTo       (x,            (y + pad)) c
  lineTo       (x,            (y + h - pad)) c
  arcNegative  ((x + pad),     (y + h - pad)) pad pi      (pi/2) c
  lineTo      ((x + w - pad), (y + h)) c
  arcNegative ((x + w - pad), (y + h - pad)) pad (pi/2)  0 c
  lineTo      ((x + w),       (y + pad)) c
  arcNegative ((x + w - pad), (y + pad))      pad 0       (-pi/2) c
  lineTo      ((x + pad),      y) c
  arcNegative ((x + pad),     (y + pad))     pad (-pi/2) (-pi) c
  closePath c

  --where pad = 1/10 * min w h
  where pad = 5

height :: [VisObject] -> UI Double
height xs = do
  let ya = 10 --Fixed for now
  let go (Named _ ys) = (ya + 15) + maxGo ys
      go (Unnamed _)  = ya
      go (Link _)     = ya + 2 * padding
      go (Thunk _) = ya + 2 * padding
      go (Function _) = ya + 2 * padding

      maxGo = maximum . (0 :) . map go

  return $ maxGo xs

width :: Canvas -> VisObject -> UI Double
width c(Named x ys) = do
  nameWidth <- simpleWidth c x 0
  w2s <- mapM (width c) ys
  return $ max nameWidth (sum w2s) + (2 * padding)
width c(Unnamed x) = simpleWidth c x padding
width c ( Link x) = simpleWidth c x $ 2 * padding
width c (Thunk x) = simpleWidth c x $ 2 * padding
width c (Function x) = simpleWidth c x $ 2 * padding

simpleWidth :: Canvas -> String -> Double -> UI Double
simpleWidth c x pad = do
  (pad +) <$> measureText c x

measureText :: Canvas -> String -> UI Double
measureText _ x = return (fromIntegral (length x * 6))

