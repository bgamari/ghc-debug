module TextCursor(module Cursor.Text, module TextCursor) where

import Data.Maybe ( fromMaybe )

import Cursor.Text
import Cursor.Types

import Brick hiding (halt)
import qualified Graphics.Vty as V

import Namespace


drawTextCursor :: TextCursor -> Widget Name
drawTextCursor tc =
  showCursor Footer (Location (textCursorIndex tc, 0))
    $ txt (rebuildTextCursor tc) <+> fill ' '

handleTextCursorEvent :: (TextCursor -> s -> s)
                      -> TextCursor
                      -> BrickEvent n e
                      -> EventM Name s ()
handleTextCursorEvent k tc e = do
    case e of
        VtyEvent ve ->
            case ve of
                V.EvKey key _mods ->
                    let mDo func = modify (k tc)
                    in case key of
                           V.KChar c -> mDo $ textCursorInsert c
                           V.KLeft -> mDo textCursorSelectPrev
                           V.KRight -> mDo textCursorSelectNext
                           V.KBS -> mDo (dullMDelete . textCursorRemove)
                           V.KHome -> modify (k (textCursorSelectStart tc))
                           V.KEnd -> modify (k (textCursorSelectEnd tc))
                           V.KDel -> mDo (dullMDelete . textCursorDelete)
                           _ -> return ()
                _ -> return ()
        _ -> return ()
