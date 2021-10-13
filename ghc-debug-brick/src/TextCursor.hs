module TextCursor(module Cursor.Text, module TextCursor) where

import Data.Maybe ( fromMaybe )

import Cursor.Text
import Cursor.Types

import Brick hiding (continue, halt)
import qualified Graphics.Vty as V

import Namespace


drawTextCursor :: TextCursor -> Widget Name
drawTextCursor tc =
  showCursor Footer (Location (textCursorIndex tc, 0))
    $ txt (rebuildTextCursor tc)

handleTextCursorEvent :: (TextCursor -> EventM Name (Next k))
                      -> TextCursor
                      -> BrickEvent n e
                      -> EventM Name (Next k)
handleTextCursorEvent k tc e =
    case e of
        VtyEvent ve ->
            case ve of
                V.EvKey key _mods ->
                    let mDo func = k . fromMaybe tc $ func tc
                    in case key of
                           V.KChar c -> mDo $ textCursorInsert c
                           V.KLeft -> mDo textCursorSelectPrev
                           V.KRight -> mDo textCursorSelectNext
                           V.KBS -> mDo (dullMDelete . textCursorRemove)
                           V.KHome -> k $ textCursorSelectStart tc
                           V.KEnd -> k $ textCursorSelectEnd tc
                           V.KDel -> mDo (dullMDelete . textCursorDelete)
                           _ -> k tc
                _ -> k tc
        _ -> k tc
