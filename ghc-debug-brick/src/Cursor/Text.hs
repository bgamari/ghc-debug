{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{- Authored by Tom Sydney Kerckhove, copied from cursor package #-}

module Cursor.Text
  ( TextCursor (..),
    emptyTextCursor,
    makeTextCursor,
    makeTextCursorWithSelection,
    rebuildTextCursor,
    textCursorNull,
    textCursorLength,
    textCursorIndex,
    textCursorSelectPrev,
    textCursorSelectNext,
    textCursorSelectIndex,
    textCursorSelectStart,
    textCursorSelectEnd,
    textCursorPrevChar,
    textCursorNextChar,
    textCursorSelectBeginWord,
    textCursorSelectEndWord,
    textCursorSelectNextWord,
    textCursorSelectPrevWord,
    textCursorInsert,
    textCursorAppend,
    textCursorInsertString,
    textCursorAppendString,
    textCursorInsertText,
    textCursorAppendText,
    textCursorRemove,
    textCursorDelete,
    textCursorSplit,
    textCursorCombine,
  )
where

import Control.DeepSeq
import Cursor.List
import Cursor.Types
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro

-- | A cursor for single-line texts
newtype TextCursor = TextCursor
  { textCursorList :: ListCursor Char
  }
  deriving (Show, Eq, Generic)

instance NFData TextCursor

emptyTextCursor :: TextCursor
emptyTextCursor = TextCursor emptyListCursor

makeTextCursor :: Text -> Maybe TextCursor
makeTextCursor t = makeTextCursorWithSelection (T.length t) t

makeTextCursorWithSelection :: Int -> Text -> Maybe TextCursor
makeTextCursorWithSelection i t =
  case T.split (== '\n') t of
    [l] -> TextCursor <$> makeListCursorWithSelection i (T.unpack l)
    _ -> Nothing

rebuildTextCursor :: TextCursor -> Text
rebuildTextCursor = T.pack . rebuildListCursor . textCursorList

textCursorListCursorL ::
  Functor f => (ListCursor Char -> f (ListCursor Char)) -> TextCursor -> f TextCursor
textCursorListCursorL = lens textCursorList (\tc lc -> tc {textCursorList = lc})

textCursorNull :: TextCursor -> Bool
textCursorNull = listCursorNull . textCursorList

textCursorLength :: TextCursor -> Int
textCursorLength = listCursorLength . textCursorList

textCursorIndex :: TextCursor -> Int
textCursorIndex = listCursorIndex . textCursorList

textCursorSelectPrev :: TextCursor -> Maybe TextCursor
textCursorSelectPrev = textCursorListCursorL listCursorSelectPrev

textCursorSelectNext :: TextCursor -> Maybe TextCursor
textCursorSelectNext = textCursorListCursorL listCursorSelectNext

textCursorSelectIndex :: Int -> TextCursor -> TextCursor
textCursorSelectIndex ix_ = textCursorListCursorL %~ listCursorSelectIndex ix_

textCursorSelectStart :: TextCursor -> TextCursor
textCursorSelectStart = textCursorListCursorL %~ listCursorSelectStart

textCursorSelectEnd :: TextCursor -> TextCursor
textCursorSelectEnd = textCursorListCursorL %~ listCursorSelectEnd

textCursorPrevChar :: TextCursor -> Maybe Char
textCursorPrevChar = listCursorPrevItem . textCursorList

textCursorNextChar :: TextCursor -> Maybe Char
textCursorNextChar = listCursorNextItem . textCursorList

-- | Move to the beginning of the word
--
-- * @"hell|o"@ -> @"|hello"@
-- * @"hello   | world"@ -> @"|hello    world"@
-- * @"hello |world"@ -> @"hello |world"@
-- * @"| hello"@ -> @"| hello"@
textCursorSelectBeginWord :: TextCursor -> TextCursor
textCursorSelectBeginWord tc =
  let goLeft = maybe tc textCursorSelectBeginWord (textCursorSelectPrev tc)
   in case textCursorPrevChar tc of
        Nothing -> tc
        Just p
          | isSpace p -> case textCursorNextChar tc of
            Nothing -> goLeft
            Just n
              | isSpace n -> goLeft
              | otherwise -> tc
          | otherwise -> goLeft

-- | Move to the end of the word
--
-- * @"hell|o"@ -> @"hello|"@
-- * @"hello   | world"@ -> @"hello    world|"@
-- * @"hello| world"@ -> @"hello| world"@
-- * @"hello |"@ -> @"hello |"@
textCursorSelectEndWord :: TextCursor -> TextCursor
textCursorSelectEndWord tc =
  let goRight = maybe tc textCursorSelectEndWord (textCursorSelectNext tc)
   in case textCursorNextChar tc of
        Nothing -> tc
        Just p
          | isSpace p -> case textCursorPrevChar tc of
            Nothing -> goRight
            Just n
              | isSpace n -> goRight
              | otherwise -> tc
          | otherwise -> goRight

-- | Move to the beginning of the next word
--
-- * @"|hello"@ -> @"hello|"@
-- * @"hell|o world"@ -> @"hello |world"@
-- * @"hello| world"@ -> @"hello |world"@
-- * @"hello |"@ -> @"hello |"@
textCursorSelectNextWord :: TextCursor -> TextCursor
textCursorSelectNextWord tc =
  case (textCursorPrevChar tc, textCursorNextChar tc) of
    (_, Nothing) -> tc
    (Just p, Just n) ->
      case (isSpace p, isSpace n) of
        (_, True) -> TextCursor $ listCursorNextUntil (not . isSpace) lc
        _ -> textCursorSelectNextWord . TextCursor $ listCursorNextUntil isSpace lc
    _ -> textCursorSelectNextWord $ TextCursor $ listCursorNextUntil isSpace lc
  where
    lc = textCursorList tc

-- | Move to the end of the previous word
--
-- * @"hello|"@ -> @"|hello"@
-- * @"hello w|orld"@ -> @"hello| world"@
-- * @"hello |world"@ -> @"hello| world"@
-- * @" h|ello"@ -> @"| hello"@
textCursorSelectPrevWord :: TextCursor -> TextCursor
textCursorSelectPrevWord tc =
  case (textCursorPrevChar tc, textCursorNextChar tc) of
    (Nothing, _) -> tc
    (Just p, Just n) ->
      case (isSpace p, isSpace n) of
        (True, _) -> TextCursor $ listCursorPrevUntil (not . isSpace) lc
        _ -> textCursorSelectPrevWord . TextCursor $ listCursorPrevUntil isSpace lc
    _ -> textCursorSelectPrevWord . TextCursor $ listCursorPrevUntil isSpace lc
  where
    lc = textCursorList tc

textCursorInsert :: Char -> TextCursor -> Maybe TextCursor
textCursorInsert '\n' _ = Nothing
textCursorInsert c tc =
  if isSafeChar c
    then Just (tc & textCursorListCursorL %~ listCursorInsert c)
    else Nothing

textCursorAppend :: Char -> TextCursor -> Maybe TextCursor
textCursorAppend '\n' _ = Nothing
textCursorAppend c tc =
  if isSafeChar c
    then Just (tc & textCursorListCursorL %~ listCursorAppend c)
    else Nothing

textCursorInsertString :: String -> TextCursor -> Maybe TextCursor
textCursorInsertString s tc =
  if any (\c -> c == '\n' || not (isSafeChar c)) s
    then Nothing
    else Just $ tc & textCursorListCursorL %~ listCursorInsertList s

textCursorAppendString :: String -> TextCursor -> Maybe TextCursor
textCursorAppendString s tc =
  if any (\c -> c == '\n' || not (isSafeChar c)) s
    then Nothing
    else Just $ tc & textCursorListCursorL %~ listCursorAppendList s

textCursorInsertText :: Text -> TextCursor -> Maybe TextCursor
textCursorInsertText = textCursorInsertString . T.unpack

textCursorAppendText :: Text -> TextCursor -> Maybe TextCursor
textCursorAppendText = textCursorAppendString . T.unpack

textCursorRemove :: TextCursor -> Maybe (DeleteOrUpdate TextCursor)
textCursorRemove = focusPossibleDeleteOrUpdate textCursorListCursorL listCursorRemove

textCursorDelete :: TextCursor -> Maybe (DeleteOrUpdate TextCursor)
textCursorDelete = focusPossibleDeleteOrUpdate textCursorListCursorL listCursorDelete

textCursorSplit :: TextCursor -> (TextCursor, TextCursor)
textCursorSplit tc =
  let (lc1, lc2) = listCursorSplit $ textCursorList tc
   in (TextCursor lc1, TextCursor lc2)

textCursorCombine :: TextCursor -> TextCursor -> TextCursor
textCursorCombine (TextCursor lc1) (TextCursor lc2) =
  TextCursor {textCursorList = listCursorCombine lc1 lc2}

