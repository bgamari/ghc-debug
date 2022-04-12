{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{- Authored by Tom Sydney Kerckhove, copied from cursor package #-}

module Cursor.List
  ( ListCursor (..),
    emptyListCursor,
    makeListCursor,
    makeListCursorWithSelection,
    rebuildListCursor,
    listCursorNull,
    listCursorLength,
    listCursorIndex,
    listCursorSelectPrev,
    listCursorSelectNext,
    listCursorSelectIndex,
    listCursorSelectStart,
    listCursorSelectEnd,
    listCursorPrevItem,
    listCursorNextItem,
    listCursorPrevUntil,
    listCursorNextUntil,
    listCursorInsert,
    listCursorAppend,
    listCursorInsertList,
    listCursorAppendList,
    listCursorRemove,
    listCursorDelete,
    listCursorSplit,
    listCursorCombine,
    traverseListCursor,
    foldListCursor,
  )
where

import Control.DeepSeq
import Cursor.Types
import GHC.Generics (Generic)

data ListCursor a = ListCursor
  { -- | In reverse order
    listCursorPrev :: [a],
    listCursorNext :: [a]
  }
  deriving (Show, Eq, Generic, Functor)

instance NFData a => NFData (ListCursor a)

emptyListCursor :: ListCursor a
emptyListCursor = ListCursor {listCursorPrev = [], listCursorNext = []}

makeListCursor :: [a] -> ListCursor a
makeListCursor as = ListCursor {listCursorPrev = [], listCursorNext = as}

makeListCursorWithSelection :: Int -> [a] -> Maybe (ListCursor a)
makeListCursorWithSelection i as
  | i < 0 = Nothing
  | i > length as = Nothing
  | otherwise = Just ListCursor {listCursorPrev = reverse $ take i as, listCursorNext = drop i as}

rebuildListCursor :: ListCursor a -> [a]
rebuildListCursor ListCursor {..} = reverse listCursorPrev ++ listCursorNext

listCursorNull :: ListCursor a -> Bool
listCursorNull ListCursor {..} = null listCursorPrev && null listCursorNext

listCursorLength :: ListCursor a -> Int
listCursorLength = length . rebuildListCursor

listCursorIndex :: ListCursor a -> Int
listCursorIndex = length . listCursorPrev

listCursorSelectPrev :: ListCursor a -> Maybe (ListCursor a)
listCursorSelectPrev tc =
  case listCursorPrev tc of
    [] -> Nothing
    (c : cs) -> Just ListCursor {listCursorPrev = cs, listCursorNext = c : listCursorNext tc}

listCursorSelectNext :: ListCursor a -> Maybe (ListCursor a)
listCursorSelectNext tc =
  case listCursorNext tc of
    [] -> Nothing
    (c : cs) -> Just ListCursor {listCursorPrev = c : listCursorPrev tc, listCursorNext = cs}

listCursorSelectIndex :: Int -> ListCursor a -> ListCursor a
listCursorSelectIndex ix_ lc =
  let ls = rebuildListCursor lc
   in case splitAt ix_ ls of
        (l, r) -> ListCursor {listCursorPrev = reverse l, listCursorNext = r}

listCursorSelectStart :: ListCursor a -> ListCursor a
listCursorSelectStart tc =
  case listCursorSelectPrev tc of
    Nothing -> tc
    Just tc' -> listCursorSelectStart tc'

listCursorSelectEnd :: ListCursor a -> ListCursor a
listCursorSelectEnd tc =
  case listCursorSelectNext tc of
    Nothing -> tc
    Just tc' -> listCursorSelectEnd tc'

listCursorPrevItem :: ListCursor a -> Maybe a
listCursorPrevItem lc =
  case listCursorPrev lc of
    [] -> Nothing
    (c : _) -> Just c

listCursorNextItem :: ListCursor a -> Maybe a
listCursorNextItem lc =
  case listCursorNext lc of
    [] -> Nothing
    (c : _) -> Just c

listCursorPrevUntil :: (a -> Bool) -> ListCursor a -> ListCursor a
listCursorPrevUntil p = go
  where
    go lc =
      case listCursorPrev lc of
        [] -> lc
        (c : _)
          | p c -> lc
        _ -> maybe lc go (listCursorSelectPrev lc)

listCursorNextUntil :: (a -> Bool) -> ListCursor a -> ListCursor a
listCursorNextUntil p = go
  where
    go lc =
      case listCursorNext lc of
        [] -> lc
        (c : _)
          | p c -> lc
        _ -> maybe lc go (listCursorSelectNext lc)

listCursorInsert :: a -> ListCursor a -> ListCursor a
listCursorInsert c lc = lc {listCursorPrev = c : listCursorPrev lc}

listCursorAppend :: a -> ListCursor a -> ListCursor a
listCursorAppend c lc = lc {listCursorNext = c : listCursorNext lc}

listCursorInsertList :: [a] -> ListCursor a -> ListCursor a
listCursorInsertList l lc = lc {listCursorPrev = reverse l ++ listCursorPrev lc}

listCursorAppendList :: [a] -> ListCursor a -> ListCursor a
listCursorAppendList l lc = lc {listCursorNext = l ++ listCursorNext lc}

listCursorRemove :: ListCursor a -> Maybe (DeleteOrUpdate (ListCursor a))
listCursorRemove tc =
  case listCursorPrev tc of
    [] ->
      case listCursorNext tc of
        [] -> Just Deleted
        _ -> Nothing
    (_ : prev) -> Just $ Updated $ tc {listCursorPrev = prev}

listCursorDelete :: ListCursor a -> Maybe (DeleteOrUpdate (ListCursor a))
listCursorDelete tc =
  case listCursorNext tc of
    [] ->
      case listCursorPrev tc of
        [] -> Just Deleted
        _ -> Nothing
    (_ : next) -> Just $ Updated $ tc {listCursorNext = next}

listCursorSplit :: ListCursor a -> (ListCursor a, ListCursor a)
listCursorSplit ListCursor {..} =
  ( ListCursor {listCursorPrev = listCursorPrev, listCursorNext = []},
    ListCursor {listCursorPrev = [], listCursorNext = listCursorNext}
  )

listCursorCombine :: ListCursor a -> ListCursor a -> ListCursor a
listCursorCombine lc1 lc2 =
  ListCursor
    { listCursorPrev = reverse $ rebuildListCursor lc1,
      listCursorNext = rebuildListCursor lc2
    }

traverseListCursor :: ([a] -> [a] -> f b) -> ListCursor a -> f b
traverseListCursor = foldListCursor

foldListCursor :: ([a] -> [a] -> b) -> ListCursor a -> b
foldListCursor func ListCursor {..} = func (reverse listCursorPrev) listCursorNext

