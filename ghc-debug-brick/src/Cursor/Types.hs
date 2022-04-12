{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{- Authored by Tom Sydney Kerckhove, copied from cursor package #-}

module Cursor.Types where

import Control.Applicative
import Data.Functor.Compose
import qualified Data.Text.Internal as T
import GHC.Generics (Generic)
import Lens.Micro

isSafeChar :: Char -> Bool
isSafeChar c = T.safe c == c

data DeleteOrUpdate a
  = Deleted
  | Updated a
  deriving (Show, Eq, Generic)

instance Functor DeleteOrUpdate where
  fmap _ Deleted = Deleted
  fmap f (Updated a) = Updated (f a)

instance Applicative DeleteOrUpdate where
  pure = Updated
  Deleted <*> _ = Deleted
  _ <*> Deleted = Deleted
  (Updated f) <*> (Updated a) = Updated (f a)

instance Alternative DeleteOrUpdate where
  empty = Deleted
  Updated a <|> _ = Updated a
  Deleted <|> doua = doua

instance Monad DeleteOrUpdate where
  dou >>= f = case dou of
    Updated a -> f a
    Deleted -> Deleted

joinDeletes :: Maybe (DeleteOrUpdate a) -> Maybe (DeleteOrUpdate a) -> DeleteOrUpdate a
joinDeletes m1 m2 =
  case (m1, m2) of
    (Nothing, Nothing) -> Deleted
    (Nothing, Just a) -> a
    (Just a, _) -> a

joinDeletes3 ::
  Maybe (DeleteOrUpdate a) ->
  Maybe (DeleteOrUpdate a) ->
  Maybe (DeleteOrUpdate a) ->
  DeleteOrUpdate a
joinDeletes3 m1 m2 m3 =
  case (m1, m2, m3) of
    (Nothing, Nothing, Nothing) -> Deleted
    (Nothing, Nothing, Just a) -> a
    (Nothing, Just a, _) -> a
    (Just a, _, _) -> a

joinPossibleDeletes ::
  Maybe (DeleteOrUpdate a) -> Maybe (DeleteOrUpdate a) -> Maybe (DeleteOrUpdate a)
joinPossibleDeletes d1 d2 = getCompose $ Compose d1 <|> Compose d2

focusPossibleDeleteOrUpdate ::
  Lens' b a -> (a -> Maybe (DeleteOrUpdate a)) -> b -> Maybe (DeleteOrUpdate b)
focusPossibleDeleteOrUpdate l func = getCompose . l (Compose . func)

dullMDelete :: Maybe (DeleteOrUpdate a) -> Maybe a
dullMDelete Nothing = Nothing
dullMDelete (Just dou) = dullDelete dou

dullDelete :: DeleteOrUpdate a -> Maybe a
dullDelete Deleted = Nothing
dullDelete (Updated a) = Just a

