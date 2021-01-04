-- | Functions for computing retainers
module GHC.Debug.Client.Retainers where

import GHC.Debug.Client
import Control.Applicative
import Data.Foldable
import Control.Monad
import Control.Monad.State

computeRetainers :: [ClosurePtr] -- ^ Roots to start from
                 -> ClosurePtr   -- ^ Target
                 -> DebugM RetainerSet  -- ^ Information about retainer sets
computeRetainers rroots target = do
  (forM rroots $ \r -> findRetainer target r)

type RetainerSet = [RetainerPath]


data RetainerPath = NoPath | RetainerPath [ClosurePtr] deriving Show

instance Semigroup RetainerPath where
  NoPath <> x = x
  (RetainerPath x) <> _ = RetainerPath x

instance Monoid RetainerPath where
  mempty = NoPath


findRetainer :: ClosurePtr -> ClosurePtr -> DebugM RetainerPath
findRetainer target cp
  | target == cp = return (RetainerPath [cp])
  | otherwise = do
      dc <- dereferenceClosureFromBlock cp
      res <- getConst . quadtraverse Const Const Const Const <$>
        quadtraverse (findRetainerPap target) (const (return NoPath)) (findRetainerStack target) (findRetainer target) dc
      case res of
        NoPath -> return NoPath
        RetainerPath p -> return (RetainerPath (cp : p))


findRetainerStack :: ClosurePtr -> StackCont -> DebugM RetainerPath
findRetainerStack target sc = do
  ds <- dereferenceStack sc
  fold <$> traverse (findRetainer target) ds

findRetainerPap :: ClosurePtr -> PayloadCont -> DebugM RetainerPath
findRetainerPap target sc = do
  ds <- dereferencePapPayload sc
  fold <$> traverse (findRetainer target) ds

