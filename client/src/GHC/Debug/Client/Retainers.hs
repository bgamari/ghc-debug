-- | Functions for computing retainers
module GHC.Debug.Client.Retainers where

import GHC.Debug.Client
import GHC.Debug.Client.Monad
import GHC.Debug.Types
import Control.Applicative
import Data.Foldable
import Control.Monad

computeRetainers :: [ClosurePtr] -- ^ Roots to start from
                 -> ClosurePtr   -- ^ Target
                 -> DebugM RetainerSet  -- ^ Information about retainer sets
computeRetainers roots target = do
  (forM roots $ \r -> findRetainer target r)

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
      res <- getConst . tritraverse Const Const Const <$>
        tritraverse (const (return NoPath)) (findRetainerStack target) (findRetainer target) dc
      case res of
        NoPath -> return NoPath
        RetainerPath p -> return (RetainerPath (cp : p))


findRetainerStack :: ClosurePtr -> StackCont -> DebugM RetainerPath
findRetainerStack target sc = do
  ds <- dereferenceStack sc
  fold <$> traverse (findRetainer target) ds

