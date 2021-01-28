{-# LANGUAGE DerivingVia #-}
module GHC.Debug.Profile.Types where

import           GHC.Debug.Types
import Data.Monoid
import Data.Semigroup

newtype Count = Count Int
                deriving (Semigroup, Monoid, Num) via Sum Int
                deriving (Show, Ord, Eq)

data CensusStats = CS { cscount :: !Count, cssize :: !Size, csmax :: !(Max Size) } deriving (Show, Eq)

mkCS :: Size -> CensusStats
mkCS i = CS (Count 1) i (Max i)

instance Monoid CensusStats where
  mempty = CS mempty mempty (Max (Size 0))

instance Semigroup CensusStats where
  (CS a b c) <> (CS a1 b1 c1) = CS (a <> a1) (b <> b1) (c <> c1)
