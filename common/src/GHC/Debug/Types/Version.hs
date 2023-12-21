module GHC.Debug.Types.Version where

import Data.Word

data Version = Version { v_major :: Word32
                       , v_patch :: Word32
                       , v_profiling :: Bool
                       , v_tntc :: Bool
                       } deriving (Show, Ord, Eq)
