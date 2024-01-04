module GHC.Debug.Types.Version where

import Data.Word
import Data.Maybe (isJust)

data ProfilingMode
  = NoProfiling -- we are running profiled code but not doing any profiling right now
  | RetainerProfiling
  | LDVProfiling
  | EraProfiling
  | OtherProfiling
  deriving (Eq, Ord, Show, Enum)

data Version = Version { v_major :: Word32
                       , v_patch :: Word32
                       , v_profiling :: Maybe ProfilingMode
                       , v_tntc :: Bool
                       } deriving (Show, Ord, Eq)

isProfiledRTS :: Version -> Bool
isProfiledRTS = isJust . v_profiling
