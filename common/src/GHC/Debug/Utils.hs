module GHC.Debug.Utils where

import Data.Binary.Get
import Data.ByteString.Lazy
import GHC.Stack

runGet_ :: HasCallStack => Get a -> ByteString -> a
runGet_ g b = case runGetOrFail g b of
                Left (_, _, err) -> error err
                Right (_, _, r) -> r
