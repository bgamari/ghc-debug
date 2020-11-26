{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module GHC.Debug.Client.RequestCache(RequestCache, cacheReq, lookupReq, emptyRequestCache) where

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import GHC.Debug.Types
import Unsafe.Coerce
import GHC.Exts

type RequestCache = HM.HashMap AnyReq AnyResp

data AnyReq = forall req . AnyReq (Request req)

instance Hashable AnyReq where
  hashWithSalt s (AnyReq r) = hashWithSalt s r

instance Eq AnyReq where
  (AnyReq r1) == (AnyReq r2) = hash r1 == hash r2

data AnyResp = AnyResp Any

cacheReq :: Request resp -> resp -> RequestCache -> RequestCache
cacheReq req resp rc = HM.insert (AnyReq req) (AnyResp (unsafeCoerce resp)) rc

lookupReq :: forall resp . Request resp -> RequestCache -> Maybe resp
lookupReq req rc = coerceResult <$> HM.lookup (AnyReq req) rc
  where
    coerceResult :: AnyResp -> resp
    coerceResult (AnyResp a) = unsafeCoerce a

emptyRequestCache :: RequestCache
emptyRequestCache = HM.empty
