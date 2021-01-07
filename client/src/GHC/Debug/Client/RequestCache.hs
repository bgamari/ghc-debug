{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module GHC.Debug.Client.RequestCache(RequestCache
                                    , cacheReq
                                    , lookupReq
                                    , emptyRequestCache
                                    , clearMovableRequests
                                    , putCache
                                    , getCache ) where

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import GHC.Debug.Types
import Unsafe.Coerce
import GHC.Exts
import Data.Binary
import Control.Applicative
import Control.Monad
import Data.Binary.Put
import Data.Binary.Get
import Debug.Trace

newtype RequestCache = RequestCache (HM.HashMap AnyReq AnyResp)

instance Binary RequestCache where
  get = getCache
  put = putCache

cacheReq :: Request resp -> resp -> RequestCache -> RequestCache
cacheReq req resp (RequestCache rc)
  -- Don't cache the results of writes, such as pause/unpause
  | isWriteRequest req = RequestCache rc
  | otherwise = RequestCache (HM.insert (AnyReq req) (AnyResp resp (putResponseBinary req)) rc)

lookupReq :: forall resp . Request resp -> RequestCache -> Maybe resp
lookupReq req (RequestCache rc) = coerceResult <$> HM.lookup (AnyReq req) rc
  where
    coerceResult :: AnyResp -> resp
    coerceResult (AnyResp a _) = unsafeCoerce a

emptyRequestCache :: RequestCache
emptyRequestCache = RequestCache HM.empty

-- These get/put functions are a lot like the ones for serialising info
-- to/from the debuggee but we are careful that each one reads a bounded
-- amount of input.

getResponseBinary :: Request a -> Get a
getResponseBinary RequestVersion       = getWord32be
getResponseBinary RequestPause         = get
getResponseBinary RequestResume        = get
getResponseBinary RequestRoots         = get
getResponseBinary (RequestClosure cs) = getRawClosure
--    replicateM (length cs) getRawClosure
getResponseBinary (RequestInfoTable itps) = (\(it, r) -> (StgInfoTableWithPtr itps it, r)) <$> getInfoTable
--    zipWith (\p (it, r) -> (StgInfoTableWithPtr p it, r)) itps
--      <$> replicateM (length itps) getInfoTable
getResponseBinary (RequestStackBitmap {}) = getPtrBitmap
getResponseBinary (RequestFunBitmap {}) = getPtrBitmap
getResponseBinary (RequestConstrDesc _)  = getConstrDesc
getResponseBinary RequestPoll          = get
getResponseBinary RequestSavedObjects  = get
getResponseBinary (RequestSourceInfo _c) = getIPE
getResponseBinary RequestAllBlocks = get
getResponseBinary RequestBlock {}  = get

putResponseBinary :: Request a -> a -> Put
putResponseBinary RequestVersion w = putWord32be w
putResponseBinary RequestPause w       = put w
putResponseBinary RequestResume w      = put w
putResponseBinary RequestRoots  rs     = put rs
putResponseBinary (RequestClosure {}) rcs = putRawClosure rcs
putResponseBinary (RequestInfoTable itps) (t, r) = putInfoTable r
--    mapM_ putInfoTable  (map (\(t, r) -> r) pitb)
putResponseBinary (RequestStackBitmap {}) pbm = putPtrBitmap pbm
putResponseBinary (RequestFunBitmap {}) pbm = putPtrBitmap pbm
putResponseBinary (RequestConstrDesc _) cd  = putConstrDesc cd
putResponseBinary RequestPoll         r = put r
putResponseBinary RequestSavedObjects os = putList os
putResponseBinary (RequestSourceInfo _c) ipe = putIPE ipe
putResponseBinary RequestAllBlocks rs = put rs
putResponseBinary RequestBlock {} r = put r

putLine :: AnyReq -> AnyResp -> Put -> Put
putLine (AnyReq req) (AnyResp resp p) k = putRequest req >> p resp >> k

getCacheLine :: Get (AnyReq, AnyResp)
getCacheLine = do
  AnyReq req <- getRequest
  resp <- getResponseBinary req
  return (AnyReq req, AnyResp resp (putResponseBinary req))


putCache :: RequestCache -> Put
putCache (RequestCache rc) = do
  put (HM.size rc)
  HM.foldrWithKey putLine (return ()) rc

getCache :: Get RequestCache
getCache = do
  n <- get
  RequestCache . HM.fromList <$> replicateM n getCacheLine

-- | Clear the part of the cache which will become invalid after pausing
-- For example, we need to clear blocks, but can keep the info table
-- caches.
clearMovableRequests :: RequestCache -> RequestCache
clearMovableRequests (RequestCache rc) = RequestCache (HM.filterWithKey (\(AnyReq r) _ -> isImmutableRequest r) rc)
