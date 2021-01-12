{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module GHC.Debug.Types(module T
                      , Request(..)
                      , requestCommandId
                      , doRequest
                      , isWriteRequest
                      , withWriteRequest
                      , isImmutableRequest
                      , AnyReq(..)
                      , AnyResp(..)
                      , CommandId(..)
                      , SourceInformation(..)
                      , ClosureType(..)

                      -- * Serialisation functions
                      , getIPE
                      , putIPE
                      , getInfoTable
                      , putInfoTable
                      , putRequest
                      , getRequest ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Foldable as F
import Data.Word
import System.IO

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Hashable

import GHC.Debug.Types.Closures as T
import GHC.Debug.Types.Ptr as T
import GHC.Exts.Heap.ClosureTypes
import GHC.Debug.Decode
import Control.Concurrent
import Debug.Trace


-- | A request sent from the debugger to the debuggee parametrized on the result type.
data Request a where
    -- | Request protocol version
    RequestVersion :: Request Word32
    -- | Pause the debuggee.
    RequestPause :: Request ()
    -- | Resume the debuggee.
    RequestResume :: Request ()
    -- | Request the debuggee's root pointers.
    RequestRoots :: Request [ClosurePtr]
    -- | Request a set of closures.
    RequestClosure :: ClosurePtr -> Request RawClosure
    -- | Request a set of info tables.
    RequestInfoTable :: InfoTablePtr -> Request (StgInfoTableWithPtr, RawInfoTable)
    -- | Wait for the debuggee to pause itself and then
    -- execute an action. It currently impossible to resume after
    -- a pause caused by a poll.
    RequestPoll :: Request ()
    -- | A client can save objects by calling a special RTS method
    -- This function returns the closures it saved.
    RequestSavedObjects :: Request [ClosurePtr]
--    -- | Calls the debugging `findPtr` function and returns the retainers
--    RequestFindPtr :: ClosurePtr -> Request [ClosurePtr]
    -- | Request the pointer bitmap for a stack frame.
    RequestStackBitmap :: StackPtr -> Word32 -> Request PtrBitmap
    -- | Decode the bitmap container in a StgFunInfoTable
    -- Used by PAP and AP closure types.
    RequestFunBitmap :: Word16 -> ClosurePtr -> Request PtrBitmap
    -- | Request the description for an info table.
    -- The `InfoTablePtr` is just used for the equality
    RequestConstrDesc :: InfoTablePtr -> Request ConstrDesc
    -- | Lookup source information of an info table
    RequestSourceInfo :: InfoTablePtr -> Request (Maybe SourceInformation)
    -- | Copy all blocks from the process at once
    RequestAllBlocks :: Request [RawBlock]
    -- | Request the block which contains a specific pointer
    RequestBlock :: ClosurePtr -> Request RawBlock

data SourceInformation = SourceInformation { infoName        :: !String
                                         , infoClosureType :: !ClosureType
                                         , infoType        :: !String
                                         , infoLabel       :: !String
                                         , infoModule      :: !String
                                         , infoPosition    :: !String }
                                         deriving (Show, Eq, Ord)



eq1request :: Request a -> Request b -> Bool
eq1request r1 r2 =
  case r1 of
    RequestVersion -> case r2 of {RequestVersion -> True; _ -> False}
    RequestPause   -> case r2 of {RequestPause -> True; _ -> False }
    RequestResume  -> case r2 of {RequestResume -> True; _ -> False }
    RequestRoots   -> case r2 of {RequestRoots -> True; _ -> False }
    RequestClosure cs -> case r2 of {(RequestClosure cs') -> cs == cs'; _ -> False }
    RequestInfoTable itp -> case r2 of { (RequestInfoTable itp') ->  itp == itp'; _ -> False }
    RequestPoll           -> case r2 of { RequestPoll -> True; _ -> False }
    RequestSavedObjects    -> case r2 of {RequestSavedObjects -> True; _ -> False }
    RequestStackBitmap p o      -> case r2 of {(RequestStackBitmap p' o') -> p == p' && o == o'; _ -> False }
    RequestFunBitmap n cp    -> case r2 of {(RequestFunBitmap n' cp') -> n == n' && cp == cp'; _ -> False }
    RequestConstrDesc cp   -> case r2 of { (RequestConstrDesc cp') -> cp == cp'; _ -> False }
    RequestSourceInfo itp  -> case r2 of { (RequestSourceInfo itp') -> itp == itp'; _ -> False }
    RequestAllBlocks       -> case r2 of { RequestAllBlocks -> True; _ -> False }
    RequestBlock cp        -> case r2 of { RequestBlock cp' -> cp == cp'; _ -> False }

-- | Whether a request mutates the debuggee state, don't cache these ones
isWriteRequest :: Request a -> Bool
isWriteRequest r = getConst $ withWriteRequest r (Const False) (const (Const True))

withWriteRequest :: Request a -> r a -> ((a ~ ()) => Request a -> r a) -> r a
withWriteRequest r def k =
  case r of
    RequestPause  -> k RequestPause
    RequestResume -> k RequestResume
    RequestPoll -> k RequestPoll
    _ -> def

-- | Requests which will always answer the same.
-- For example, info tables are immutable and so requesting an info table
-- will always result in the same value and is safe to cache across pause
-- lines.
isImmutableRequest :: Request a -> Bool
isImmutableRequest r =
  case r of
    RequestVersion {} -> True
    RequestInfoTable {} -> True
    RequestSourceInfo {} -> True
    RequestConstrDesc {} -> True
    _ -> False





deriving instance Show (Request a)
deriving instance Eq (Request a)
--deriving instance Ord (Request a)

instance Hashable (Request a) where
  hashWithSalt s r = case r of
    RequestVersion ->  s `hashWithSalt` cmdRequestVersion
    RequestPause   ->  s `hashWithSalt` cmdRequestPause
    RequestResume  ->  s `hashWithSalt` cmdRequestResume
    RequestRoots   -> s `hashWithSalt` cmdRequestRoots
    RequestClosure cs -> s `hashWithSalt` cmdRequestClosures `hashWithSalt` cs
    RequestInfoTable itp -> s `hashWithSalt` cmdRequestInfoTables `hashWithSalt` itp
    RequestPoll           -> s `hashWithSalt` cmdRequestPoll
    RequestSavedObjects    -> s `hashWithSalt` cmdRequestSavedObjects
    RequestStackBitmap p o -> s `hashWithSalt` cmdRequestStackBitmap `hashWithSalt` p `hashWithSalt` o
    RequestFunBitmap n cp  -> s `hashWithSalt` cmdRequestFunBitmap `hashWithSalt` cp `hashWithSalt` n
    RequestConstrDesc cp   -> s `hashWithSalt` cmdRequestConstrDesc `hashWithSalt` cp
    RequestSourceInfo itp  -> s `hashWithSalt` cmdRequestSourceInfo `hashWithSalt` itp
    RequestAllBlocks       -> s `hashWithSalt` cmdRequestAllBlocks
    RequestBlock cp        -> s `hashWithSalt` cmdRequestBlock `hashWithSalt` cp



newtype CommandId = CommandId Word32
                  deriving (Eq, Ord, Show)
                  deriving newtype (Binary, Hashable)

requestCommandId :: Request a -> CommandId
requestCommandId r = case r of
    RequestVersion {} -> cmdRequestVersion
    RequestPause {}   -> cmdRequestPause
    RequestResume {}  -> cmdRequestResume
    RequestRoots {}   -> cmdRequestRoots
    RequestClosure {}  -> cmdRequestClosures
    RequestInfoTable {}  -> cmdRequestInfoTables
    RequestPoll {}         -> cmdRequestPoll
    RequestSavedObjects {} -> cmdRequestSavedObjects
    RequestStackBitmap {}       -> cmdRequestStackBitmap
    RequestFunBitmap {}       -> cmdRequestFunBitmap
    RequestConstrDesc {}   -> cmdRequestConstrDesc
    RequestSourceInfo {}   -> cmdRequestSourceInfo
    RequestAllBlocks {} -> cmdRequestAllBlocks
    RequestBlock {} -> cmdRequestBlock

cmdRequestVersion :: CommandId
cmdRequestVersion = CommandId 1

cmdRequestPause :: CommandId
cmdRequestPause = CommandId 2

cmdRequestResume :: CommandId
cmdRequestResume = CommandId 3

cmdRequestRoots :: CommandId
cmdRequestRoots = CommandId 4

cmdRequestClosures :: CommandId
cmdRequestClosures = CommandId 5

cmdRequestInfoTables :: CommandId
cmdRequestInfoTables = CommandId 6

cmdRequestStackBitmap :: CommandId
cmdRequestStackBitmap = CommandId 7

cmdRequestPoll :: CommandId
cmdRequestPoll = CommandId 8

cmdRequestSavedObjects :: CommandId
cmdRequestSavedObjects = CommandId 9

--cmdRequestFindPtr :: CommandId
--cmdRequestFindPtr = CommandId 10

cmdRequestConstrDesc :: CommandId
cmdRequestConstrDesc = CommandId 11

cmdRequestSourceInfo :: CommandId
cmdRequestSourceInfo = CommandId 12

cmdRequestAllBlocks :: CommandId
cmdRequestAllBlocks = CommandId 14

cmdRequestBlock :: CommandId
cmdRequestBlock = CommandId 15

cmdRequestFunBitmap :: CommandId
cmdRequestFunBitmap = CommandId 16

data AnyReq = forall req . AnyReq !(Request req)

instance Hashable AnyReq where
  hashWithSalt s (AnyReq r) = hashWithSalt s r

instance Eq AnyReq where
  (AnyReq r1) == (AnyReq r2) = eq1request r1 r2

data AnyResp = forall a . AnyResp !a !(a -> Put)

putCommand :: CommandId -> Put -> Put
putCommand c body = do
    putWord32be $ fromIntegral (4 + BSL.length body')
    put c
    putLazyByteString body'
  where
    body' = runPut body

putRequest :: Request a -> Put
putRequest RequestVersion        = putCommand cmdRequestVersion mempty
putRequest RequestPause          = putCommand cmdRequestPause mempty
putRequest RequestResume         = putCommand cmdRequestResume mempty
putRequest RequestRoots          = putCommand cmdRequestRoots mempty
putRequest (RequestClosure cs)  =
  putCommand cmdRequestClosures $ do
    putWord16be 1
    put cs
putRequest (RequestInfoTable ts) =
  putCommand cmdRequestInfoTables $ do
    putWord16be 1
    put ts
putRequest (RequestStackBitmap sp o)       =
  putCommand cmdRequestStackBitmap $ put sp >> putWord32be o
putRequest (RequestFunBitmap n cp)       =
  putCommand cmdRequestFunBitmap $ put cp >> putWord16be n
putRequest (RequestConstrDesc itb) =
  putCommand cmdRequestConstrDesc $ put itb
putRequest RequestPoll           = putCommand cmdRequestPoll mempty
putRequest RequestSavedObjects   = putCommand cmdRequestSavedObjects mempty
--putRequest (RequestFindPtr c)       =
--  putCommand cmdRequestFindPtr $ put c
putRequest (RequestSourceInfo it) = putCommand cmdRequestSourceInfo $ put it
putRequest (RequestAllBlocks) = putCommand cmdRequestAllBlocks $ return ()
putRequest (RequestBlock cp)  = putCommand cmdRequestBlock $ put cp

-- This is used to serialise the RequestCache
getRequest :: Get AnyReq
getRequest = do
  len <- getWord32be
  isolate (fromIntegral len) $ do
    cmd <- get
    if
      | cmd == cmdRequestVersion -> return (AnyReq RequestVersion)
      | cmd == cmdRequestPause   -> return (AnyReq RequestPause)
      | cmd == cmdRequestResume  -> return (AnyReq RequestResume)
      | cmd == cmdRequestRoots   -> return (AnyReq RequestRoots)
      | cmd == cmdRequestClosures -> do
          _n <- getWord16be
--          cs <- replicateM (fromIntegral n) get
          cp <- get
          return (AnyReq (RequestClosure cp))
      | cmd == cmdRequestInfoTables -> do
          _n <- getWord16be
          --itbs <- replicateM (fromIntegral n) get
          itb <- get
          return (AnyReq (RequestInfoTable itb))
      | cmd == cmdRequestStackBitmap -> do
          sp <- get
          o  <- getWord32be
          return (AnyReq (RequestStackBitmap sp o))
      | cmd == cmdRequestFunBitmap -> do
          cp <- get
          n <- getWord16be
          return (AnyReq (RequestFunBitmap n cp))
      | cmd == cmdRequestConstrDesc -> do
          itb <- get
          return (AnyReq (RequestConstrDesc itb))
      | cmd == cmdRequestPoll -> return (AnyReq RequestPoll)
      | cmd == cmdRequestSavedObjects -> return (AnyReq RequestSavedObjects)
      | cmd == cmdRequestSourceInfo -> do
          it <- get
          return (AnyReq (RequestSourceInfo it))
      | cmd == cmdRequestAllBlocks -> return (AnyReq RequestAllBlocks)
      | cmd == cmdRequestBlock -> do
            cp <- get
            return (AnyReq (RequestBlock cp))
      | otherwise -> error (show cmd)


getResponse :: Request a -> Get a
getResponse RequestVersion       = getWord32be
getResponse RequestPause         = get
getResponse RequestResume        = get
getResponse RequestRoots         = many get
getResponse (RequestClosure {}) = get
getResponse (RequestInfoTable itbp) = (\(it, r) -> (StgInfoTableWithPtr itbp it, r)) <$> getInfoTable
--    zipWith (\p (it, r) -> (StgInfoTableWithPtr p it, r)) itps
--      <$> replicateM (length itps) getInfoTable
getResponse (RequestStackBitmap {}) = get
getResponse (RequestFunBitmap {}) = get
getResponse (RequestConstrDesc _)  = getConstrDesc
getResponse RequestPoll          = get
getResponse RequestSavedObjects  = many get
getResponse (RequestSourceInfo _c) = getIPE
getResponse RequestAllBlocks = many get
getResponse RequestBlock {}  = get


getConstrDesc :: Get ConstrDesc
getConstrDesc = do
  len <- getInt32be
  parseConstrDesc . C8.unpack <$> getByteString (fromIntegral len)

getIPE :: Get (Maybe SourceInformation)
getIPE = do
  num <- getInt32be
  res <- replicateM (fromIntegral num) getOne
  case res of
    (id_name:cty:ty:lab:modu:loc:[]) ->
      return . Just $! SourceInformation id_name (readCTy cty) ty lab modu loc
    [] -> return Nothing
    fs -> fail (show ("Expecting 6 or 0 fields in IPE" :: String,  fs,num))
  where
    getOne = do
      !len <- getInt32be
      !res <- C8.unpack <$> getByteString (fromIntegral len)
      return res
    -- All constructor nodes get 0, this is a wibble in the implementation
    -- of IPEs
    readCTy "0" = CONSTR
    readCTy n   = toEnum (read @Int n)

putIPE :: Maybe SourceInformation -> Put
putIPE Nothing = putInt32be 0
putIPE (Just (SourceInformation a ty b c d e)) = do
  putInt32be 6
  putOne a
  putOne (show (fromEnum ty))
  putOne b
  putOne c
  putOne d
  putOne e
  where
    putOne s = do
      putInt32be (fromIntegral $ length s)
      putByteString (C8.pack s)




getInfoTable :: Get (StgInfoTable, RawInfoTable)
getInfoTable = do
  !len <- getInt32be
  !r <- RawInfoTable <$> getByteString (fromIntegral len)
  let !it = decodeInfoTable r
  return (it, r)

putInfoTable :: RawInfoTable -> Put
putInfoTable (RawInfoTable rc) = do
  let n = BS.length rc
  putWord32be (fromIntegral n)
  putByteString rc



data Error = BadCommand
           | BadStack
           | AlreadyPaused
           | NotPaused
           | NoResume
           deriving stock (Eq, Ord, Show)

instance Exception Error

data ResponseCode = Okay
                  | OkayContinues
                  | Error Error
                  deriving stock (Eq, Ord, Show)

getResponseCode :: Get ResponseCode
getResponseCode = getWord16be >>= f
  where
    f 0x0   = pure Okay
    f 0x1   = pure OkayContinues
    f 0x100 = pure $ Error BadCommand
    f 0x104 = pure $ Error BadStack
    f 0x101 = pure $ Error AlreadyPaused
    f 0x102 = pure $ Error NotPaused
    f 0x103 = pure $ Error NoResume
    f _     = fail "Unknown response code"

data Stream a r = Next !a (Stream a r)
                | End r

readFrames :: Handle -> IO (Stream BS.ByteString (Maybe Error))
readFrames hdl = do
    (respLen, status) <- runGet frameHeader <$> BSL.hGet hdl 6
    respBody <- BS.hGet hdl (fromIntegral respLen)
    case status of
      OkayContinues -> do rest <- readFrames hdl
                          return $ Next respBody rest
      Okay     -> return $ Next respBody (End Nothing)
      Error err-> return $ End (Just err)
  where
    frameHeader :: Get (Word32, ResponseCode)
    frameHeader =
      (,) <$> getWord32be
          <*> getResponseCode

throwStream :: Exception e => Stream a (Maybe e) -> [a]
throwStream = f
  where
    f (Next x rest)  = x : f rest
    f (End Nothing)  = []
    f (End (Just e)) = throw e

concatStream :: Stream BS.ByteString (Maybe Error) -> BSL.ByteString
concatStream = BSL.fromChunks . throwStream

doRequest :: MVar Handle -> Request a -> IO a
doRequest mhdl req = withMVar mhdl $ \hdl -> do
    BSL.hPutStr hdl $ runPut $ putRequest req
    bframes <- readFrames hdl
    let x = runGet (getResponse req) (concatStream bframes)
    return x


