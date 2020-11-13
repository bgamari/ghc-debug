{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Debug.Types(module T, module GHC.Debug.Types) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Word
import System.IO
import System.IO.Unsafe

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Debug.Trace

import GHC.Debug.Types.Closures as T
import GHC.Debug.Types.Ptr as T


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
    RequestClosures :: [ClosurePtr] -> Request [RawClosure]
    -- | Request a stack
    RequestStack :: StackPtr -> Request RawStack
    -- | Request a set of info tables.
    RequestInfoTables :: [InfoTablePtr] -> Request [RawInfoTable]
    -- | Wait for the debuggee to pause itself and then
    -- execute an action. It currently impossible to resume after
    -- a pause caused by a poll.
    RequestPoll :: Request ()
    -- | A client can save objects by calling a special RTS method
    -- This function returns the closures it saved.
    RequestSavedObjects :: Request [ClosurePtr]
--    -- | Calls the debugging `findPtr` function and returns the retainers
--    RequestFindPtr :: ClosurePtr -> Request [ClosurePtr]
    -- | Request the pointer bitmap for an info table.
    RequestBitmap :: InfoTablePtr -> Request PtrBitmap
    -- | Request the description for an info table.
    RequestConstrDesc :: ClosurePtr -> Request ConstrDesc
    -- | Lookup source information of an info table
    RequestSourceInfo :: InfoTablePtr -> Request [String]

-- | A bitmap that records whether each field of a stack frame is a pointer.
newtype PtrBitmap = PtrBitmap (A.Array Int Bool) deriving (Show)

traversePtrBitmap :: Monad m => (Bool -> m a) -> PtrBitmap -> m [a]
traversePtrBitmap f (PtrBitmap arr) = mapM f (A.elems arr)

newtype CommandId = CommandId Word32
                  deriving (Eq, Ord, Show)
                  deriving newtype (Binary)

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

cmdRequestBitmap :: CommandId
cmdRequestBitmap = CommandId 7

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

cmdRequestStack :: CommandId
cmdRequestStack = CommandId 13

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
putRequest (RequestClosures cs)  =
  putCommand cmdRequestClosures $ do
    putWord16be $ fromIntegral (length cs)
    foldMap put cs
putRequest (RequestInfoTables ts) =
  putCommand cmdRequestInfoTables $ do
    putWord16be $ fromIntegral (length ts)
    foldMap put ts
putRequest (RequestBitmap info)       =
  putCommand cmdRequestBitmap $ put info
putRequest (RequestConstrDesc info) =
  putCommand cmdRequestConstrDesc $ put info
putRequest RequestPoll           = putCommand cmdRequestPoll mempty
putRequest RequestSavedObjects   = putCommand cmdRequestSavedObjects mempty
--putRequest (RequestFindPtr c)       =
--  putCommand cmdRequestFindPtr $ put c
putRequest (RequestSourceInfo it) = putCommand cmdRequestSourceInfo $ put it
putRequest (RequestStack sp) = putCommand cmdRequestStack $ put sp

getResponse :: Request a -> Get a
getResponse RequestVersion       = getWord32be
getResponse RequestPause         = get
getResponse RequestResume        = get
getResponse RequestRoots         = many get
getResponse (RequestClosures _)  = many getRawClosure
getResponse (RequestInfoTables _) = many getRawInfoTable
getResponse (RequestBitmap _)    = getPtrBitmap
getResponse (RequestConstrDesc _)  = getConstrDesc
getResponse RequestPoll          = get
getResponse RequestSavedObjects  = many get
--getResponse (RequestFindPtr _c)  = many get
getResponse (RequestSourceInfo _c) = getIPE
getResponse (RequestStack _)       = getRawStack

getConstrDesc :: Get ConstrDesc
getConstrDesc = do
  len <- getInt32be
  parseConstrDesc . C8.unpack <$> getByteString (fromIntegral len)

getIPE :: Get [String]
getIPE = do
  num <- getInt32be
  replicateM (fromIntegral num) getOne
  where
    getOne = do
      len <- getInt32be
      res <- C8.unpack <$> getByteString (fromIntegral len)
      return res


getPtrBitmap :: Get PtrBitmap
getPtrBitmap = do
  len <- getWord32be
  bits <- replicateM (fromIntegral len) getWord8
  let arr = A.listArray (0, fromIntegral len-1) (map (==1) bits)
  return $ PtrBitmap arr

getRawClosure :: Get RawClosure
getRawClosure = do
  len <- getInt32be
  RawClosure <$> getByteString (fromIntegral len)

-- The raw stack is sp to the end of stack
getRawStack :: Get RawStack
getRawStack = do
  len <- getInt32be
  RawStack <$> getByteString (fromIntegral len)

getRawInfoTable :: Get RawInfoTable
getRawInfoTable = do
  len <- getInt32be
  RawInfoTable <$> getByteString (fromIntegral len)


data Error = BadCommand
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
      OkayContinues -> do rest <- unsafeInterleaveIO $ readFrames hdl
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

doRequest :: Handle -> Request a -> IO a
doRequest hdl req = do
    BSL.hPutStr hdl $ runPut $ putRequest req
    frames <- readFrames hdl
    let x = runGet (getResponse req) (concatStream frames)
    return x

