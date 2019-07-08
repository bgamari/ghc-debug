{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module GHC.Debug.Types where

import Control.Applicative
import Control.Exception
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Hashable
import Data.Word
import System.IO
import System.IO.Unsafe

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

-- import GHC.Exts.Heap
-- import GHC.Exts.Heap (StgInfoTable)

-- TODO: Fetch this from debuggee
tablesNextToCode :: Bool
tablesNextToCode = True

-- TODO: Fetch this from debuggee
profiling :: Bool
profiling = True

newtype InfoTablePtr = InfoTablePtr Word64
                     deriving (Eq, Ord, Show)
                     deriving newtype (Binary, Hashable)

newtype ClosurePtr = ClosurePtr Word64
                   deriving (Eq, Ord, Show)
                   deriving newtype (Binary, Hashable)

newtype RawInfoTable = RawInfoTable BS.ByteString
                     deriving (Eq, Ord, Show)
                     deriving newtype (Binary)

newtype RawClosure = RawClosure BS.ByteString
                   deriving (Eq, Ord, Show)
                   deriving newtype (Binary)

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
    -- | Request a set of info tables.
    RequestInfoTables :: [InfoTablePtr] -> Request [RawInfoTable]
    -- | Wait for the debugee to pause itself and then
    -- execute an action
    RequestPoll :: Request ()
    -- | A client can save objects by calling a special RTS method
    -- This function returns the closures it saved.
    RequestSavedObjects :: Request [ClosurePtr]
    -- | Calls the debugging `findPtr` function and returns the retainers
    RequestFindPtr :: ClosurePtr -> Request [ClosurePtr]


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

cmdRequestPoll :: CommandId
cmdRequestPoll = CommandId 7

putCommand :: CommandId -> Put -> Put
putCommand c body = do
    putWord32be $ fromIntegral $ (4 + BSL.length body')
    put c
    putLazyByteString body'
  where
    body' = runPut body

putRequest :: Request a -> Put
putRequest RequestVersion        = putCommand cmdRequestVersion mempty
putRequest RequestPause          = putCommand cmdRequestPause mempty
putRequest RequestResume         = putCommand cmdRequestResume mempty
putRequest RequestRoots          = putCommand cmdRequestRoots mempty
putRequest (RequestClosures cs)  = putCommand cmdRequestClosures $ foldMap put cs
putRequest RequestPoll           = putCommand cmdRequestPoll mempty
putRequest _ = error "Not implemented"

getResponse :: Request a -> Get a
getResponse RequestVersion       = getWord32be
getResponse RequestPause         = get
getResponse RequestResume        = get
getResponse RequestRoots         = many get
getResponse (RequestClosures _)  = many get
getResponse RequestPoll          = get
getResponse _ = error "Not implemented"

data Error = BadCommand
           | AlreadyPaused
           | NotPaused
           deriving stock (Eq, Ord, Show)

instance Exception Error

data ResponseCode = Okay
                  | OkayContinues
                  | Error Error
                  deriving stock (Eq, Ord, Show)

getResponseCode :: Get ResponseCode
getResponseCode = getWord16be >>= f
  where
    f 0x0   = pure $ Okay
    f 0x1   = pure $ OkayContinues
    f 0x100 = pure $ Error BadCommand
    f 0x101 = pure $ Error AlreadyPaused
    f 0x102 = pure $ Error NotPaused
    f _     = fail "Unknown response code"

data Stream a r = Next !a (Stream a r)
                | End r

readFrames :: Handle -> IO (Stream BS.ByteString (Maybe Error))
readFrames hdl = do
    bs <- BSL.hGet hdl 6
    print bs
    (respLen, status) <- runGet frameHeader <$> return bs --BSL.hGet hdl 6
    print (respLen, status)
    respBody <- BS.hGet hdl (fromIntegral respLen)
    print respBody
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

