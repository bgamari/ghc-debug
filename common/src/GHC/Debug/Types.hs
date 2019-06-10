{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module GHC.Debug.Types where

import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Monoid
import Data.Word
import System.IO

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import GHC.Exts.Heap
import GHC.Exts.Heap (StgInfoTable)

-- TODO: Fetch this from debuggee
tablesNextToCode :: Bool
tablesNextToCode = True

-- TODO: Fetch this from debuggee
profiling :: Bool
profiling = True

newtype InfoTablePtr = InfoTablePtr Word64
newtype ClosurePtr = ClosurePtr Word64
                   deriving (Eq, Ord, Show)
                   deriving newtype (Binary)

newtype RawInfoTable = RawInfoTable BS.ByteString
                     deriving (Eq, Ord, Show)
                     deriving newtype (Binary)

newtype RawClosure = RawClosure BS.ByteString
                   deriving (Eq, Ord, Show)
                   deriving newtype (Binary)

-- | A request sent from the debugger to the debuggee parametrized on the result type.
data Request a where
    -- | Request protocol version
    RequestVersion :: Request Word64
    -- | Pause the debuggee.
    RequestPause :: Request ()
    -- | Resume the debuggee.
    RequestResume :: Request ()
    -- | Request the debuggee's root pointers.
    RequestRoots :: Request [ClosurePtr]
    -- | Request a set of closures.
    RequestClosures :: [ClosurePtr] -> Request RawClosure
    -- | Request a set of info tables.
    RequestInfoTables :: [InfoTablePtr] -> Request [StgInfoTable]

newtype CommandId = CommandId Word16
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

putCommand :: CommandId -> Put -> Put
putCommand c body = do
    putWord32be $ fromIntegral $ BSL.length body'
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

data Error = OtherError String
           deriving stock (Eq, Ord, Show, Generic)
           deriving anyclass (Binary)

getResponse :: Request a -> Get (Either Error a)
getResponse RequestVersion       = get
getResponse RequestPause         = get
getResponse RequestResume        = get
getResponse RequestRoots         = get
getResponse (RequestClosures _)  = get

doRequest :: Handle -> Request a -> IO (Either Error a)
doRequest hdl req = do
    BSL.hPutStr hdl $ runPut $ putRequest req
    respLen <- runGet getWord32be <$> BSL.hGet hdl 8
    respBody <- BSL.hGet hdl $ fromIntegral respLen
    return $! runGet (getResponse req) respBody

