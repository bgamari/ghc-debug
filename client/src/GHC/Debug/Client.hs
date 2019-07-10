module GHC.Debug.Client
  ( Debuggee
  , withDebuggee
  , pauseDebuggee
  , request
  , Request(..)
  , getInfoTblPtr
  , decodeClosure
  , lookupInfoTable
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.Debug.Types
import GHC.Debug.Decode
import Network.Socket
import qualified Data.HashMap.Strict as HM
import System.IO

data Debuggee = Debuggee { debuggeeHdl :: Handle
                         , debuggeeInfoTblEnv :: MVar (HM.HashMap InfoTablePtr RawInfoTable)
                         }

-- | Open a debuggee's socket.
withDebuggee :: FilePath  -- ^ debuggee's socket location
             -> (Debuggee -> IO a)
             -> IO a
withDebuggee fname action = do
    s <- socket AF_UNIX Stream defaultProtocol
    print s
    putStrLn ( fname)
    connect s (SockAddrUnix fname)
    print "connected"
    hdl <- socketToHandle s ReadWriteMode
    infoTableEnv <- newMVar mempty
    action (Debuggee hdl infoTableEnv)

-- | Send a request to a 'Debuggee' paused with 'pauseDebuggee'.
request :: Debuggee -> Request resp -> IO resp
request (Debuggee hdl _) req = doRequest hdl req

lookupInfoTable :: Debuggee -> RawClosure -> IO (RawInfoTable, RawClosure)
lookupInfoTable d rc = do
    let ptr = getInfoTblPtr rc
    itblEnv <- readMVar (debuggeeInfoTblEnv d)
    case HM.lookup ptr itblEnv of
      Nothing -> do
        [itbl] <- request d (RequestInfoTables [ptr])
        modifyMVar_ (debuggeeInfoTblEnv d) $ return . HM.insert ptr itbl
        return (itbl, rc)
      Just itbl ->  return (itbl, rc)

pauseDebuggee :: Debuggee -> IO a -> IO a
pauseDebuggee d =
    bracket_ (void $ request d RequestPause) (void $ request d RequestResume)
