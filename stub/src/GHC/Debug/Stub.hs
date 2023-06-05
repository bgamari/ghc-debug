{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
This module provides the functions you need to use to instrument your application
so it can be debugged using ghc-debug. Usually all you need to do is to
wrap the main function with the 'withGhcDebug' wrapper.

@
    main = withGhcDebug $ do ...
@

Then when you application starts, a socket will be created which the debugger
can be attached to. The location of the socket is controlled by the @GHC_DEBUG_SOCKET@
environment variable.
-}
module GHC.Debug.Stub
  ( withGhcDebug
  , withGhcDebugUnix
  , withGhcDebugTCP
  , SocketAddr (..)
  , withGhcDebugX
  , saveClosures
  , Box(..)
  , pause
  , resume
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.StablePtr
import GHC.Exts
import GHC.Int
import GHC.IO
import GHC.Prim
import System.FilePath
import System.Directory
import System.Environment
import System.Mem
import System.IO

import GHC.Debug.Convention (socketDirectory)

foreign import ccall safe "start_over_tcp"
    start_over_tcp_c :: CString -> Word16 -> IO ()

foreign import ccall safe "start_over_un"
    start_over_un_c :: CString -> IO ()

foreign import ccall safe "unistd.h getpid"
    getpid_c :: IO CInt

-- | Start listening on a unix domain for remote debugging.
-- You should wrap your main thread in this as it performs some cleanup on exit.
-- If not used on the Main thread, user interupt (Ctrl-C) may skip the cleanup
-- step.
--
-- By default the socket is created by referring to 'socketDirectory' which is
-- in your XDG data directory.
--
-- The socket created can also be controlled using the @GHC_DEBUG_SOCKET@
-- environment variable.
withGhcDebug :: IO a -> IO a
withGhcDebug main = do
    defaultSocketPath <- getDefaultSocketPath
    socketPath <- fromMaybe defaultSocketPath <$> lookupEnv "GHC_DEBUG_SOCKET"
    withGhcDebugUnix socketPath main
  where
    getDefaultSocketPath = do
        socketOverride <- fromMaybe "" <$> lookupEnv "GHC_DEBUG_SOCKET"
        if not (null socketOverride)
        then return socketOverride
        else do
            dir <- socketDirectory
            name <- getProgName
            pid <- show <$> getpid_c
            let socketName = pid ++ "-" ++ name
            return (dir </> socketName)

-- | Similar to 'withGhcDebug', but with an explicit socket path
--
-- The file directory will be created automatically if it does not exist.
--
-- > main = withGhcDebugUnix "/tmp/ghc-debug" $ do ...
withGhcDebugUnix :: String -> IO a -> IO a
withGhcDebugUnix socketPath main = do
    createDirectoryIfMissing True (takeDirectory socketPath)
    hPutStrLn stderr $ "Starting ghc-debug on socket: " ++ socketPath

    -- Start a thread to handle requests
    _threadId <- forkIO $ withCString socketPath start_over_un_c

    -- Run the main thread with cleanup
    main
        `finally`
        (removeFile socketPath
            <|> putStrLn ("ghc-debug: failed to cleanup socket: " ++ socketPath)
        )

-- | Start listening on a tcp for remote debugging.
--
-- > main = withGhcDebugTCP "127.0.0.1" 1235 $ do ...
withGhcDebugTCP :: String -> Word16 -> IO a -> IO a
withGhcDebugTCP host port main = do
    hPutStrLn stderr $ "Starting ghc-debug on tcp: " ++ host ++ ":" ++ (show port)

    -- Start a thread to handle requests
    _threadId <- forkIO $ withCString host $ \host_c ->
      start_over_tcp_c host_c port

    -- Run the main thread
    main

data SocketAddr
  = SocketAddrIp !String !Word16
  | SocketAddrUnix !String
  deriving (Show, Eq)

withGhcDebugX :: SocketAddr -> IO a -> IO a
withGhcDebugX (SocketAddrUnix socketPath) = withGhcDebugUnix socketPath
withGhcDebugX (SocketAddrIp host port) = withGhcDebugTCP host port

-- | Break program execution for debugging.
foreign import ccall safe "pause_mutator"
    pause_c :: IO ()

pause :: IO ()
pause = performGC >> pause_c

-- | Resume program execution for debugging.
foreign import ccall safe "resume_mutator"
    resume :: IO ()

foreign import ccall unsafe "saveClosures" c_saveClosures
    :: CInt -> Ptr (Ptr ()) -> IO ()

data Box = forall a . Box a

unbox :: (forall a . a -> b) -> Box -> b
unbox f (Box a) = f a

-- | Mark a set of closures to be saved, they can then be retrieved from
-- the debugger using the 'RequestSavedClosures' requests. This can be
-- useful to transmit specific closures you care about (such as a cache or
-- large map).
saveClosures :: [Box] -> IO ()
saveClosures xs = do
  sps   <- mapM (\(Box x) -> castStablePtrToPtr <$> newStablePtr x) xs
  withArray sps $ \sps_arr ->
    c_saveClosures (fromIntegral (length xs)) sps_arr
