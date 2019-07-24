{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module GHC.Debug.Stub (pause, resume, start, saveClosures, Box(..)) where

import GHC.Prim
import GHC.Exts
import GHC.IO
import GHC.Int
import Foreign.StablePtr
import Foreign.Marshal.Array
import Control.Monad
import Foreign.C.Types
import System.Mem

-- | Start listening for remote debugging
foreign import ccall safe "start"
    start :: IO ()

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


saveClosures :: [Box] -> IO ()
saveClosures xs = do
  sps   <- mapM (\(Box x) -> castStablePtrToPtr <$> newStablePtr x) xs
  withArray sps $ \sps_arr ->
    c_saveClosures (fromIntegral (length xs)) sps_arr

