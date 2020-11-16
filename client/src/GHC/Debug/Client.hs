{-# LANGUAGE TupleSections #-}
module GHC.Debug.Client
  ( Debuggee(..)
  , DebugM
  , withDebuggee
  , withDebuggeeSocket
  , pauseDebuggee
  , pause
  , pauseThen
  , resume
  , request
  , Request(..)
  , getInfoTblPtr
  , decodeClosure
  , FieldValue(..)
  , decodeInfoTable
  , lookupInfoTable
  , lookupDwarf
  , showFileSnippet
  , DebugClosure(..)
  , dereferenceClosures
  , dereferenceClosure
  , dereferenceStack
  , dereferenceConDesc
  , fullTraversal
  , Tritraversable(..)
  , traceRequestLog
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.Debug.Types
import GHC.Debug.Decode
import GHC.Debug.Decode.Stack
import Network.Socket
import qualified Data.HashMap.Strict as HM
import System.IO
import Data.Word
import Data.Maybe
import System.Endian
import Data.Foldable
import Data.Coerce
import Data.Bitraversable


import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.ADT.Pretty as DwarfPretty
import qualified Data.Dwarf.Elf as Dwarf.Elf

import Data.Dwarf
import Data.Dwarf.ADT
import qualified Data.Text  as T
import Data.List
import System.Process
import System.Environment
import System.FilePath
import System.Directory
import Text.Printf

import GHC.Debug.Client.Monad
import Haxl.Core hiding (Request)
import Haxl.Core.Monad (unsafeLiftIO)


import Data.IORef


lookupInfoTable :: RawClosure -> DebugM (RawInfoTable, RawClosure)
lookupInfoTable rc = do
    let ptr = getInfoTblPtr rc
    [itbl] <- request (RequestInfoTables [ptr])
    return (itbl, rc)



{-
    -- Memoisation taken care of by Haxl
    itblEnv <- readMVar (debuggeeInfoTblEnv d)
    case HM.lookup ptr itblEnv of
      Nothing -> do
        [itbl] <- request d (RequestInfoTables [ptr])
        modifyMVar_ (debuggeeInfoTblEnv d) $ return . HM.insert ptr itbl
        return (itbl, rc)
      Just itbl ->  return (itbl, rc)
-}

pause e = runHaxl e (request RequestPause)
pauseThen e d =
  pause e >> runTrace e d
resume e = runHaxl e (request RequestResume)

pauseDebuggee :: Env Debuggee String -> IO a -> IO a
pauseDebuggee e act = bracket_ (pause e) (resume e)  act


lookupDwarf :: Debuggee -> InfoTablePtr -> Maybe ([FilePath], Int, Int)
lookupDwarf d (InfoTablePtr w) = do
  (Dwarf units) <- debuggeeDwarf d
  asum (map (lookupDwarfUnit (fromBE64 w)) units)

lookupDwarfUnit :: Word64 -> Boxed CompilationUnit -> Maybe ([FilePath], Int, Int)
lookupDwarfUnit w (Boxed _ cu) = do
  low <- cuLowPc cu
  high <- cuHighPc cu
  guard (low <= w && w <= high)
  (LNE ds fs ls) <- cuLineNumInfo cu
  (fp, l, c) <- foldl' (lookupDwarfLine w) Nothing (zip ls (tail ls))
  let res_fps = if null ds then [T.unpack (cuCompDir cu) </> fp]
                           else map (\d -> T.unpack (cuCompDir cu) </> T.unpack d </> fp) ds
  return ( res_fps
         , l , c)

lookupDwarfSubprogram :: Word64 -> Boxed Def -> Maybe Subprogram
lookupDwarfSubprogram w (Boxed _ (DefSubprogram s)) = do
  low <- subprogLowPC s
  high <- subprogHighPC s
  guard (low <= w && w <= high)
  return s
lookupDwarfSubprogram _ _ = Nothing

lookupDwarfLine :: Word64
                -> Maybe (FilePath, Int, Int)
                -> (Dwarf.DW_LNE, Dwarf.DW_LNE)
                -> Maybe (FilePath, Int, Int)
lookupDwarfLine w Nothing (d, nd) =
  if lnmAddress d <= w && w <= lnmAddress nd
    then do
      let (LNEFile file _ _ _) = lnmFiles nd !! (fromIntegral (lnmFile nd) - 1)
      Just
        (T.unpack file, fromIntegral (lnmLine nd), fromIntegral (lnmColumn nd))
    else Nothing
lookupDwarfLine _ (Just r) _ =  Just r

showFileSnippet :: Debuggee -> ([FilePath], Int, Int) -> IO ()
showFileSnippet d (fps, l, c) = go fps
  where
    go [] = putStrLn ("No files could be found: " ++ show fps)
    go (fp: fps) = do
      exists <- doesFileExist fp
      -- get file modtime
      if not exists
        then go fps
        else do
          fp `warnIfNewer` debuggeeFilename d
          src <- zip [1..] . lines <$> readFile fp
          let ctx = take 10 (drop (max (l - 5) 0) src)
          putStrLn (fp <> ":" <> show l <> ":" <> show c)
          mapM_ (\(n, l) ->
           let sn = show n
           in putStrLn (sn <> replicate (5 - length sn) ' ' <> l)) ctx

dereferenceClosure :: ClosurePtr -> DebugM Closure
dereferenceClosure c = noSize . head <$> dereferenceClosures [c]

dereferenceSizedClosure :: ClosurePtr -> DebugM SizedClosure
dereferenceSizedClosure c = head <$> dereferenceClosures [c]

dereferenceClosures  :: [ClosurePtr] -> DebugM [SizedClosure]
dereferenceClosures cs = do
    raw_cs <- request (RequestClosures cs)
    let its = map getInfoTblPtr raw_cs
    --print $ map (lookupDwarf d) its
    raw_its <- request (RequestInfoTables its)
    return $ zipWith decodeClosureWithSize raw_its (zip cs raw_cs)

dereferenceStack :: StackCont -> DebugM Stack
dereferenceStack (StackCont sp) = do
  stack <- request (RequestStack sp)
  let get_bitmap p = request (RequestBitmap (getInfoTblPtr p))
      get_info_table rc =  lookupInfoTable rc
  decoded_stack <- decodeStack get_info_table get_bitmap stack
  return decoded_stack


dereferenceConDesc :: ClosurePtr -> DebugM ConstrDesc
dereferenceConDesc i = request (RequestConstrDesc i)

fullTraversal :: ClosurePtr -> DebugM UClosure
fullTraversal c = do
--  putStrLn ("TIME TO DEREFERENCE: " ++ show c)
  dc <- dereferenceSizedClosure c
--  putStrLn ("FULL TRAVERSE(" ++ show c ++ ") = " ++ show dc)
  MkFix1 <$> tritraverse dereferenceConDesc fullStackTraversal fullTraversal  dc

fullStackTraversal :: StackCont -> DebugM UStack
fullStackTraversal sc = do
  ds <- dereferenceStack sc
--  print ("FULL STACK", ds)
  MkFix2 <$> traverse fullTraversal ds

-- | Print a warning if source file (first argument) is newer than the binary (second argument)
warnIfNewer :: FilePath -> FilePath -> IO ()
warnIfNewer fpSrc fpBin = do
    modTimeSource <- getModificationTime fpSrc
    modTimeBinary <- getModificationTime fpBin

    when (modTimeSource > modTimeBinary) $
      hPutStrLn stderr $
        printf "Warning: %s is newer than %s. Code snippets might be wrong!"
        fpSrc
        fpBin


-- | Print out the number of request made for each request type
traceRequestLog :: Env u w -> IO ()
traceRequestLog d = do
  s <- readIORef (statsRef d)
  putStrLn (ppStats s)
