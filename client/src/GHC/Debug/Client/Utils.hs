module GHC.Debug.Client.Utils where

import System.Directory
import Control.Monad
import System.IO
import Text.Printf

showFileSnippet :: FilePath -> ([FilePath], Int, Int) -> IO ()
showFileSnippet base_fp (all_fps, l, c) = go all_fps
  where
    go [] = putStrLn ("No files could be found: " ++ show all_fps)
    go (fp: fps) = do
      exists <- doesFileExist fp
      -- get file modtime
      if not exists
        then go fps
        else do
          fp `warnIfNewer` base_fp
          src <- zip [1 :: Int ..] . lines <$> readFile fp
          let ctx = take 10 (drop (max (l - 5) 0) src)
          putStrLn (fp <> ":" <> show l <> ":" <> show c)
          mapM_ (\(n, line) ->
           let sn = show n
           in putStrLn (sn <> replicate (5 - length sn) ' ' <> line)) ctx

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
