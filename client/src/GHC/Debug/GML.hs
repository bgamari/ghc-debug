module GHC.Debug.GML (writeTpfToGML, addSourceInfo, typePointsFromToGML) where

import GHC.Debug.TypePointsFrom as TPF
import GHC.Debug.Types (SourceInformation(..))
import GHC.Debug.Types.Closures (Size(..))
import GHC.Debug.Profile.Types (CensusStats(..), Count(..))
import GHC.Debug.Client.Monad
import GHC.Debug.Client.Query (getSourceInfo, gcRoots)

import Data.Map as Map
import Data.Int (Int32)
import Data.Semigroup
import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Foldable as F

import System.IO

type SourceInfoMap = Map.Map TPF.Key SourceInformation

typePointsFromToGML :: FilePath -> Debuggee -> IO ()
typePointsFromToGML path e = do
  (tpf, infoMap) <- run e $ do
    roots <- gcRoots
    tpf <- TPF.typePointsFrom roots
    si <- addSourceInfo tpf
    return (tpf, si)
  writeTpfToGML path tpf infoMap

-- | Exports TypePointsFrom graph to a GML file
addSourceInfo :: TypePointsFrom -> DebugM SourceInfoMap
addSourceInfo tpf = do
    -- Generate a map of InfoTablePtr to SourceInformation pairs
    let ptrs = MMap.keys . TPF.nodes $ tpf
    infos <- mapM getSourceInfo ptrs

    let kvPairs :: [(TPF.Key, SourceInformation)]
        kvPairs = do
          (k, Just si) <- zip ptrs infos
          return (k, si)

        infoMap :: SourceInfoMap
        infoMap = Map.fromList kvPairs

    return (infoMap)


writeTpfToGML :: FilePath -> TPF.TypePointsFrom -> SourceInfoMap -> IO ()
writeTpfToGML path tpf infoMap = do
  outHandle <- openFile path WriteMode
  writeGML outHandle
  hClose outHandle
  where
    ixMap :: Map.Map TPF.Key Int32
    ixMap = Map.fromList $ zip ((MMap.keys . nodes) tpf) [1..]

    lookupId :: TPF.Key -> Int32
    lookupId key' = case Map.lookup key' ixMap of
      Nothing -> error "This shouldn't happen, see function ixMap"
      Just i -> i

    writeGML :: Handle -> IO ()
    writeGML outHandle = do
      let nodesKvPairs = MMap.assocs . TPF.nodes $ tpf
          edgesKvPairs = MMap.assocs . TPF.edges $ tpf

      -- Beginning of GML file
      hPutStrLn stderr $ "Writing to file " <> path <> "..."
      writeOpenGML

      F.forM_ nodesKvPairs (uncurry writeNode)
      F.forM_ edgesKvPairs (uncurry writeEdge)

      writeCloseGML
      hPutStrLn stderr $ "Finished writing to GML file..."
      -- End of GML file
      where
        write = hPutStr outHandle

        writeOpenGML =
          write $ "graph[\n"
            <> "comment \"this is a graph in GML format\"\n"
            <> "directed 1\n"

        writeCloseGML =
          write $ "]\n"

        writeNode :: TPF.Key -> CensusStats -> IO ()
        writeNode key' cs =
          write $ "node [\n"
            <> "id " <> showPtr key' <> "\n"
            <> gmlShowCensus cs
            <> gmlShowSourceInfo key'
            <> "]\n"

        writeEdge :: TPF.Edge -> CensusStats -> IO ()
        writeEdge edge cs =
          write $ "edge [\n"
            <> "source " <> (showPtr . TPF.edgeSource) edge <> "\n"
            <> "target " <> (showPtr . TPF.edgeTarget) edge <> "\n"
            <> gmlShowCensus cs
            <> "]\n"

        gmlShowCensus :: CensusStats -> String
        gmlShowCensus (CS (Count c) (Size s) (Max (Size m))) =
          "count " <> show c <> "\n"
          <> "size " <> show s <> "\n"
          <> "max " <> show m <> "\n"

        gmlShowSourceInfo :: TPF.Key -> String
        gmlShowSourceInfo key = case Map.lookup key infoMap of
          Nothing -> mempty
          Just si -> "infoName \"" <> infoName si <> "\"\n"
            <> "infoClosureType \"" <> (show . infoClosureType) si <> "\"\n"
            <> "infoType \"" <> infoType si <> "\"\n"
            <> "infoLabel \"" <> infoLabel si <> "\"\n"
            <> "infoModule \"" <> infoModule si <> "\"\n"
            <> "infoPosition \"" <> infoPosition si <> "\"\n"

        showPtr :: TPF.Key -> String
        showPtr = show . lookupId
