{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{- | Type Points From analysis in the style of
- Cork: Dynamic Memory Leak Detectionfor Garbage-Collected Languages
- https://dl.acm.org/doi/10.1145/1190216.1190224
- -}
module GHC.Debug.TypePointsFrom( typePointsFrom
                               , detectLeaks
                               , TypePointsFrom(..)
                               , getNodes
                               , getEdges
                               , edgeSource
                               , edgeTarget
                               , Key
                               , Edge(..)
                               , getKey
                               ) where

import GHC.Debug.Client.Monad
import GHC.Debug.Client
import Control.Monad.State
import GHC.Debug.ParTrace
import GHC.Debug.Types.Ptr
import qualified Data.Map.Monoidal.Strict as Map
import Data.Map (Map)
import qualified Data.Map.Internal as M
import GHC.Debug.Profile
import Control.Monad.Identity
import Control.Concurrent
import Data.List (sortOn)
import Language.Dot
import qualified Data.Set as S


type Key = InfoTablePtr

data Edge = Edge !Key !Key deriving (Eq, Ord, Show)

edgeSource :: Edge -> Key
edgeTarget :: Edge -> Key
edgeSource (Edge k1 _) = k1
edgeTarget (Edge _ k2) = k2

data TypePointsFrom = TypePointsFrom { nodes :: !(Map.MonoidalMap Key CensusStats)
                                      , edges :: !(Map.MonoidalMap Edge CensusStats)
                                      } deriving (Show)

getNodes :: TypePointsFrom -> Map Key CensusStats
getEdges :: TypePointsFrom -> Map Edge CensusStats
getNodes = Map.getMonoidalMap . nodes
getEdges = Map.getMonoidalMap . edges

instance Monoid TypePointsFrom where
  mempty = TypePointsFrom mempty mempty

instance Semigroup TypePointsFrom where
  (TypePointsFrom a1 a2) <> (TypePointsFrom b1 b2) = TypePointsFrom (a1 <> b1) (a2 <> b2)

singletonTPF :: Key -> CensusStats -> [(Edge, CensusStats)] -> TypePointsFrom
singletonTPF k s es = TypePointsFrom (Map.singleton k s)
                                  (Map.fromList es)

-- | Perform a "type points from" heap census
typePointsFrom :: [ClosurePtr] -> DebugM TypePointsFrom
typePointsFrom cs = traceParFromM funcs (map (ClosurePtrWithInfo Root) cs)

  where
    nop = const (return ())
    funcs = TraceFunctionsIO nop nop nop clos visit nop

    visit :: ClosurePtr -> Context -> DebugM TypePointsFrom
    visit cp ctx = do
      sc <- dereferenceClosure cp
      let k = tableId $ info (noSize sc)
          v = mkCS (dcSize sc)
          parent_edge = case ctx of
                          Root -> []
                          Parent pk -> [(Edge k pk, v)]
      return $ TypePointsFrom Map.empty (Map.fromList parent_edge)



    clos :: ClosurePtr -> SizedClosure -> Context
              -> DebugM (Context, TypePointsFrom, DebugM () -> DebugM ())
    clos _cp sc ctx = do
      let k = tableId $ info (noSize sc)
      let s :: Size
          s = dcSize sc
          v =  mkCS s

          -- Edges point from the object TO what retains it
          parent_edge = case ctx of
                          Root -> []
                          Parent pk -> [(Edge k pk, v)]

      return (Parent k, singletonTPF k v parent_edge, id)


data Context = Root | Parent Key


-- | Repeatedly call 'typesPointsFrom' and perform the leak detection
-- analysis.
detectLeaks :: Int -> Debuggee -> IO ()
detectLeaks interval e = loop Nothing (M.empty, M.empty) 0
  where
    loop :: Maybe TypePointsFrom -> RankMaps -> Int -> IO ()
    loop prev_census rms i = do
      print i
      threadDelay (interval * 1_000_000)
      pause e
      (gs, r, new_rmaps) <- runTrace e $ do
        _ <- precacheBlocks
        rs <- gcRoots
        traceWrite (length rs)
        res <- typePointsFrom rs
        let !new_rmaps = case prev_census of
                           Nothing -> rms
                           Just pcensus -> updateRankMap rms pcensus res
        let cands = chooseCandidates (fst new_rmaps)
        traceWrite (length cands)
        gs <- mapM (findSlice (snd new_rmaps)) (take 10 cands)
        return (gs, res, new_rmaps)
      resume e
      zipWithM_ (\n g -> writeFile ("slices/"
                                      ++ show @Int i ++ "-"
                                      ++ show @Int n ++ ".dot")
                                   (renderDot g)) [0..] gs
      loop (Just r) new_rmaps (i + 1)


-- Analysis code
--
getKey :: InfoTablePtr -> DebugM String
getKey itblp = do
    loc <- getSourceInfo itblp
    itbl <- dereferenceInfoTable itblp
    case loc of
      Nothing -> getKeyFallback itblp itbl
      Just s -> return $ show (tipe itbl) ++ ":" ++ renderSourceInfo s

getKeyFallback :: ConstrDescCont -> StgInfoTable -> DebugM String
getKeyFallback itbp itbl = do
    case tipe itbl of
      t | CONSTR <= t && t <= CONSTR_NOCAF   -> do
        ConstrDesc a b c <- dereferenceConDesc itbp
        return $ a ++ ":" ++ b ++ ":" ++ c
      _ -> return $ show (tipe itbl)

type Rank = Double
type Decay = Double

data RankInfo = RankInfo !Rank !Int deriving Show

getRank :: RankInfo -> Rank
getRank (RankInfo r _) = r

default_decay :: Decay
default_decay = 0.15

rank_threshold :: Double
rank_threshold = 100

min_iterations :: Int
min_iterations = 2

applyRankFilter :: RankInfo -> Bool
applyRankFilter (RankInfo r i) = r >= rank_threshold && i >= min_iterations

-- | Lookup suitable candidates from the RankMap
-- , Chooses values based on 'rank_threshold' and 'min_iterations'
lookupRM :: Key -> RankMap Edge -> [(Edge, RankInfo)]
lookupRM k m = M.assocs filtered_map
  where
    -- TODO, work out how to use these functions O(log n)
    --smaller =  traceShow (M.size m) (M.dropWhileAntitone ((/= k) . edgeSource) $ m)
    --res_map = traceShow (M.size smaller) (M.takeWhileAntitone ((== k) . edgeSource) smaller)
    (res_map, _) = M.partitionWithKey (\e _ -> (== k) . edgeSource $ e) m
    filtered_map = M.filter (\(RankInfo r _) -> r > 0) res_map

mkDotId :: InfoTablePtr -> Id
mkDotId (InfoTablePtr w) = IntegerId (fromIntegral w)

findSlice :: RankMap Edge -> Key -> DebugM Graph
findSlice rm k = Graph StrictGraph DirectedGraph (Just (mkDotId k)) <$> evalStateT (go 3 k) S.empty

  where

    go :: Int -> InfoTablePtr -> StateT (S.Set InfoTablePtr) DebugM [Statement]
    go n cur_k = do
      visited_set <- get
      -- But don't stop going deep until we've seen a decent number of
      -- nodes
      if S.member cur_k visited_set || (n <= 0 && S.size visited_set >= 20)
        then return []
        else do
          label <- lift $ getKey cur_k
          let next_edges = take 20 (lookupRM cur_k rm)
              -- Decoding very wide is bad
              edge_stmts = map mkEdge next_edges
              node_stmt = NodeStatement (NodeId (mkDotId cur_k) Nothing) [AttributeSetValue (StringId "label") (StringId label) ]
              mkEdge (Edge _ e, ri) = EdgeStatement [ENodeId NoEdge (NodeId (mkDotId cur_k) Nothing), ENodeId DirectedEdge (NodeId (mkDotId e) Nothing)] [AttributeSetValue (StringId "label") (StringId (show (getRank ri))) ]

          modify' (S.insert cur_k)
          ss <- concat <$> mapM (go (n-1) . edgeTarget . fst) next_edges
          return $ node_stmt : edge_stmts ++ ss

renderSourceInfo :: SourceInformation -> String
renderSourceInfo s = escapeQuotes (infoName s ++ ":" ++ infoType s ++ ":" ++ infoPosition s)

escapeQuotes :: String -> String
escapeQuotes [] = []
escapeQuotes ('"':xs) = '\\' : '"' : escapeQuotes xs
escapeQuotes (x:xs) = x:escapeQuotes xs


chooseCandidates :: RankMap Key -> [Key]
chooseCandidates = map fst . reverse . sortOn (getRank . snd) . M.assocs . M.filter applyRankFilter

type RankMap k = M.Map k RankInfo

type RankMaps = (RankMap Key, RankMap Edge)

type RankUpdateMap k = M.Map k RankUpdateInfo

type RankUpdateInfo = Int -> Double -> Double

-- | Update the current rank predictions based on the difference between
-- two censuses.
updateRankMap :: (RankMap Key, RankMap Edge)
              -> TypePointsFrom
              -> TypePointsFrom
              -> (RankMap Key, RankMap Edge)
updateRankMap (rm_n, rm_e) t1 t2 = (ns, es)
  where
    !(rnodes, redges) = ratioRank t1 t2
    missingL = M.dropMissing
    missingR = M.mapMissing (\_ f -> RankInfo (f 0 0) 1)
    matched = M.zipWithMatched (\_ (RankInfo r iters) f -> RankInfo (f iters r) (iters + 1))

    !ns = runIdentity $ M.mergeA missingL missingR matched rm_n rnodes
    !es = runIdentity $ M.mergeA missingL missingR matched rm_e redges


compareSize :: CensusStats -> CensusStats -> Maybe (Int -> Double -> Double)
compareSize (cssize -> Size s1) (cssize -> Size s2) =
  if fromIntegral s2 > (1 - default_decay) * fromIntegral s1
    -- Calculate "Q"
    then if s1 > s2
          -- Shrinking phase, penalise rank
          then Just (\phases rank ->
                      rank
                        - ((fromIntegral (phases + 1))
                            * ((fromIntegral s1 / fromIntegral s2) - 1)))
          else Just (\phases rank ->
                        rank +
                          ((fromIntegral (phases + 1))
                            * ((fromIntegral s2 / fromIntegral s1) - 1)))
    else Nothing

-- | Compute how to update the ranks based on the difference between two
-- censuses.
ratioRank :: TypePointsFrom -> TypePointsFrom -> (RankUpdateMap Key, RankUpdateMap Edge)
ratioRank t1 t2 = (candidates, redges)
  where
    ns1 = getNodes t1
    ns2 = getNodes t2

    es1 = getEdges t1
    es2 = getEdges t2
    missingL = M.dropMissing
    missingR = M.dropMissing
    matched = M.zipWithMaybeMatched (\_ cs1 cs2 -> compareSize cs1 cs2)
    !candidates = runIdentity $ M.mergeA missingL missingR matched ns1 ns2

    !redges = runIdentity $ M.mergeA missingL missingR matched es1 es2



