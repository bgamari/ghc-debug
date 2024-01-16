{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Functions for computing retainers
module GHC.Debug.Retainers
  ( findRetainers
  , findRetainersOf
  , findRetainersOfConstructor
  , findRetainersOfConstructorExact
  , findRetainersOfInfoTable
  , findRetainers
  , addLocationToStack
  , displayRetainerStack
  , addLocationToStack'
  , displayRetainerStack'
  , findRetainersOfArrWords
  , EraRange(..)
  , profHeaderInEraRange
  , Filter(..)
  , findRetainersOfEra) where

import Prelude hiding (filter)
import GHC.Debug.Client
import Control.Monad.State
import GHC.Debug.Trace
import GHC.Debug.Types.Graph
import Control.Monad
import Control.Monad.Extra

import qualified Data.Set as Set
import Control.Monad.RWS
import Data.Word

addOne :: [ClosurePtr] -> (Maybe Int, [[ClosurePtr]]) -> (Maybe Int, [[ClosurePtr]])
addOne _ (Just 0, cp) = (Just 0, cp)
addOne cp (n, cps)    = (subtract 1 <$> n, cp:cps)

data EraRange
  = EraRange { startEra :: Word64, endEra :: Word64} -- inclusive
  deriving (Eq, Ord, Show)

inEraRange :: Word64 -> Maybe EraRange -> Bool
inEraRange _ Nothing = True
inEraRange n (Just (EraRange s e)) = s <= n && n <= e

profHeaderInEraRange :: Maybe (ProfHeader a) -> Maybe EraRange -> Bool
profHeaderInEraRange Nothing _ = True
profHeaderInEraRange (Just ph) eras
  = case hp ph of
      EraWord w -> w `inEraRange` eras
      _ -> True -- Don't filter if no era profiling

data Filter
 = ConstructorDescFilter (ConstrDesc -> Bool)
 | InfoFilter (StgInfoTable -> Bool)
 | InfoPtrFilter (InfoTablePtr -> Bool)
 | InfoSourceFilter (SourceInformation -> Bool)
 | SizeFilter (Size -> Bool)
 | ProfHeaderFilter (Maybe ProfHeaderWithPtr -> Bool)
 | AddressFilter (ClosurePtr -> Bool)
 | ParentFilter Int Filter
 | SomeParentFilter (Maybe Int) Filter
 | AllParentFilter (Maybe Int) Filter
 | AndFilter Filter Filter
 | OrFilter Filter Filter
 | NotFilter Filter
 | PureFilter Bool

matchesFilter :: Filter -> ClosurePtr -> SizedClosure -> [ClosurePtr] -> DebugM Bool
matchesFilter filter ptr sc parents = case filter of
  ConstructorDescFilter p -> case noSize sc of
    ConstrClosure _ _ _ _ cd -> do
      cd' <- dereferenceConDesc cd
      return $ p cd'
    _ -> pure False
  InfoFilter p -> pure $ p (decodedTable (info (noSize sc)))
  InfoPtrFilter p -> pure $ p (tableId (info (noSize sc)))
  InfoSourceFilter p -> do
    loc <- getSourceInfo (tableId (info (noSize sc)))
    case loc of
      Nothing -> return False
      Just cur_loc -> pure $ p cur_loc
  SizeFilter p -> pure $ p (dcSize sc)
  ProfHeaderFilter p -> pure $ p (profHeader $ noSize sc)
  AddressFilter p -> pure $ p ptr
  ParentFilter idx f -> case drop idx parents of
    [] -> pure False
    (p:rest) -> do
      sc_p <- dereferenceClosure p
      matchesFilter f p sc_p rest
  SomeParentFilter within f -> do
    let to_consider = foldr (\x xs -> (x, map fst xs) : xs) []
                    $ maybe parents (`take` parents) within
    flip anyM to_consider $ \(cur,parents_cur) -> do
      sc_cur <- dereferenceClosure cur
      matchesFilter f cur sc_cur parents_cur
  AllParentFilter within f -> do
    let to_consider = foldr (\x xs -> (x, map fst xs) : xs) []
                    $ maybe parents (`take` parents) within
    flip allM to_consider $ \(cur,parents_cur) -> do
      sc_cur <- dereferenceClosure cur
      matchesFilter f cur sc_cur parents_cur
  AndFilter f1 f2 -> do
    r1 <- matchesFilter f1 ptr sc parents
    case r1 of
      False -> pure False
      True -> matchesFilter f2 ptr sc parents
  OrFilter f1 f2 -> do
    r1 <- matchesFilter f1 ptr sc parents
    case r1 of
      True -> pure True
      False -> matchesFilter f2 ptr sc parents
  NotFilter f1  -> do
    r1 <- matchesFilter f1 ptr sc parents
    pure (not r1)
  PureFilter b -> pure b

findRetainersOf :: Maybe Int
                -> [ClosurePtr]
                -> [ClosurePtr]
                -> DebugM [[ClosurePtr]]
findRetainersOf limit cps bads =
  findRetainers limit (AddressFilter (`Set.member` bad_set)) cps
  where
    bad_set = Set.fromList bads

findRetainersOfConstructor :: Maybe Int
                           -> [ClosurePtr] -> String -> DebugM [[ClosurePtr]]
findRetainersOfConstructor limit rroots con_name =
  findRetainers limit (ConstructorDescFilter ((== con_name) . name)) rroots

findRetainersOfConstructorExact
  :: Maybe Int
  -> [ClosurePtr] -> String -> DebugM [[ClosurePtr]]
findRetainersOfConstructorExact limit rroots clos_name =
  findRetainers limit (InfoSourceFilter ((== clos_name) . infoName)) rroots

findRetainersOfEra
  :: Maybe Int
  -> EraRange
  -> [ClosurePtr] -> DebugM [[ClosurePtr]]
findRetainersOfEra limit eras rroots =
  findRetainers limit filter rroots
  where
    filter = ProfHeaderFilter (`profHeaderInEraRange` (Just eras))

findRetainersOfArrWords
  :: Maybe Int
  -> [ClosurePtr] -> Size -> DebugM [[ClosurePtr]]
findRetainersOfArrWords limit rroots lim =
  findRetainers limit filter rroots
  where
    -- TODO : this is the size of the entire closure, not the size of the ArrWords
    filter = AndFilter (InfoFilter ((== ARR_WORDS) . tipe))
                       (SizeFilter (>= lim))

findRetainersOfInfoTable
  :: Maybe Int
  -> [ClosurePtr] -> InfoTablePtr -> DebugM [[ClosurePtr]]
findRetainersOfInfoTable limit rroots info_ptr =
  findRetainers limit (InfoPtrFilter (== info_ptr)) rroots

-- | From the given roots, find any path to one of the given pointers.
-- Note: This function can be quite slow! The first argument is a limit to
-- how many paths to find. You should normally set this to a small number
-- such as 10.
findRetainers :: Maybe Int
  -> Filter
  -> [ClosurePtr] -> DebugM [[ClosurePtr]]
findRetainers limit filter rroots = (\(_, r, _) -> snd r) <$> runRWST (traceFromM funcs rroots) [] (limit, [])
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
              , srtTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedVal = const (return ())
              , conDescTrace = const (return ())

            }
    -- Add clos
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> RWST [ClosurePtr] () (Maybe Int, [[ClosurePtr]]) DebugM ()
               -> RWST [ClosurePtr] () (Maybe Int, [[ClosurePtr]]) DebugM ()
    closAccum _ (noSize -> WeakClosure {}) _ = return ()
    closAccum cp sc k = do
      ctx <- ask
      b <- lift $ matchesFilter filter cp sc ctx
      if b
      then do
        modify' (addOne (cp: ctx))
        local (cp:) k
      else do
        (lim, _) <- get
        case lim of
          Just 0 -> return ()
          _ -> local (cp:) k

addLocationToStack :: [ClosurePtr] -> DebugM [(SizedClosureP, Maybe SourceInformation)]
addLocationToStack r = do
  cs <- dereferenceClosures r
  cs' <- mapM dereferenceToClosurePtr cs
  locs <- mapM getSourceLoc cs'
  return $ (zip cs' locs)
  where
    getSourceLoc c = getSourceInfo (tableId (info (noSize c)))

addLocationToStack' :: [ClosurePtr] -> DebugM [(ClosurePtr, SizedClosureP, Maybe SourceInformation)]
addLocationToStack' r = do
  cs <- dereferenceClosures r
  cs' <- mapM dereferenceToClosurePtr cs
  locs <- mapM getSourceLoc cs'
  return $ (zip3 r cs' locs)
  where
    getSourceLoc c = getSourceInfo (tableId (info (noSize c)))

displayRetainerStack :: [(String, [(SizedClosureP, Maybe SourceInformation)])] -> IO ()
displayRetainerStack rs = do
      let disp (d, l) =
            (ppClosure  (\_ -> show) 0 . noSize $ d) ++  " <" ++ maybe "nl" tdisplay l ++ ">"
            where
              tdisplay sl = infoName sl ++ ":" ++ infoType sl ++ ":" ++ infoModule sl ++ ":" ++ infoPosition sl
          do_one k (l, stack) = do
            putStrLn (show k ++ "-------------------------------------")
            print l
            mapM (putStrLn . disp) stack
      zipWithM_ do_one [0 :: Int ..] rs

displayRetainerStack' :: [(String, [(ClosurePtr, SizedClosureP, Maybe SourceInformation)])] -> IO ()
displayRetainerStack' rs = do
      let disp (p, d, l) =
            show p ++ ": " ++ (ppClosure  (\_ -> show) 0 . noSize $ d) ++  " <" ++ maybe "nl" tdisplay l ++ ">"
            where
              tdisplay sl = infoName sl ++ ":" ++ infoType sl ++ ":" ++ infoModule sl ++ ":" ++ infoPosition sl
          do_one k (l, stack) = do
            putStrLn (show k ++ "-------------------------------------")
            print l
            mapM (putStrLn . disp) stack
      zipWithM_ do_one [0 :: Int ..] rs
