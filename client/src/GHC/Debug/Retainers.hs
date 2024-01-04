{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Functions for computing retainers
module GHC.Debug.Retainers(findRetainersOf, findRetainersOfConstructor, findRetainersOfConstructorExact, findRetainersOfInfoTable, findRetainers, addLocationToStack, displayRetainerStack, addLocationToStack', displayRetainerStack', findRetainersOfArrWords, EraRange(..), findRetainersOfEra) where

import GHC.Debug.Client
import Control.Monad.State
import GHC.Debug.Trace
import GHC.Debug.Types.Graph
import Control.Monad

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

findRetainersOf :: Maybe Int
                -> Maybe EraRange
                -> [ClosurePtr]
                -> [ClosurePtr]
                -> DebugM [[ClosurePtr]]
findRetainersOf limit eras cps bads = findRetainers limit eras cps (\cp _ -> return (cp `Set.member` bad_set))
  where
    bad_set = Set.fromList bads

findRetainersOfConstructor :: Maybe Int
                           -> Maybe EraRange
                           -> [ClosurePtr] -> String -> DebugM [[ClosurePtr]]
findRetainersOfConstructor limit eras rroots con_name =
  findRetainers limit eras rroots go
  where
    go _ sc =
      case noSize sc of
        ConstrClosure _ _ _ _ cd -> do
          ConstrDesc _ _  cname <- dereferenceConDesc cd
          return $ cname == con_name
        _ -> return $ False

findRetainersOfConstructorExact
  :: Maybe Int
  -> Maybe EraRange
  -> [ClosurePtr] -> String -> DebugM [[ClosurePtr]]
findRetainersOfConstructorExact limit eras rroots clos_name =
  findRetainers limit eras rroots go
  where
    go _ sc = do
      loc <- getSourceInfo (tableId (info (noSize sc)))
      case loc of
        Nothing -> return False
        Just cur_loc ->

          return $ (infoName cur_loc) == clos_name

findRetainersOfEra
  :: Maybe Int
  -> EraRange
  -> [ClosurePtr] -> DebugM [[ClosurePtr]]
findRetainersOfEra limit eras rroots =
  findRetainers limit (Just eras) rroots go
  where
    go _ _ = return True

findRetainersOfArrWords
  :: Maybe Int
  -> Maybe EraRange
  -> [ClosurePtr] -> Word -> DebugM [[ClosurePtr]]
findRetainersOfArrWords limit eras rroots lim =
  findRetainers limit eras rroots go
  where
    go _ sc = do
      case noSize sc of
        ArrWordsClosure {..} -> return $ bytes >= lim
        _ -> return False

findRetainersOfInfoTable
  :: Maybe Int
  -> Maybe EraRange
  -> [ClosurePtr] -> InfoTablePtr -> DebugM [[ClosurePtr]]
findRetainersOfInfoTable limit eras rroots info_ptr =
  findRetainers limit eras rroots go
  where
    go _ sc = return $ tableId (info (noSize sc)) == info_ptr

-- | From the given roots, find any path to one of the given pointers.
-- Note: This function can be quite slow! The first argument is a limit to
-- how many paths to find. You should normally set this to a small number
-- such as 10.
findRetainers :: Maybe Int
  -> Maybe EraRange
  -> [ClosurePtr] -> (ClosurePtr -> SizedClosure -> DebugM Bool) -> DebugM [[ClosurePtr]]
findRetainers limit eras rroots p = (\(_, r, _) -> snd r) <$> runRWST (traceFromM funcs rroots) [] (limit, [])
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
      b <- lift $ p cp sc
      if (b && (profHeader $ noSize sc) `profHeaderInEraRange` eras)
        then do
          ctx <- ask
          modify' (addOne (cp: ctx))
          local (cp:) k
          -- Don't call k, there might be more paths to the pointer but we
          -- probably just care about this first one.
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
