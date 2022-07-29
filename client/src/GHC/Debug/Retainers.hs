{-# LANGUAGE ViewPatterns #-}
-- | Functions for computing retainers
module GHC.Debug.Retainers(findRetainersOf, findRetainersOfConstructor, findRetainersOfConstructorExact, findRetainers, addLocationToStack, displayRetainerStack, addLocationToStack', displayRetainerStack') where

import GHC.Debug.Client
import Control.Monad.State
import GHC.Debug.Trace
import GHC.Debug.Types.Graph

import qualified Data.Set as Set
import Control.Monad.RWS

addOne :: [ClosurePtr] -> (Maybe Int, [[ClosurePtr]]) -> (Maybe Int, [[ClosurePtr]])
addOne _ (Just 0, cp) = (Just 0, cp)
addOne cp (n, cps)    = (subtract 1 <$> n, cp:cps)

findRetainersOf :: Maybe Int
                -> [ClosurePtr]
                -> [ClosurePtr]
                -> DebugM [[ClosurePtr]]
findRetainersOf limit cps bads = findRetainers limit cps (\cp _ -> return (cp `Set.member` bad_set))
  where
    bad_set = Set.fromList bads

findRetainersOfConstructor :: Maybe Int -> [ClosurePtr] -> String -> DebugM [[ClosurePtr]]
findRetainersOfConstructor limit roots con_name =
  findRetainers limit roots go
  where
    go cp sc =
      case noSize sc of
        ConstrClosure _ _ _ cd -> do
          ConstrDesc _ _  cname <- dereferenceConDesc cd
          return $ cname == con_name
        _ -> return $ False

findRetainersOfConstructorExact :: Maybe Int -> [ClosurePtr] -> String -> DebugM [[ClosurePtr]]
findRetainersOfConstructorExact limit roots clos_name =
  findRetainers limit roots go
  where
    go cp sc = do
      loc <- getSourceInfo (tableId (info (noSize sc)))
      case loc of
        Nothing -> return False
        Just loc ->

          return $ (infoName loc) == clos_name

-- | From the given roots, find any path to one of the given pointers.
-- Note: This function can be quite slow! The first argument is a limit to
-- how many paths to find. You should normally set this to a small number
-- such as 10.
findRetainers :: Maybe Int -> [ClosurePtr] -> (ClosurePtr -> SizedClosure -> DebugM Bool) -> DebugM [[ClosurePtr]]
findRetainers limit rroots p = (\(_, r, _) -> snd r) <$> runRWST (traceFromM funcs rroots) [] (limit, [])
  where
    funcs = TraceFunctions {
               papTrace = const (return ())
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
      if b
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

addLocationToStack :: [ClosurePtr] -> DebugM [(SizedClosureC, Maybe SourceInformation)]
addLocationToStack r = do
  cs <- dereferenceClosures r
  cs' <- mapM (quadtraverse pure dereferenceConDesc pure pure) cs
  locs <- mapM getSourceLoc cs'
  return $ (zip cs' locs)
  where
    getSourceLoc c = getSourceInfo (tableId (info (noSize c)))

addLocationToStack' :: [ClosurePtr] -> DebugM [(ClosurePtr, SizedClosureC, Maybe SourceInformation)]
addLocationToStack' r = do
  cs <- dereferenceClosures r
  cs' <- mapM (quadtraverse pure dereferenceConDesc pure pure) cs
  locs <- mapM getSourceLoc cs'
  return $ (zip3 r cs' locs)
  where
    getSourceLoc c = getSourceInfo (tableId (info (noSize c)))

displayRetainerStack :: [(String, [(SizedClosureC, Maybe SourceInformation)])] -> IO ()
displayRetainerStack rs = do
      let disp (d, l) =
            (ppClosure ""  (\_ -> show) 0 . noSize $ d) ++  " <" ++ maybe "nl" tdisplay l ++ ">"
            where
              tdisplay sl = infoName sl ++ ":" ++ infoType sl ++ ":" ++ infoModule sl ++ ":" ++ infoPosition sl
          do_one k (l, stack) = do
            putStrLn (show k ++ "-------------------------------------")
            print l
            mapM (putStrLn . disp) stack
      zipWithM_ do_one [0 :: Int ..] rs

displayRetainerStack' :: [(String, [(ClosurePtr, SizedClosureC, Maybe SourceInformation)])] -> IO ()
displayRetainerStack' rs = do
      let disp (p, d, l) =
            show p ++ ": " ++ (ppClosure ""  (\_ -> show) 0 . noSize $ d) ++  " <" ++ maybe "nl" tdisplay l ++ ">"
            where
              tdisplay sl = infoName sl ++ ":" ++ infoType sl ++ ":" ++ infoModule sl ++ ":" ++ infoPosition sl
          do_one k (l, stack) = do
            putStrLn (show k ++ "-------------------------------------")
            print l
            mapM (putStrLn . disp) stack
      zipWithM_ do_one [0 :: Int ..] rs
