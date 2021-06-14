{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
module GHC.Debug.Types.Graph( -- * Types
                              HeapGraph(..)
                            , HeapGraphEntry(..)
                            , HeapGraphIndex
                            , PapHI
                            , StackHI
                            -- * Building a heap graph
                            , DerefFunction
                            , buildHeapGraph
                            , multiBuildHeapGraph
                            , generalBuildHeapGraph

                            -- * Printing a heap graph
                            , ppHeapGraph
                            , ppClosure

                            -- * Utility
                            , lookupHeapGraph
                            , traverseHeapGraph
                            , updateHeapGraph
                            , heapGraphSize
                            , annotateHeapGraph

                            -- * Reverse Graph
                            , ReverseGraph
                            , mkReverseGraph
                            , reverseEdges
                            )
                            where

import Data.Char
import Data.List (intercalate, foldl', sort, group, sortBy, groupBy)
import Data.Maybe       ( catMaybes )
import Data.Function
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import GHC.Debug.Types.Ptr
import GHC.Debug.Types.Closures
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

-- | For heap graphs, i.e. data structures that also represent sharing and
-- cyclic structures, these are the entries. If the referenced value is
-- @Nothing@, then we do not have that value in the map, most likely due to
-- exceeding the recursion bound passed to 'buildHeapGraph'.
--
-- Besides a pointer to the stored value and the closure representation we
-- have a slot for arbitrary data, for the user's convenience.
data HeapGraphEntry a = HeapGraphEntry {
        hgeClosurePtr :: ClosurePtr,
        hgeClosure :: DebugClosure PapHI ConstrDesc StackHI (Maybe HeapGraphIndex),
        hgeData :: a}
    deriving (Show, Functor, Foldable, Traversable)
type HeapGraphIndex = ClosurePtr

type StackHI = GenStackFrames (Maybe HeapGraphIndex)
type PapHI =  GenPapPayload (Maybe HeapGraphIndex)

-- | The whole graph. The suggested interface is to only use 'lookupHeapGraph',
-- as the internal representation may change. Nevertheless, we export it here:
-- Sometimes the user knows better what he needs than we do.
data HeapGraph a = HeapGraph
                      { roots :: !(NE.NonEmpty ClosurePtr)
                      , graph :: !(IM.IntMap (HeapGraphEntry a)) }
    deriving (Show, Foldable, Traversable, Functor)

traverseHeapGraph :: Applicative m =>
                    (HeapGraphEntry a -> m (HeapGraphEntry b))
                  -> HeapGraph a
                  -> m (HeapGraph b)
traverseHeapGraph f (HeapGraph r im) = HeapGraph r <$> traverse f im


lookupHeapGraph :: HeapGraphIndex -> HeapGraph a -> Maybe (HeapGraphEntry a)
lookupHeapGraph (ClosurePtr i) (HeapGraph _r m) = IM.lookup (fromIntegral i) m

insertHeapGraph :: HeapGraphIndex -> HeapGraphEntry a -> HeapGraph a -> HeapGraph a
insertHeapGraph (ClosurePtr i) a (HeapGraph r m) = HeapGraph r (IM.insert (fromIntegral i) a m)

updateHeapGraph :: (HeapGraphEntry a -> Maybe (HeapGraphEntry a))
                -> HeapGraphIndex
                -> HeapGraph a
                -> HeapGraph a
updateHeapGraph f (ClosurePtr i) (HeapGraph r m) = HeapGraph r (IM.update f (fromIntegral i) m)

heapGraphSize :: HeapGraph a -> Int
heapGraphSize (HeapGraph _ g) = IM.size g

-- | Creates a 'HeapGraph' for the value in the box, but not recursing further
-- than the given limit.
buildHeapGraph
   :: (MonadFix m)
   => DerefFunction m a
   -> Maybe Int
   -> ClosurePtr -- ^ The value to start with
   -> m (HeapGraph a)
buildHeapGraph deref limit initialBox =
  multiBuildHeapGraph deref limit (NE.singleton initialBox)

-- TODO: It is a bit undesirable that the ConstrDesc field is already
-- dereferenced, but also, not such a big deal. It could lead to additional
-- requests to the debuggee which are not necessary and causes a mismatch
-- with the step-by-step decoding functions in `Client.hs`
type DerefFunction m a = ClosurePtr -> m (DebugClosureWithExtra a PapPayload ConstrDesc StackFrames ClosurePtr)

-- | Creates a 'HeapGraph' for the values in multiple boxes, but not recursing
--   further than the given limit.
multiBuildHeapGraph
    :: (MonadFix m)
    => DerefFunction m a
    -> Maybe Int
    -> NonEmpty ClosurePtr -- ^ Starting values with associated data entry
    -> m (HeapGraph a)
multiBuildHeapGraph deref limit rs =
  generalBuildHeapGraph deref limit (HeapGraph rs IM.empty) rs
{-# INLINE multiBuildHeapGraph #-}

-- | Adds the given annotation to the entry at the given index, using the
-- 'mappend' operation of its 'Monoid' instance.
annotateHeapGraph ::  (a -> a) -> HeapGraphIndex -> HeapGraph a -> HeapGraph a
annotateHeapGraph f i hg = updateHeapGraph go i hg
  where
    go hge = Just $ hge { hgeData = f (hgeData hge) }

{-# INLINE generalBuildHeapGraph #-}
generalBuildHeapGraph
    :: forall m a .  (MonadFix m)
    => DerefFunction m a
    -> Maybe Int
    -> HeapGraph a
    -> NonEmpty ClosurePtr
    -> m (HeapGraph a)
generalBuildHeapGraph deref limit hg addBoxes = do
    -- First collect all boxes from the existing heap graph
    (_is, hg') <- runStateT (mapM (add limit) addBoxes) hg
    return hg'
  where
    add :: Maybe Int -> ClosurePtr -> StateT (HeapGraph a) m (Maybe ClosurePtr)
    add (Just 0) _ = return Nothing
    add n cp = do
        -- If the box is in the map, return the index
        hm <- get
        case lookupHeapGraph cp hm of
            Just {} -> return (Just cp)
            -- FIXME GHC BUG: change `mdo` to `do` below:
            --       "GHC internal error: ‘c’ is not in scope during type checking, but it passed the renamer"
            Nothing -> mdo
                -- Look up the closure
                c <- lift $ deref cp
                let new_add = add (subtract 1 <$> n)
                -- NOTE: We tie-the-knot here with RecursiveDo so that we don't
                -- get into an infinite loop with cycles in the heap.
                rec modify' (insertHeapGraph cp (HeapGraphEntry cp c' e))
                    -- Add the resulting closure below to the map (above):
                    DCS e c' <- quadtraverse (traverse new_add) pure (traverse new_add) new_add c
                return (Just cp)

-- | Pretty-prints a HeapGraph. The resulting string contains newlines. Example
-- for @let s = \"Ki\" in (s, s, cycle \"Ho\")@:
--
-- >let x1 = "Ki"
-- >    x6 = C# 'H' : C# 'o' : x6
-- >in (x1,x1,x6)
ppHeapGraph :: (a -> String) -> HeapGraph a -> String
ppHeapGraph printData (HeapGraph (heapGraphRoot :| rs) m) = letWrapper ++ "(" ++ printData (hgeData (iToE heapGraphRoot)) ++ ") " ++ roots
  where
    -- All variables occuring more than once
    bindings = boundMultipleTimes (HeapGraph (heapGraphRoot :| rs) m) [heapGraphRoot]

    roots = unlines [
              "r" ++ show n ++ ":(" ++ printData (hgeData (iToE r)) ++ ") " ++ ppRef 0 (Just r) ++ "\n"
              | (n, r) <- zip [0 :: Int ..] (heapGraphRoot : rs) ]

    letWrapper =
        if null bindings
        then ""
        else "let " ++ intercalate "\n    " (map ppBinding bindings) ++ "\nin "

    bindingLetter i = case hgeClosure (iToE i) of
        ThunkClosure {} -> 't'
        SelectorClosure {} -> 't'
        APClosure {} -> 't'
        PAPClosure {} -> 'f'
        BCOClosure {} -> 't'
        FunClosure {} -> 'f'
        _ -> 'x'

    ppBindingMap = M.fromList $
        concatMap (zipWith (\j (i,c) -> (i, c : show j)) [(1::Int)..]) $
        groupBy ((==) `on` snd) $
        sortBy (compare `on` snd)
        [ (i, bindingLetter i) | i <- bindings ]

    ppVar i = ppBindingMap M.! i
    ppBinding i = ppVar i ++ "(" ++ printData (hgeData (iToE i)) ++  ") = " ++ ppEntry 0 (iToE i)

    ppEntry prec hge
        | Just s <- isString (hgeClosure hge) = show s
        | Just l <- isList (hgeClosure hge)   = "[" ++ intercalate "," (map (ppRef 0) l) ++ "]"
        | otherwise = ppClosure (printData (hgeData hge)) ppRef prec (hgeClosure hge)
      where
        _app [a] = a  ++ "()"
        _app xs = addBraces (10 <= prec) (unwords xs)

    ppRef _ Nothing = "..."
    ppRef prec (Just i) | i `elem` bindings = ppVar i
                        | otherwise = ppEntry prec (iToE i)
    iToE (ClosurePtr i) = m IM.! (fromIntegral i)

    iToUnboundE cp@(ClosurePtr i)
        | cp `elem` bindings = Nothing
        | otherwise         = IM.lookup (fromIntegral i) m

    isList :: DebugClosure p ConstrDesc s (Maybe HeapGraphIndex) -> Maybe [Maybe HeapGraphIndex]
    isList c
        | isNil c =
            return []
        | otherwise = do
            (h,t) <- isCons c
            ti <- t
            e <- iToUnboundE ti
            t' <- isList (hgeClosure e)
            return $ (:) h t'

    isString :: DebugClosure p ConstrDesc s (Maybe HeapGraphIndex) -> Maybe String
    isString e = do
        list <- isList e
        -- We do not want to print empty lists as "" as we do not know that they
        -- are really strings.
        if null list
        then Nothing
        else mapM (isChar . hgeClosure <=< iToUnboundE <=< id) list


-- | In the given HeapMap, list all indices that are used more than once. The
-- second parameter adds external references, commonly @[heapGraphRoot]@.
boundMultipleTimes :: HeapGraph a -> [HeapGraphIndex] -> [HeapGraphIndex]
boundMultipleTimes (HeapGraph _rs m) roots = map head $ filter (not.null) $ group $ sort $
     roots ++ concatMap (catMaybes . allClosures . hgeClosure) (IM.elems m)

-- Utilities

addBraces :: Bool -> String -> String
addBraces True t = "(" ++ t ++ ")"
addBraces False t = t

braceize :: [String] -> String
braceize [] = ""
braceize xs = "{" ++ intercalate "," xs ++ "}"

isChar :: DebugClosure p ConstrDesc s c -> Maybe Char
isChar ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = "C#"}, dataArgs = [ch], ptrArgs = []} = Just (chr (fromIntegral ch))
isChar _ = Nothing

isNil :: DebugClosure p ConstrDesc s c -> Bool
isNil ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = "[]"}, dataArgs = _, ptrArgs = []} = True
isNil _ = False

isCons :: DebugClosure p ConstrDesc s c -> Maybe (c, c)
isCons ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = ":"}, dataArgs = [], ptrArgs = [h,t]} = Just (h,t)
isCons _ = Nothing

isTup :: DebugClosure p ConstrDesc s c -> Maybe [c]
isTup ConstrClosure{ dataArgs = [], ..} =
    if length (name constrDesc) >= 3 &&
       head (name constrDesc) == '(' && last (name constrDesc) == ')' &&
       all (==',') (tail (init (name constrDesc)))
    then Just ptrArgs else Nothing
isTup _ = Nothing



-- | A pretty-printer that tries to generate valid Haskell for evalutated data.
-- It assumes that for the included boxes, you already replaced them by Strings
-- using 'Data.Foldable.map' or, if you need to do IO, 'Data.Foldable.mapM'.
--
-- The parameter gives the precedendence, to avoid avoidable parenthesises.
ppClosure :: String -> (Int -> c -> String) -> Int -> DebugClosure p ConstrDesc s c -> String
ppClosure herald showBox prec c = case c of
    _ | Just ch <- isChar c -> app
        ["C#", show ch]
    _ | Just (h,t) <- isCons c -> addBraces (5 <= prec) $
        showBox 5 h ++ " : " ++ showBox 4 t
    _ | Just vs <- isTup c ->
        "(" ++ intercalate "," (map (showBox 0) vs) ++ ")"
    ConstrClosure {..} -> app $
        name constrDesc : map (showBox 10) ptrArgs ++ map show dataArgs
    ThunkClosure {..} -> app $
        "_thunk(" : herald : ")" : map (showBox 10) ptrArgs ++ map show dataArgs
    SelectorClosure {..} -> app
        ["_sel", showBox 10 selectee]
    IndClosure {..} -> app
        ["_ind", showBox 10 indirectee]
    BlackholeClosure {..} -> app
        ["_bh",  showBox 10 indirectee]
    APClosure {..} -> app $ map (showBox 10) $
        [fun]
        -- TODO: Payload
    PAPClosure {..} -> app $ map (showBox 10) $
        [fun] -- TODO payload
    APStackClosure {..} -> app $ map (showBox 10) $
        [fun] -- TODO: stack
    TRecChunkClosure {} -> "_trecChunk" --TODO
    BCOClosure {..} -> app
        ["_bco", showBox 10 bcoptrs]
    ArrWordsClosure {..} -> app
        ["ARR_WORDS", "("++show bytes ++ " bytes)", ((show $ arrWordsBS arrWords)) ]
    MutArrClosure {..} -> app
        --["toMutArray", "("++show (length mccPayload) ++ " ptrs)",  intercalate "," (shorten (map (showBox 10) mccPayload))]
        ["[", intercalate ", " (shorten (map (showBox 10) mccPayload)),"]"]
    SmallMutArrClosure {..} -> app
        ["[", intercalate ", " (shorten (map (showBox 10) mccPayload)),"]"]
    MutVarClosure {..} -> app
        ["_mutVar", showBox 10 var]
    MVarClosure {..} -> app
        ["MVar", showBox 10 value]
    FunClosure {..} ->
        "_fun" ++ braceize (map (showBox 0) ptrArgs ++ map show dataArgs)
    BlockingQueueClosure {} ->
        "_blockingQueue"
    OtherClosure {} ->
        "_other"
    TSOClosure {} -> "TSO"
    StackClosure {..} -> app ["Stack(", show stack_size, ")"] -- TODO
    WeakClosure {} -> "_wk" -- TODO
    TVarClosure {} -> "_tvar" -- TODO
    MutPrimClosure {} -> "_mutPrim" -- TODO
    UnsupportedClosure {info} -> (show info)


  where
    app [a] = a  ++ "()"
    app xs = addBraces (10 <= prec) (unwords xs)

    shorten xs = if length xs > 20 then take 20 xs ++ ["(and more)"] else xs


-- Reverse Edges
--
closurePtrToInt :: ClosurePtr -> Int
closurePtrToInt (ClosurePtr p) = fromIntegral p

intToClosurePtr :: Int -> ClosurePtr
intToClosurePtr i = mkClosurePtr (fromIntegral i)

newtype ReverseGraph = ReverseGraph (IM.IntMap IS.IntSet)

reverseEdges :: ClosurePtr -> ReverseGraph -> Maybe [ClosurePtr]
reverseEdges cp (ReverseGraph rg) =
  map intToClosurePtr . IS.toList <$> IM.lookup (closurePtrToInt cp) rg

mkReverseGraph :: HeapGraph a -> ReverseGraph
mkReverseGraph (HeapGraph _ hg) = ReverseGraph graph
  where
    graph = IM.foldlWithKey' collectNodes IM.empty hg
    collectNodes newMap k h =
      let bs = allClosures (hgeClosure h)
      in foldl' (\m ma ->
                    case ma of
                      Nothing -> m
                      Just a -> IM.insertWith IS.union (closurePtrToInt a) (IS.singleton k) m) newMap bs

