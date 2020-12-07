{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
module GHC.Debug.Types.Graph where

import Data.Char
import Data.List
import Data.Maybe       ( catMaybes, fromJust )
import Data.Function
import qualified Data.Traversable as T
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import GHC.Debug.Types.Ptr
import GHC.Debug.Types.Closures
import Control.Applicative
import Data.Functor.Compose
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Foldable as F
import qualified Data.Graph.Dom as DO
import qualified Data.Tree as Tree
import Debug.Trace

-- | For heap graphs, i.e. data structures that also represent sharing and
-- cyclic structures, these are the entries. If the referenced value is
-- @Nothing@, then we do not have that value in the map, most likely due to
-- exceeding the recursion bound passed to 'buildHeapGraph'.
--
-- Besides a pointer to the stored value and the closure representation we
-- also keep track of whether the value was still alive at the last update of the
-- heap graph. In addition we have a slot for arbitrary data, for the user's convenience.
data HeapGraphEntry a = HeapGraphEntry {
        hgeClosurePtr :: ClosurePtr,
        hgeClosure :: DebugClosure ConstrDesc StackHI (Maybe HeapGraphIndex),
        hgeData :: a}
    deriving (Show, Functor, Foldable, Traversable)
type HeapGraphIndex = ClosurePtr

type StackHI = GenStack (Maybe HeapGraphIndex)

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

heapGraphSize (HeapGraph _ g) = IM.size g

-- | Creates a 'HeapGraph' for the value in the box, but not recursing further
-- than the given limit. The initial value has index 'heapGraphRoot'.
buildHeapGraph
   :: (Monad m)
   => DerefFunction m a
   -> Maybe Int -- ^ Search limit
   -> ClosurePtr -- ^ The value to start with
   -> m (HeapGraph a)
buildHeapGraph deref limit initialBox =
    fst <$> multiBuildHeapGraph deref limit (NE.singleton initialBox)

type DerefFunction m a = ClosurePtr -> m (DebugClosureWithExtra a ConstrDesc Stack ClosurePtr)

-- | Creates a 'HeapGraph' for the values in multiple boxes, but not recursing
--   further than the given limit.
--
--   Returns the 'HeapGraph' and the indices of initial values. The arbitrary
--   type @a@ can be used to make the connection between the input and the
--   resulting list of indices, and to store additional data.
multiBuildHeapGraph
    :: (Monad m)
    => DerefFunction m a
    -> Maybe Int -- ^ Search limit
    -> NonEmpty ClosurePtr -- ^ Starting values with associated data entry
    -> m (HeapGraph a, NonEmpty HeapGraphIndex)
multiBuildHeapGraph deref limit rs = generalBuildHeapGraph deref (HeapGraph rs IM.empty) rs
{-# INLINE multiBuildHeapGraph #-}

-- | Adds an entry to an existing 'HeapGraph'.
--
--   Returns the updated 'HeapGraph' and the index of the added value.
addHeapGraph
    :: (Monad m)
    => DerefFunction m a
    -> Maybe Int -- ^ Search limit
    -> a -- ^ Data to be stored with the added value
    -> ClosurePtr -- ^ Value to add to the graph
    -> HeapGraph a -- ^ Graph to extend
    -> m (HeapGraphIndex, HeapGraph a)
addHeapGraph deref limit d box hg = do
    (hg', (NE.head -> i)) <- generalBuildHeapGraph deref hg (NE.singleton box)
    return (i, hg')
{-# INLINABLE addHeapGraph #-}

-- | Adds the given annotation to the entry at the given index, using the
-- 'mappend' operation of its 'Monoid' instance.
annotateHeapGraph ::  (a -> a) -> HeapGraphIndex -> HeapGraph a -> HeapGraph a
annotateHeapGraph f i hg = updateHeapGraph go i hg
  where
    go hge = Just $ hge { hgeData = f (hgeData hge) }

{-# INLINE generalBuildHeapGraph #-}
generalBuildHeapGraph
    :: (Monad m)
    => DerefFunction m a
    -- -> Maybe Int
    -> HeapGraph a
    -> NonEmpty ClosurePtr
    -> m (HeapGraph a, NonEmpty HeapGraphIndex)
--generalBuildHeapGraph _deref (Just limit) _ _ | limit <= 0 = error "buildHeapGraph: limit has to be positive"
generalBuildHeapGraph deref hg addBoxes = do
    -- First collect all boxes from the existing heap graph
    (is, hg') <- runStateT (mapM add addBoxes) hg
    return (hg', fromJust <$> is)
  where
    add cp = do
        -- If the box is in the map, return the index
        hm <- get
        case lookupHeapGraph cp hm of
            Just {} -> return (Just cp)
            Nothing -> do
                -- Look up the closure
                c <- lift $ deref cp
                DCS e c' <- tritraverse pure (traverse add) add c
                -- Add add the resulting closure to the map
                modify' (insertHeapGraph cp (HeapGraphEntry cp c' e))
                return (Just cp)

-- | Pretty-prints a HeapGraph. The resulting string contains newlines. Example
-- for @let s = \"Ki\" in (s, s, cycle \"Ho\")@:
--
-- >let x1 = "Ki"
-- >    x6 = C# 'H' : C# 'o' : x6
-- >in (x1,x1,x6)
ppHeapGraph :: (a -> String) -> HeapGraph a -> String
ppHeapGraph printData (HeapGraph (heapGraphRoot :| rs) m) = letWrapper ++ "(" ++ printData (hgeData (iToE (rs !! 0))) ++ ") " ++ ppRef 0 (Just heapGraphRoot)
  where
    -- All variables occuring more than once
    bindings = boundMultipleTimes (HeapGraph (heapGraphRoot :| rs) m) [heapGraphRoot]

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
        | Just s <- isString hge = show s
        | Just l <- isList hge   = "[" ++ intercalate "," (map (ppRef 0) l) ++ "]"
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

    isList :: HeapGraphEntry a -> Maybe [Maybe HeapGraphIndex]
    isList hge
        | isNil (hgeClosure hge) =
            return []
        | otherwise = do
            (h,t) <- isCons (hgeClosure hge)
            ti <- t
            e <- iToUnboundE ti
            t' <- isList e
            return $ (:) h t'

    isString :: HeapGraphEntry a -> Maybe String
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
boundMultipleTimes (HeapGraph rs m) roots = map head $ filter (not.null) $ map tail $ group $ sort $
     roots ++ concatMap (catMaybes . allClosures . hgeClosure) (IM.elems m)

-- Utilities

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _p [] = return Nothing
findM p (x:xs) = do
    b <- p x
    if b then return (Just x) else findM p xs

addBraces :: Bool -> String -> String
addBraces True t = "(" ++ t ++ ")"
addBraces False t = t

braceize :: [String] -> String
braceize [] = ""
braceize xs = "{" ++ intercalate "," xs ++ "}"

isChar :: DebugClosure ConstrDesc s c -> Maybe Char
isChar ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = "C#"}, dataArgs = [ch], ptrArgs = []} = Just (chr (fromIntegral ch))
isChar _ = Nothing

isNil :: DebugClosure ConstrDesc s c -> Bool
isNil ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = "[]"}, dataArgs = _, ptrArgs = []} = True
isNil _ = False

isCons :: DebugClosure ConstrDesc s c -> Maybe (c, c)
isCons ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = ":"}, dataArgs = [], ptrArgs = [h,t]} = Just (h,t)
isCons _ = Nothing

isTup :: DebugClosure ConstrDesc s c -> Maybe [c]
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
ppClosure :: String -> (Int -> c -> String) -> Int -> DebugClosure ConstrDesc s c -> String
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
        fun : payload
    PAPClosure {..} -> app $ map (showBox 10) $
        fun : payload
    APStackClosure {..} -> app $ map (showBox 10) $
        fun : payload
    BCOClosure {..} -> app
        ["_bco", showBox 10 bcoptrs]
    ArrWordsClosure {..} -> app
        ["toArray", "("++show (length arrWords) ++ " words)", intercalate "," (shorten (map show arrWords)) ]
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
    IntClosure {..} -> app
        ["Int", show intVal]
    WordClosure {..} -> app
        ["Word", show wordVal]
    Int64Closure {..} -> app
        ["Int64", show int64Val]
    Word64Closure {..} -> app
        ["Word64", show word64Val]
    AddrClosure {..} -> app
        ["Addr", show addrVal]
    FloatClosure {..} -> app
        ["Float", show floatVal]
    DoubleClosure {..} -> app
        ["Double", show doubleVal]
    OtherClosure {} ->
        "_other"
    TSOClosure {..} -> "TSO"
    WeakClosure {..} -> "_wk"
    UnsupportedClosure {} ->
        "_unsupported"


  where
    app [a] = a  ++ "()"
    app xs = addBraces (10 <= prec) (unwords xs)

    shorten xs = if length xs > 20 then take 20 xs ++ ["(and more)"] else xs


closurePtrToInt :: ClosurePtr -> Int
closurePtrToInt (ClosurePtr p) = fromIntegral p

intToClosurePtr :: Int -> ClosurePtr
intToClosurePtr i = ClosurePtr (fromIntegral i)

convertToDom :: HeapGraph a -> DO.Rooted
convertToDom  (HeapGraph roots is) = (0, graph)
  where
    rootNodes = IS.fromList (map closurePtrToInt (NE.toList roots))
    graph = IM.insert 0 rootNodes (IM.foldlWithKey' collectNodes IM.empty is)
    collectNodes newMap k h =  IM.insert k (IS.fromList (map closurePtrToInt (catMaybes (allClosures (hgeClosure h))))) newMap

computeDominators :: HeapGraph a -> [Tree.Tree (HeapGraphEntry a)]
computeDominators hg = map (fmap (fromJust . flip lookupHeapGraph hg . intToClosurePtr)) entries
  where
    entries = case DO.domTree (convertToDom hg) of
                Tree.Node 0 es -> es

retainerSize :: HeapGraph Size -> [Tree.Tree (HeapGraphEntry (Size, RetainerSize))]
retainerSize hg = map bottomUpSize doms
  where
    doms = computeDominators hg

annotateWithRetainerSize :: HeapGraph Size -> HeapGraph (Size, RetainerSize)
annotateWithRetainerSize h@(HeapGraph rs _) =
  HeapGraph rs (foldMap convertToHeapGraph (retainerSize h))

bottomUpSize :: Tree.Tree (HeapGraphEntry Size) -> Tree.Tree (HeapGraphEntry (Size, RetainerSize))
bottomUpSize (Tree.Node rl sf) =
  let ts = map bottomUpSize sf
      s'@(Size s) =  hgeData rl
      RetainerSize children_size = foldMap (snd . hgeData . Tree.rootLabel) ts
      inclusive_size :: RetainerSize
      !inclusive_size = RetainerSize  (s + children_size)
      rl' = rl { hgeData = (s', inclusive_size) }
  in Tree.Node rl' ts

convertToHeapGraph ::  Tree.Tree (HeapGraphEntry a) -> IM.IntMap (HeapGraphEntry a)
convertToHeapGraph t = IM.fromList ([(fromIntegral cp, c) | c <- F.toList t, let ClosurePtr cp = hgeClosurePtr c ])

