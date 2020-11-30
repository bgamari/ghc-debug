{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module GHC.Debug.Types.Graph where

import Data.Char
import Data.List
import Data.Maybe       ( catMaybes, fromJust )
import Data.Function
import qualified Data.Traversable as T
import qualified Data.IntMap as M
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import GHC.Debug.Types.Ptr
import GHC.Debug.Types.Closures

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
        hgeLive :: Bool,
        hgeData :: a}
    deriving (Show, Functor, Foldable, Traversable)
type HeapGraphIndex = Int

type StackHI = GenStack (Maybe HeapGraphIndex)

-- | The whole graph. The suggested interface is to only use 'lookupHeapGraph',
-- as the internal representation may change. Nevertheless, we export it here:
-- Sometimes the user knows better what he needs than we do.
newtype HeapGraph a = HeapGraph (M.IntMap (HeapGraphEntry a))
    deriving (Show, Foldable, Traversable, Functor)

traverseHeapGraph :: Applicative m =>
                    (HeapGraphEntry a -> m (HeapGraphEntry b))
                  -> HeapGraph a
                  -> m (HeapGraph b)
traverseHeapGraph f (HeapGraph im) = HeapGraph <$> traverse f im


lookupHeapGraph :: HeapGraphIndex -> HeapGraph a -> Maybe (HeapGraphEntry a)
lookupHeapGraph i (HeapGraph m) = M.lookup i m

heapGraphRoot :: HeapGraphIndex
heapGraphRoot = 0

-- | Creates a 'HeapGraph' for the value in the box, but not recursing further
-- than the given limit. The initial value has index 'heapGraphRoot'.
buildHeapGraph
   :: (Monad m, Monoid a)
   => DerefFunction m
   -> Int -- ^ Search limit
   -> a -- ^ Data value for the root
   -> ClosurePtr -- ^ The value to start with
   -> m (HeapGraph a)
buildHeapGraph deref limit rootD initialBox =
    fst <$> multiBuildHeapGraph deref limit [(rootD, initialBox)]

type DerefFunction m = ClosurePtr -> m (DebugClosure ConstrDesc Stack ClosurePtr)

-- | Creates a 'HeapGraph' for the values in multiple boxes, but not recursing
--   further than the given limit.
--
--   Returns the 'HeapGraph' and the indices of initial values. The arbitrary
--   type @a@ can be used to make the connection between the input and the
--   resulting list of indices, and to store additional data.
multiBuildHeapGraph
    :: (Monad m, Monoid a)
    => DerefFunction m
    -> Int -- ^ Search limit
    -> [(a, ClosurePtr)] -- ^ Starting values with associated data entry
    -> m (HeapGraph a, [(a, HeapGraphIndex)])
multiBuildHeapGraph deref limit = generalBuildHeapGraph deref limit (HeapGraph M.empty)

-- | Adds an entry to an existing 'HeapGraph'.
--
--   Returns the updated 'HeapGraph' and the index of the added value.
addHeapGraph
    :: (Monoid a, Monad m)
    => DerefFunction m
    -> Int -- ^ Search limit
    -> a -- ^ Data to be stored with the added value
    -> ClosurePtr -- ^ Value to add to the graph
    -> HeapGraph a -- ^ Graph to extend
    -> m (HeapGraphIndex, HeapGraph a)
addHeapGraph deref limit d box hg = do
    (hg', (head -> (_,i))) <- generalBuildHeapGraph deref limit hg [(d,box)]
    return (i, hg')

-- | Adds the given annotation to the entry at the given index, using the
-- 'mappend' operation of its 'Monoid' instance.
annotateHeapGraph :: Monoid a => a -> HeapGraphIndex -> HeapGraph a -> HeapGraph a
annotateHeapGraph d i (HeapGraph hg) = HeapGraph $ M.update go i hg
  where
    go hge = Just $ hge { hgeData = hgeData hge <> d }

generalBuildHeapGraph
    :: (Monoid a, Monad m)
    => DerefFunction m
    -> Int
    -> HeapGraph a
    -> [(a,ClosurePtr)]
    -> m (HeapGraph a, [(a, HeapGraphIndex)])
generalBuildHeapGraph _deref limit _ _ | limit <= 0 = error "buildHeapGraph: limit has to be positive"
generalBuildHeapGraph deref limit (HeapGraph hg) addBoxes = do
    -- First collect all boxes from the existing heap graph
    let boxList = [ (hgeClosurePtr hge, i) | (i, hge) <- M.toList hg ]
        indices | M.null hg = [0..]
                | otherwise = [1 + fst (M.findMax hg)..]

        initialState = (boxList, indices, [])
    -- It is ok to use the Monoid (IntMap a) instance here, because
    -- we will, besides the first time, use 'tell' only to add singletons not
    -- already there
    (is, hg') <- runWriterT (evalStateT run initialState)
    -- Now add the annotations of the root values
    let hg'' = foldl' (flip (uncurry annotateHeapGraph)) (HeapGraph hg') is
    return (hg'', is)
  where
    run = do
        lift $ tell hg -- Start with the initial map
        forM addBoxes $ \(d, b) -> do
            -- Cannot fail, as limit is not zero here
            i <- fromJust <$> (add limit b)
            return (d, i)

    add 0  _ = return Nothing
    add n b = do
        -- If the box is in the map, return the index
        (existing,_,_) <- get
        mbI <- lift $ lift $ findM (return . (== b) . fst) existing
        case mbI of
            Just (_,i) -> return $ Just i
            Nothing -> do
                -- Otherwise, allocate a new index
                i <- nextI
                -- And register it
                modify (\(x,y,z) -> ((b,i):x, y, z))
                -- Look up the closure
                c <- lift $ lift $ deref b
                -- Find indicies for all boxes contained in the map
                c' <- tritraverse pure (traverse (add (n-1))) (add (n-1)) c
                -- Add add the resulting closure to the map
                lift $ tell (M.singleton i (HeapGraphEntry b c' True mempty))
                return $ Just i
    nextI = do
        i <- gets (head . (\(_,b,_) -> b))
        modify (\(a,b,c) -> (a, tail b, c))
        return i

-- | This function updates a heap graph to reflect the current state of
-- closures on the heap, conforming to the following specification.
--
--  * Every entry whose value has been garbage collected by now is marked as
--    dead by setting 'hgeLive' to @False@
--  * Every entry whose value is still live gets the 'hgeClosure' field updated
--    and newly referenced closures are, up to the given depth, added to the graph.
--  * A map mapping previous indicies to the corresponding new indicies is returned as well.
--  * The closure at 'heapGraphRoot' stays at 'heapGraphRoot'
updateHeapGraph :: (Monad m, Monoid a) => DerefFunction m -> Int -> HeapGraph a -> m (HeapGraph a, HeapGraphIndex -> HeapGraphIndex)
updateHeapGraph deref limit (HeapGraph startHG) = do
    (hg', indexMap) <- runWriterT $ foldM go (HeapGraph M.empty) (M.toList startHG)
    return (hg', (M.!) indexMap)
  where
    go hg (i, hge) = do
        (j, hg') <- liftH $ addHeapGraph deref limit (hgeData hge) (hgeClosurePtr hge) hg
        tell (M.singleton i j)
        return hg'

liftH :: (Monad m, Monoid w) => m a -> WriterT w m a
liftH = lift

-- | Pretty-prints a HeapGraph. The resulting string contains newlines. Example
-- for @let s = \"Ki\" in (s, s, cycle \"Ho\")@:
--
-- >let x1 = "Ki"
-- >    x6 = C# 'H' : C# 'o' : x6
-- >in (x1,x1,x6)
ppHeapGraph :: (a -> String) -> HeapGraph a -> String
ppHeapGraph printData (HeapGraph m) = letWrapper ++ ppRef 0 (Just heapGraphRoot)
  where
    -- All variables occuring more than once
    bindings = boundMultipleTimes (HeapGraph m) [heapGraphRoot]

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
    ppBinding i = ppVar i ++ " = " ++ ppEntry 0 (iToE i)

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
    iToE i = m M.! i

    iToUnboundE i
        | i `elem` bindings = Nothing
        | otherwise         = M.lookup i m

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
boundMultipleTimes (HeapGraph m) roots = map head $ filter (not.null) $ map tail $ group $ sort $
     roots ++ concatMap (catMaybes . allClosures . hgeClosure) (M.elems m)

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
