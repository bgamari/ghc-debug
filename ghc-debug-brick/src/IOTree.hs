{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}

module IOTree
  ( IOTree
  , IOTreePath
  , ioTree
  , renderIOTree
  , handleIOTreeEvent
  , ioTreeSelection
  , ioTreeToggle

  , ioTreeViewSelection
  , unViewTree
  , viewPath
  , viewSelect
  , viewUp
  , viewUnsafeDown
  , viewPrevSibling
  , viewNextSibling
  , viewCollapse
  , viewExpand
  , viewIsCollapsed
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe)
import qualified Data.List as List
import           GHC.Stack

import qualified Graphics.Vty.Input.Events as Vty
import Graphics.Vty.Input.Events (Key(..))
import Brick

-- A tree style list where items can be expanded and collapsed
data IOTree node name = IOTree
    { _name :: name
    , _roots :: [IOTreeNode node name]
    , _renderRow :: Bool         -- Is row selected
                 -> Int          -- Tree depth
                 -> node         -- the node to render
                 -> Widget name
    -- ^ Render a single row
    , _renderFirstChild
                 :: Int             -- Tree depth (of the children)
                 -> node         -- the (parent) node
                 -> [node]       -- the children
                 -> Widget name
    -- Render some extra info as the first child of each node
    , _selection :: [Int]
    -- ^ Indices along the path to the current selection. Empty list means no
    -- selection.
    }


type IOTreePath node = [(Int, node)]

data IOTreeNode node name
  = IOTreeNode
    { _node :: node
      -- ^ Current node
    , _children :: Either
        (IO [IOTreeNode node name])  -- Node is collapsed
        [IOTreeNode node name]       -- Node is expanded
    }

ioTree
  :: forall node name
  .  name
  -- ^ Name of the tree
  -> [node]
  -- ^ Root nodes
  -> (node -> IO [node])
  -- ^ Get child nodes of a node
  -> (Bool         -- Is row selected
      -> Int          -- Tree depth
      -> node         -- the node to render
      -> Widget name)
  -- ^ Row renderer (should add it's own indent based on depth)
  -> (Int             -- Tree depth (of the children)
      -> node         -- the (parent) node
      -> [node]       -- the children
      -> Widget name)
    -- Render some extra info as the first child of each node
  -> IOTree node name
ioTree name rootNodes getChildrenIO renderRow renderFirstChild
  = IOTree
    { _name = name
    , _roots = nodeToTreeNode <$> rootNodes
    , _renderRow = renderRow
    , _renderFirstChild = renderFirstChild
    , _selection = []
    -- ^ TODO we could take the initial path but we'd have to expand through to
    -- that path with IO
    }
  where
  nodeToTreeNode :: node -> IOTreeNode node name
  nodeToTreeNode n = IOTreeNode n (Left (fmap nodeToTreeNode <$> getChildrenIO n))

renderIOTree :: (Show name, Ord name) => IOTree node name -> Widget name
renderIOTree (IOTree widgetName rs renderRow renderFirstChild pathTop)
  = viewport widgetName Both $ vBox $ renderTree 0 0 rs pathTop
  where
  -- Render the tree of nodes
  renderTree _ _ [] _ = []
  renderTree minorIx depth (IOTreeNode node' csE : ns) path = case csE of
    -- Collapsed
    Left _ -> row : rowsRest
    -- Expanded
    Right cs -> row
                  : renderFirstChild (depth + 1) node' (_node <$> cs)
                  : renderTree 0 (depth + 1) cs (if childIsSelected then drop 1 path else [])
                    ++ rowsRest
    where
    childIsSelected = case path of
      x:_ -> x == minorIx
      _ -> False
    selected = path == [minorIx]
    row = (if selected then visible else id) $ renderRow selected depth node'
    rowsRest = renderTree (minorIx + 1) depth ns path

handleIOTreeEvent :: Vty.Event -> IOTree node name -> EventM name (IOTree node name)
handleIOTreeEvent e tree
  = liftIO
  $ forIOTreeViewSelection tree
  $ \view -> fmap viewSelect $ case e of
    Vty.EvKey KRight _ -> do
        (view', cs) <- viewExpand view
        return $ if null cs then view' else viewUnsafeDown view' 0
    Vty.EvKey KDown _ -> return $ fromMaybe view (viewNextVisible view)
    Vty.EvKey KLeft _ -> return $ viewCollapse $ fromMaybe view (viewUp view)
    Vty.EvKey KUp _ -> return $ fromMaybe view (viewPrevVisible view)
    _ -> return view

-- | Toggle (expanded/collapsed) at the current selection.
ioTreeToggle :: IOTree node name -> IO (IOTree node name)
ioTreeToggle t = forIOTreeViewSelection t $ \view ->
  if viewIsCollapsed view
  then fst <$> viewExpand view
  else return (viewCollapse view)

-- | A view (or Zipper) used to navigate the tree
data IOTreeView node name
  = Root (IOTree node name)
  | Node
      (IOTreeNode node name -> IOTreeView node name) -- reconstruct the parent given this node
      Int -- The index in the parent
      (IOTreeNode node name) -- This node

forIOTreeViewSelection
  :: IOTree node name
  -> (IOTreeView node name -> IO (IOTreeView node name))
  -> IO (IOTree node name)
forIOTreeViewSelection t f = unViewTree <$> f (ioTreeViewSelection t)

ioTreeViewSelection :: IOTree node name -> IOTreeView node name
ioTreeViewSelection t = List.foldl' viewUnsafeDown (Root t) (_selection t)

ioTreeSelection :: IOTree node name -> Maybe node
ioTreeSelection t = case ioTreeViewSelection t of
  Root{} -> Nothing
  Node _ _ n -> Just (_node n)

unViewTree :: IOTreeView node name -> IOTree node name
unViewTree t = case t of
    Root t' -> t'
    Node mkParent _ t' -> unViewTree (mkParent t')

-- | Current path in the tree
viewPath :: IOTreeView node name -> [Int]
viewPath tTop = reverse $ go tTop
  where
  go t = case t of
    Root _ -> []
    Node mkParent i t' -> i : go (mkParent t')

-- | Select the current path
viewSelect :: IOTreeView node name -> IOTreeView node name
viewSelect t = ioTreeViewSelection newTree
  where
  newTree = oldTree { _selection = newSelection }
  oldTree = unViewTree t
  newSelection = viewPath t

-- | move up the tree
viewUp :: IOTreeView node name -> Maybe (IOTreeView node name)
viewUp t = case t of
  Root{} -> Nothing
  Node mkParent _ t' -> Just (mkParent t')

-- | Move down to a cild in the tree. Index must be in range. Must be expanded.
viewUnsafeDown :: HasCallStack => IOTreeView node name -> Int -> IOTreeView node name
viewUnsafeDown view i
  | viewIsCollapsed view = error "viewUnsafeDown: view must be expanded"
  | otherwise = case view of
      Root t -> Node (\c -> Root t{ _roots = listSet i c (_roots t) }) i (t !. i)
      Node mkParent ixInParent t -> Node
                  (\c -> Node mkParent ixInParent (unsafeSetChild c i t))
                  i
                  (t ! i)

viewPrevVisible :: HasCallStack => IOTreeView node name -> Maybe (IOTreeView node name)
viewPrevVisible view = case viewPrevSibling view of
  Nothing -> viewUp view
  Just nextSib -> Just (viewLastVisibleChild nextSib)
  where
  viewLastVisibleChild view' = if viewIsCollapsed view'
    then view'
    else let
      n = case view' of
            Root t -> length (_roots t) - 1
            Node _ _ t -> either (error "Impossible! view' is expanded") length (_children t)
      in if n == 0 then view' else viewLastVisibleChild $ viewUnsafeDown view' (n-1)

viewNextVisible :: HasCallStack => IOTreeView node name -> Maybe (IOTreeView node name)
viewNextVisible view = let
  upwardNext v = case viewNextSibling v of
    Nothing -> upwardNext =<< viewUp v
    Just s -> Just s
  in viewFirstVisibleChild view <|> upwardNext view
  where
  viewFirstVisibleChild view' = if viewIsCollapsed view'
    then Nothing
    else let
      nullChildren = case view' of
            Root t -> null (_roots t)
            Node _ _ t -> either (error "Impossible! view' is expanded") null (_children t)
      in if nullChildren then Nothing else Just (viewUnsafeDown view' 0)

-- | Move to the previous sibling within the parent node
viewPrevSibling :: HasCallStack => IOTreeView node name -> Maybe (IOTreeView node name)
viewPrevSibling t = case t of
  Root{} -> Nothing
  Node mkParent ixInParent t' -> if ixInParent == 0
    then Nothing
    else Just $ viewUnsafeDown (mkParent t') (ixInParent - 1)

-- | Move to the next sibling within the parent node
viewNextSibling :: HasCallStack => IOTreeView node name -> Maybe (IOTreeView node name)
viewNextSibling t = case t of
  Root{} -> Nothing
  Node mkParent ixInParent t' -> let
    parent = mkParent t'
    nSiblings = case parent of
        Root t'' -> length (_roots t'')
        Node _ _ t'' -> length (either (error "Impossible! syblings must be expanded") id (_children t''))
    in if ixInParent + 1 == nSiblings
        then Nothing
        else Just (viewUnsafeDown parent (ixInParent + 1))

-- | Collapse the current node.
viewCollapse :: HasCallStack => IOTreeView node name -> IOTreeView node name
viewCollapse t = case t of
  Root _ -> t -- Can't collapse the root
  Node mkParent i t' -> case _children t' of
    Left _ -> t
    Right cs -> Node mkParent i t'{_children = Left (return cs)}

-- | Expand the current node. Returns the children
viewExpand :: HasCallStack => IOTreeView node name -> IO (IOTreeView node name, [IOTreeNode node name])
viewExpand t = case t of
  Root t' -> return (t, _roots t')
  Node mkParent i t' -> case _children t' of
    Left getChildren -> do
      cs <- getChildren
      return (Node mkParent i t'{_children=Right cs}, cs)
    Right cs -> return (t, cs)


viewIsCollapsed :: HasCallStack => IOTreeView node name -> Bool
viewIsCollapsed t = case t of
  Root{} -> False
  Node _ _ t' -> case _children t' of
    Left{} -> True
    Right{} -> False

(!.) :: IOTree node name -> Int -> IOTreeNode node name
t !. i = _roots t !! i

(!) :: IOTreeNode node name -> Int -> IOTreeNode node name
t ! i = case _children t of
  Right xs -> xs !! i
  Left _ -> error "(!): tree node not expanded"

unsafeSetChild ::  HasCallStack => IOTreeNode node name -> Int -> IOTreeNode node name -> IOTreeNode node name
unsafeSetChild newChild i t = case _children t of
  Right xs -> t { _children = Right (listSet i newChild xs) }
  Left _ -> error "(!): tree node not expanded"

listSet :: HasCallStack => Int -> a -> [a] -> [a]
listSet i a as
  | i >= length as = error $ "listSet: index (" ++ show i ++ ") out of bounds [0 - " ++ show (length as) ++ ")"
  | otherwise = take i as ++ [a] ++ drop (i+1) as
