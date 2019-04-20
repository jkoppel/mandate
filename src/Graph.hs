{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Graph (
    Graph
  , showDfsOrder
  , empty
  , insert
  , member
  , toRealGraph
  , toRealConvGraph
  , graphQuotient
  ) where

import Control.Monad.Writer ( tell, execWriter )
import Control.Monad.State ( gets, modify, evalStateT )

import Data.List ( partition, elemIndex )
import Data.Maybe (fromMaybe)

import qualified Data.Graph.Inductive.Graph as RealGraph
import Data.HashMap.Strict ( HashMap, (!) )
import qualified Data.HashMap.Strict as M
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Data.Hashable ( Hashable(..) )



-- | A simple digraph, implemented as a map of nodes to edges.

------------------------------------------------------------------------------------------------

data GraphNode a = GraphNode { inDeg  :: !Int
                             , outDeg :: !Int
                             , edges  :: !(HashSet a)}

newtype Graph a = Graph { getGraph :: HashMap a (GraphNode a) }

showsPrecNode' :: (Show a) => Int -> a -> GraphNode a -> ShowS -> ShowS
showsPrecNode' d n es s = showString "Node: " . showsPrec d n . showString "\n" .
                          showString "Edges: \n" . S.foldr showsTarg id (edges es) .
                          showString "\n\n" . s
  where
    showsTarg e s = showString "--" . showsPrec d e . showString "\n" . s

instance Show a => Show (Graph a) where
  showsPrec d (Graph edgeMap) = M.foldrWithKey (showsPrecNode' d) id edgeMap

-- | Pretty-prints a graph in DFS order. This means that consecutive nodes in a CFG
-- will tend to be printed in sequence.
showDfsOrder :: (Eq a, Hashable a, Show a) => Graph a -> String
showDfsOrder g = (foldr (\(x,n) s -> showsPrecNode' 0 x n s) id $ dfsOrder g) ""

-- Used only by showDfsOrder
dfsOrder :: (Eq a, Hashable a) => Graph a -> [(a, GraphNode a)]
dfsOrder (Graph g) = execWriter $ evalStateT (mapM doDfs inspectOrder) S.empty
  where
    allNodes = M.toList g
    (startNodes, othNodes) = partition (\(_, n) -> inDeg n == 0) allNodes
    inspectOrder = map fst (startNodes ++ othNodes)

    doDfs x = do
      looked <- gets (S.member x)
      if looked then
        return ()
       else do
        let node = g ! x
        modify (S.insert x)
        tell [(x, node)]
        mapM_ doDfs (S.toList $ edges node)


empty :: Graph a
empty = Graph M.empty

newNode :: GraphNode a
newNode = GraphNode 0 0 S.empty

nodeToIndex :: (Eq a) => Graph a -> a -> Int
nodeToIndex g n = fromMaybe (negate 1) (elemIndex n (M.keys $ getGraph g))

indexToNode :: (Eq a) => Graph a -> Int -> a
indexToNode g n = M.keys (getGraph g) !! n

realNodes :: (Eq a, Show a) => Graph a -> [RealGraph.LNode String]
realNodes g = map (\key -> (nodeToIndex g key, show key)) (M.keys $ getGraph g)

realNodesConv :: (Eq a, Show b) => Graph a -> (a -> b) -> [RealGraph.LNode String]
realNodesConv g conv = map (\key -> (nodeToIndex g key, show $ conv key)) (M.keys $ getGraph g)

realEdgesForNode :: (Eq a) => Graph a -> a -> GraphNode a -> [RealGraph.LEdge String]
realEdgesForNode g n es = S.toList $ S.map (\edge -> (nodeToIndex g n, nodeToIndex g edge, "")) (edges es)

realEdges :: (Eq a) => Graph a -> [RealGraph.LEdge String]
realEdges g = concat $ M.mapWithKey (realEdgesForNode g) (getGraph g)

-- | `insert x y g` adds an edge from `x` to `y` in graph `g`. `x` and `y` do
-- not need to be pre-existing nodes in the graph.
insert :: (Eq a, Hashable a) => a -> a -> Graph a -> Graph a
insert x y (Graph m) = Graph $ addEdge x y
                             $ incOutDeg x
                             $ incInDeg y
                             $ addIfNotExists x newNode
                             $ addIfNotExists y newNode
                             $ m
  where
    incInDeg  i mp = M.adjust (\n -> n { inDeg =  inDeg n + 1}) i mp
    incOutDeg i mp = M.adjust (\n -> n {outDeg = outDeg n + 1}) i mp
    addIfNotExists a b mp = M.insertWith (\_ x -> x) a b mp
    addEdge a b mp = M.adjust (\n -> n { edges = S.insert b (edges n)}) a mp


-- | `member n graph` returns whether n is a node in `graph`
member :: (Eq a, Hashable a) => a -> Graph a -> Bool
member a = M.member a . getGraph

toRealGraph :: (RealGraph.Graph gr, Eq a, Show a) => Graph a -> gr String String
toRealGraph g = RealGraph.mkGraph (realNodes g) (realEdges g)

toRealConvGraph :: (RealGraph.Graph gr, Eq a, Show b) => Graph a -> (a -> b) -> gr String String
toRealConvGraph g conv = RealGraph.mkGraph (realNodesConv g conv) (realEdges g)

type Projection a b = a -> b

graphQuotient :: (Eq a, Hashable a, Eq b, Hashable b) => Projection a b -> Graph a -> Graph b
graphQuotient p ga = foldr (\(ia, ib, l) -> insertWithoutSelfEdges ia ib) empty (realEdges ga)
    where
      aIndexToB i = p $ indexToNode ga i
      insertWithoutSelfEdges a b = if aIndexToB a /= aIndexToB b
                                      then insert (aIndexToB a) (aIndexToB b)
                                      else id
