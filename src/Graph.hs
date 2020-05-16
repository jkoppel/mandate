{-# LANGUAGE DeriveGeneric, FlexibleContexts, TupleSections #-}

module Graph (
    Graph
  , EdgeType(..)
  , showDfsOrder
  , nodeList

  , edgeList
  , edgeListForType
  , normalEdgeList
  , transitiveEdgeList

  , empty
  , insertNode
  , insert

  , withoutIsolatedNodes

  , member
  , succs
  , preds
  , sinks
  , reachableNodes


  , toRealGraph
  , toRealConvGraph

  , Projection
  , graphQuotient
  ) where

import Control.Monad.Writer ( tell, execWriter )
import Control.Monad.State ( gets, modify, evalStateT )

import Data.Function ( on )
import Data.List ( partition, elemIndex, sortBy, groupBy, null, nub )
import Data.Maybe (fromMaybe)

import GHC.Generics (Generic)

import qualified Data.Graph.Inductive.Graph as RealGraph
import Data.HashMap.Strict ( HashMap, (!) )
import qualified Data.HashMap.Strict as M
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Data.Hashable ( Hashable(..) )



-- | A simple digraph, implemented as a map of nodes to edges.

------------------------------------------------------------------------------------------------

--------------------------------------- Types ------------------------

-- NOTE to self:
-- Figuring out how to cleanly parameterize over the type of edges without
-- complexifying the interface is a great and easy-to-understand example of
-- a problem I've faced a ton in Haskell

-- Until I figure that out, this is a design problem,
-- as the definition of graph node is needlessly specialized to transition systems


data EdgeType = NormalEdge | TransitiveEdge
  deriving (Eq, Ord, Show, Generic)

instance Hashable EdgeType


data GraphNode a = GraphNode { inDeg  :: !Int
                             , outDeg :: !Int
                             , edges  :: !(HashSet (EdgeType, a))}

newtype Graph a = Graph { getGraph :: HashMap a (GraphNode a) }


---------------------------- Printing -----------------------------

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
        mapM_ (doDfs.snd) (S.toList $ edges node)

---------------------------- As list -----------------------------

nodeList :: Graph a -> [a]
nodeList (Graph g) = M.keys g

edgeList :: Graph a -> [(a, EdgeType, a)]
edgeList (Graph g) = map flat3 $ M.foldrWithKey (\a n lst -> map (a,) (S.toList $ edges n) ++ lst) [] g
  where flat3 (a, (b, c)) = (a, b, c)

edgeListForType :: EdgeType -> Graph a -> [(a, a)]
edgeListForType t g = [(a, b) | (a, t', b) <- edgeList g, t == t']

normalEdgeList :: Graph a -> [(a, a)]
normalEdgeList = edgeListForType NormalEdge

transitiveEdgeList :: Graph a -> [(a, a)]
transitiveEdgeList = edgeListForType TransitiveEdge


------------------------ Export -----------------------------------

nodeToIndex :: (Eq a) => Graph a -> a -> Int
nodeToIndex g n = fromMaybe (negate 1) (elemIndex n (M.keys $ getGraph g))

indexToNode :: (Eq a) => Graph a -> Int -> a
indexToNode g n = M.keys (getGraph g) !! n

realNodes :: (Eq a, Show a) => Graph a -> [RealGraph.LNode String]
realNodes g = map (\key -> (nodeToIndex g key, show key)) (M.keys $ getGraph g)

realNodesConv :: (Eq a, Show b) => Graph a -> (a -> b) -> [RealGraph.LNode String]
realNodesConv g conv = map (\key -> (nodeToIndex g key, show $ conv key)) (M.keys $ getGraph g)

realEdgesForNode :: (Eq a) => Graph a -> a -> GraphNode a -> [RealGraph.LEdge String]
realEdgesForNode g n es = S.toList $ S.map (\(edgeType, targ) -> (nodeToIndex g n, nodeToIndex g targ, prettyET edgeType)) (edges es)
  where
    prettyET NormalEdge = ""
    prettyET TransitiveEdge = "transitive"

realEdges :: (Eq a) => Graph a -> [RealGraph.LEdge String]
realEdges g = concat $ M.mapWithKey (realEdgesForNode g) (getGraph g)

toRealGraph :: (RealGraph.Graph gr, Eq a, Show a) => Graph a -> gr String String
toRealGraph g = RealGraph.mkGraph (realNodes g) (realEdges g)

toRealConvGraph :: (RealGraph.Graph gr, Eq a, Show b) => Graph a -> (a -> b) -> gr String String
toRealConvGraph g conv = RealGraph.mkGraph (realNodesConv g conv) (realEdges g)

---------------------- Graph construction -------------------------

empty :: Graph a
empty = Graph M.empty

newNode :: GraphNode a
newNode = GraphNode 0 0 S.empty

-- | `insertNode a g` adds x as a new node to g. Does nothing if x is already in the graph.
insertNode :: (Eq a, Hashable a) => a -> Graph a -> Graph a
insertNode a (Graph m) = Graph $ M.insertWith (\_ x -> x) a newNode m

-- | `insert x y g` adds an edge from `x` to `y` in graph `g`. `x` and `y` do
-- not need to be pre-existing nodes in the graph.
insert :: (Eq a, Hashable a) => a -> EdgeType -> a -> Graph a -> Graph a
insert x et y (Graph m) = Graph $ addEdge x et y
                                $ incOutDeg x
                                $ incInDeg y
                                $ addIfNotExists x newNode
                                $ addIfNotExists y newNode
                                $ m
  where
    incInDeg  i mp = M.adjust (\n -> n { inDeg =  inDeg n + 1}) i mp
    incOutDeg i mp = M.adjust (\n -> n {outDeg = outDeg n + 1}) i mp
    addIfNotExists a b mp = M.insertWith (\_ x -> x) a b mp
    addEdge a t b mp = M.adjust (\n -> n { edges = S.insert (t, b) (edges n)}) a mp

------------------------------- Graph transformation ------------------------


withoutIsolatedNodes :: (Eq a, Hashable a) => Graph a -> Graph a
withoutIsolatedNodes (Graph m) = Graph $ M.filter (\n -> inDeg n > 0 || outDeg n > 0) m

------------------------------- Graph querying ------------------------

-- | `member n graph` returns whether n is a node in `graph`
member :: (Eq a, Hashable a) => a -> Graph a -> Bool
member a = M.member a . getGraph

-- | All types of edges
succs :: (Eq a, Hashable a) => Graph a -> a -> [a]
succs g a = case M.lookup a (getGraph g) of
              Nothing -> error ("succs: Node not in graph")
              Just n  -> map snd $ S.toList (edges n)


-- | All types of edges
---  Warning: iterates over entire graph; does quadratic filtering
preds :: (Eq a, Hashable a) => Graph a -> a -> [a]
preds g a = nub $ map (\(n,_,_) -> n) $ filter (\(_,_,x) -> x == a) $ edgeList g

sinks :: (Eq a, Hashable a) => Graph a -> [a]
sinks g = filter (\x -> null (succs g x)) $ nodeList g

reachableNodes :: (Eq a, Hashable a) => Graph a -> a -> [a]
reachableNodes g a = S.toList (go [a] S.empty)
  where

    expand []     (s, added) = (s, added)
    expand (x:xs) (s, added) = expand xs (newS, newAdded)
      where
        (newS, newAdded) = foldr (\x' (s', added') -> if S.member x' s' then (s', added')
                                                                        else (S.insert x' s', x' : added'))
                                 (s, added)
                                 (succs g x)

    go xs s = case expand xs (s, []) of
                (s', [])  -> s'
                (s', xs') -> go xs' s'

------------------------ Graph quotients ---------------------------

type Projection a b = a -> b


-- TODO: Only handles normal edges
graphQuotient :: (Eq a, Hashable a, Ord b, Hashable b) => Projection a b -> Graph a -> Graph b
graphQuotient p g = let projNoSelfEs = foldr (\(a, t, b) -> insertWithoutSelfEdges a t b) empty (edgeList g)
                    in foldr (\a -> insert a NormalEdge a) projNoSelfEs selfEdges
    where
      insertWithoutSelfEdges a t b = if p a /= p b
                                     then insert (p a) t (p b)
                                     else id

      equivClasses :: (Ord b) => (a -> b) -> [a] -> [[a]]
      equivClasses f = groupBy ((==) `on` f) . sortBy (compare `on` f)

      selfEdges = map (p.head) $ filter (all hasSelfEdge) $ equivClasses p (nodeList g)
        where
          hasSelfEdge a = let es = S.toList $ edges (getGraph g ! a)
                          in case partition (\(t, b) -> t == NormalEdge) es of
                            (_, x:xs)      -> error "Non-normal edge found in graph to quotient; not yet implemented"
                            (normalEs, []) -> any (\(t, b) -> p b == p a) normalEs
