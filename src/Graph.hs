{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Graph (
    Graph
  , showDfsOrder
  , empty
  , insert
  , member
  ) where

import Control.Monad.Writer ( tell, execWriter )
import Control.Monad.State ( gets, modify, evalStateT )

import Data.List ( partition )

import Data.HashMap.Strict ( HashMap, (!) )
import qualified Data.HashMap.Strict as M
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Data.Hashable ( Hashable(..) )

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

showDfsOrder :: (Eq a, Hashable a, Show a) => Graph a -> String
showDfsOrder g = (foldr (\(x,n) s -> showsPrecNode' 0 x n s) id $ dfsOrder g) ""

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

member :: (Eq a, Hashable a) => a -> Graph a -> Bool
member a = M.member a . getGraph

