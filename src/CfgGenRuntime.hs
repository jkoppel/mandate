{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}

module CfgGenRuntime (
    MonadGraphGen
  , GraphGen
  , runGraphGen
  , NodeType(..)
  , GraphNode(..)
  , GraphNodes
  , makeInOut
  , connect

  , inNodes
  , outNodes
  ) where

import Control.Monad.State ( MonadState(..), State, gets, modify, execState)

import Data.Hashable ( Hashable(..) )

import GHC.Generics ( Generic )

import Graph as Graph
import Term

----------------------------------


data NodeType = EnterNode | ExitNode
  deriving (Eq, Ord, Show, Generic)

instance Hashable NodeType

data GraphNode l = GraphNode {
      graphNode_id   :: Int
    , graphNode_type :: NodeType
    , graphNode_term :: Term l
    }
  deriving (Eq, Ord, Show, Generic)

instance (Hashable (Term l)) => Hashable (GraphNode l)

data GraphGenState l = GraphGenState {
    ggs_graph   :: Graph (GraphNode l)
  , ggs_counter :: Int
  }

class (MonadState (GraphGenState l) m) => MonadGraphGen l m | m -> l
instance (MonadState (GraphGenState l) m) => MonadGraphGen l m

newtype GraphGen l a = GraphGen { _runGraphGen :: State (GraphGenState l) a}
  deriving ( Functor, Applicative, Monad, MonadState (GraphGenState l) )

makeEmptyState :: GraphGenState l
makeEmptyState = GraphGenState Graph.empty 0

runGraphGen :: GraphGen l a -> Graph (GraphNode l)
runGraphGen m = ggs_graph $ execState (_runGraphGen m) makeEmptyState

nextId :: (MonadGraphGen l m) => m Int
nextId = do counter <- gets ggs_counter
            modify (\s -> s { ggs_counter = counter + 1})
            return counter

type GraphNodes l = [GraphNode l]

makeInOut :: (MonadGraphGen l m) => Term l -> m (GraphNodes l, GraphNodes l)
makeInOut t = do id1 <- nextId
                 id2 <- nextId
                 let node1 = GraphNode id1 EnterNode t
                 let node2 = GraphNode id2 ExitNode t
                 curGraph <- gets ggs_graph
                 let curGraph' = insertNode node1 $ insertNode node2 curGraph
                 modify (\s -> s {ggs_graph = curGraph'})

                 return ([node1], [node2])


-- This would be just "ggs_graph %= insert a b" with lens, but I'm
-- not using lens b/c compile time (and friendliness to undergrads)
connect1 :: (MonadGraphGen l m) => GraphNode l -> GraphNode l -> m ()
connect1 a b = do curGraph <- gets ggs_graph
                  let curGraph' = insert a NormalEdge b curGraph
                  modify (\s -> s {ggs_graph = curGraph'})

connect :: (MonadGraphGen l m) => GraphNodes l -> GraphNodes l -> m ()
connect as bs = sequence_ [connect1 a b | a <- as, b <- bs]

inNodes :: [GraphNodes l] -> GraphNodes l
inNodes = concat

outNodes :: [GraphNodes l] -> GraphNodes l
outNodes = concat