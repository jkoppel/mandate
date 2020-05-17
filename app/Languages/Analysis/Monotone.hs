{-# LANGUAGE ScopedTypeVariables #-}

module Languages.Analysis.Monotone (
    MonotoneFramework(..)
  , chaoticIteration
  ) where


import           Data.Map ( Map, (!) )
import qualified Data.Map.Strict as Map

import CfgGenRuntime
import Graph
import Term

--------------------------------------------------------------

forMap :: (Ord k) => Map k v -> (k -> v -> v) -> Map k v
forMap = flip Map.mapWithKey

--------------------------------------------------------------

data MonotoneFramework s l = MonotoneFramework {
    bottom :: s
  , sourceSt :: s
  , join :: s -> s -> s
  , transfer :: Map (GraphNode l) s -> Term l -> s -> s
  }


iterateToFixpoint :: (Eq a) => (a -> a) -> a -> a
iterateToFixpoint f x = let next = f x in
                        if next == x then x else iterateToFixpoint f next

chaoticIteration :: forall s l. (Eq s) => MonotoneFramework s l -> Graph (GraphNode l) -> GraphNode l -> Map (GraphNode l) s
chaoticIteration fram g source = iterateToFixpoint update startState
  where
    startState = Map.insert source (sourceSt fram) $
                   foldr (\n s -> Map.insert n (bottom fram) s) Map.empty (nodeList g)

    doTransfer :: GraphNode l -> Map (GraphNode l) s -> GraphNode l -> s
    doTransfer cur st n = case graphNode_type cur of
                            EnterNode -> st ! n
                            ExitNode  -> transfer fram st (graphNode_term cur) (st ! n)

    update :: Map (GraphNode l) s -> Map (GraphNode l) s
    update oldSt = forMap oldSt $ \n oldVal ->
                     foldr (join fram) oldVal $
                       map (doTransfer n oldSt) (Graph.preds g n)