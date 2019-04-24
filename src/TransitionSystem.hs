{-# LANGUAGE FlexibleContexts, TupleSections #-}

module TransitionSystem (
    TransitionType(..)
  , explorationGraph
  , transitionGraph
  , transitionTreeDepth
  , transitionTree
  , transitionSequence
  ) where

import Control.Monad ( forM_, (=<<) )
import Control.Monad.State ( gets, modify, execStateT )
import Control.Monad.Trans ( lift )

import Data.Hashable ( Hashable )
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Debug
import Graph ( Graph )
import qualified Graph as Graph
import Rose

-- | Functions in this file are used to construct all reachable states
-- from some start under a "step" function that returns all successor states.
-- Functions are available for graphs, trees, and (for deterministic systems) sequences.
--
-- This is used to take our various formulations of semantics, which each define one step,
-- and use them to execute a program fully.


data TransitionType = Step | Explore

-- | Variant of `transitionGraph` where successor function can also generate nodes that should
-- be viewed as a new start point and not have incoming edges
--
-- Naming this was really hard. Best I came up with.
explorationGraph :: (Eq a, Hashable a, Monad m) => (a -> m [(a, TransitionType)]) -> a -> m (Graph a)
explorationGraph step start = fst <$> execStateT (go [start]) (Graph.empty, S.empty)
  where
    go []     = return ()
    go states = do nextStates <- concat <$> mapM expand states
                   go nextStates

    expand st = do modify (\(g, seen) -> (g, S.insert st seen))
                   debugM "Doing step"
                   succs <- lift (step st)
                   debugM "Evalling succs"
                   forM_ succs $ \(succ, transType) ->
                                  modify $ \(g, seen) ->
                                    case transType of
                                      Step    -> (Graph.insert st  succ g, seen)
                                      Explore -> (Graph.insertNode succ g, seen)
                   debugM "Inserted nexts succs"
                   seen <- gets snd
                   return $ filter (\s -> not (S.member s seen)) $ map fst succs

-- | Similar to `transitionTree`, but merges repeated states. Especially important for cyclic transition systems.
--
-- Cyclic transition systems include nonterminating programs and (most importantly)
-- abstracted non-cyclic systems. I.e.: if the concrete executions of a program
-- form a tree, its abstract executions will form a graph, namely a control-flow graph
transitionGraph :: (Eq a, Hashable a, Monad m) => (a -> m [a]) -> a -> m (Graph a)
transitionGraph step = explorationGraph (fmap (map (,Step)) . step)


transitionTreeDepth :: (Num n, Eq n, Monad m) => (a -> m [a]) -> n -> a -> m (Rose a)
transitionTreeDepth _    0     st = return (Rose st [])
transitionTreeDepth step depth st = Rose st <$> (mapM (transitionTreeDepth step (depth-1)) =<< (step st))

transitionTree :: (Monad m) => (a -> m [a]) -> a -> m (Rose a)
transitionTree step st = transitionTreeDepth step infty st
  where
    infty = read "Infinity" :: Float


transitionSequence :: (Monad m) => (a -> m (Maybe a)) -> a -> m [a]
transitionSequence step st = (st :) <$> (maybe (return []) (transitionSequence step) =<< (step st))
