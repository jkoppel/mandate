{-# LANGUAGE FlexibleContexts #-}

module TransitionSystem (
    transitionGraph
  , transitionTreeDepth
  , transitionTree
  , transitionSequence
  ) where

import Control.Monad ( forM_, (=<<) )
import Control.Monad.State ( get, modify, evalStateT )
import Control.Monad.Trans ( lift )

import Data.Hashable ( Hashable )

import Graph ( Graph )
import qualified Graph as Graph
import Rose


transitionGraph :: (Eq a, Hashable a, Monad m) => (a -> m [a]) -> a -> m (Graph a)
transitionGraph step start = evalStateT (go [start]) Graph.empty
  where
    go states = do nextStates <- concat <$> mapM expand states
                   go nextStates

    expand st = do succs <- lift (step st)
                   forM_ succs (\succ -> modify (Graph.insert st succ))
                   curGraph <- get
                   return $ filter (\s -> not (Graph.member s curGraph)) succs


transitionTreeDepth :: (Num n, Eq n, Monad m) => (a -> m [a]) -> n -> a -> m (Rose a)
transitionTreeDepth _    0     st = return (Rose st [])
transitionTreeDepth step depth st = Rose st <$> (mapM (transitionTreeDepth step (depth-1)) =<< (step st))

transitionTree :: (Monad m) => (a -> m [a]) -> a -> m (Rose a)
transitionTree step st = transitionTreeDepth step infty st
  where
    infty = read "Infinity" :: Float


transitionSequence :: (Monad m) => (a -> m (Maybe a)) -> a -> m [a]
transitionSequence step st = (st :) <$> (maybe (return []) (transitionSequence step) =<< (step st))
