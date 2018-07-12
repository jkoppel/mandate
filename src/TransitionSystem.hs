{-# LANGUAGE FlexibleContexts #-}

module TransitionSystem (
    transitionGraph
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


transitionGraph :: (Eq a, Hashable a, Monad m) => (a -> m [a]) -> a -> m (Graph a)
transitionGraph step start = fst <$> execStateT (go [start]) (Graph.empty, S.empty)
  where
    go []     = return ()
    go states = do nextStates <- concat <$> mapM expand states
                   go nextStates

    expand st = do modify (\(g, seen) -> (g, S.insert st seen))
                   debugM "Doing step"
                   succs <- lift (step st)
                   debugM "Evalling succs"
                   forM_ succs (\succ -> modify (\(g, seen) -> (Graph.insert st succ g, seen)))
                   debugM "Inserted nexts succs"
                   seen <- gets snd
                   return $ filter (\s -> not (S.member s seen)) succs


transitionTreeDepth :: (Num n, Eq n, Monad m) => (a -> m [a]) -> n -> a -> m (Rose a)
transitionTreeDepth _    0     st = return (Rose st [])
transitionTreeDepth step depth st = Rose st <$> (mapM (transitionTreeDepth step (depth-1)) =<< (step st))

transitionTree :: (Monad m) => (a -> m [a]) -> a -> m (Rose a)
transitionTree step st = transitionTreeDepth step infty st
  where
    infty = read "Infinity" :: Float


transitionSequence :: (Monad m) => (a -> m (Maybe a)) -> a -> m [a]
transitionSequence step st = (st :) <$> (maybe (return []) (transitionSequence step) =<< (step st))
