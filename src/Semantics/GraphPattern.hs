{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TupleSections, TypeApplications #-}

module Semantics.GraphPattern (
    HasTopState(..)
  , abstractGraphPattern
  , makeGraphPatterns
  ) where

import Control.Monad ( forM, (=<<) )
import Data.Foldable
import Data.Map ( Map )
import qualified Data.Map as Map

import Configuration
import Graph
import Lang
import Lattice
import Semantics.Abstraction
import Semantics.AbstractMachine
import Semantics.Context
import Term
import TransitionSystem
import Var

-- This can be different from (UpperBound (RedState l)).
-- Main difference: top for environments is [AllStar -> AllStar], but
-- you may use environments which only store values, and hence top would be [ValStar -> ValStar]
class (Lang l) => HasTopState l where
  topRedState :: RedState l

-- | This is useful because the Configuration value holds
-- some useful instances
swapState :: Configuration l -> RedState l -> Configuration l
swapState (Conf t _) s = Conf t s

conf :: (Lang l) => Term l -> RedState l -> Configuration l
conf t s = swapState (initConf t) s

abstractGraphPattern :: forall l. (HasTopState l) => Abstraction (CompFunc l) -> Abstraction (AMState l) -> NamedAMRules l -> Term l -> IO (Graph (AMState l))
abstractGraphPattern absFunc abs rules t = do
    initState <- abs <$> AMState (conf t (topRedState @l)) <$> KVar <$> nextVar
    explorationGraph step initState
  where
    step :: AMState l -> IO [(AMState l, TransitionType)]
    step as@(AMState (Conf  NonvalStar   _) k) = return [(AMState (Conf ValStar (topRedState @l)) k, Explore)]
    step as@(AMState (Conf (NonvalVar _) _) k) = return [(AMState (Conf ValStar (topRedState @l)) k, Explore)]
    step as = map (,Step) <$> map abs <$> stepAm (map (abstractCompFuncs absFunc) rules) as


makeGraphPatterns :: (HasTopState l) => Abstraction (CompFunc l) -> Abstraction (AMState l)
                                     -> NamedAMRules l -> Signature l
                                     -> IO (Map Symbol (Graph (AMState l)))
makeGraphPatterns absFunc abs rules sig = flip foldMap nodeSigs $ \n ->
                                                   Map.singleton (sigNodeSymbol n)
                                                             <$> (makePattern =<< canonicalElt n)
  where
    isNode (NodeSig _ _ _) = True
    isNode _               = False

    nodeSigs = filter isNode sigNodes
      where sigNodes = case sig of (Signature s) -> s

    canonicalElt :: SigNode -> IO (Term l)
    --canonicalElt (NodeSig sym children _) = Node sym <$> mapM (const (NonvalVar <$> nextVar)) children
    canonicalElt (NodeSig sym children _) = return $ Node sym (map (const NonvalStar) children)

    makePattern = abstractGraphPattern absFunc abs rules


{-
 - <+(t1, t2) | 50 > ->
 - (a, b) <- makeInOut
 - (in1, out1) <- genCfg t1
 - (in2, out2) <- genCfg t2
 - wire a in1
 - wire out1 in2
 - wire out2 b
-}

-- |
-- Strategy to implement algo:
--