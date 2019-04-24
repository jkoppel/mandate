{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TupleSections, TypeApplications #-}

module Semantics.GraphPattern (
    HasTopState(..)
  , abstractGraphPattern
  ) where

import Configuration
import Graph
import Lang
import Semantics.Abstraction
import Semantics.AbstractMachine
import Semantics.Context
import Term
import TransitionSystem
import Var

class (Lang l) => HasTopState l where
  topRedState :: RedState l

abstractGraphPattern :: forall l. (HasTopState l) => Abstraction (CompFunc l) -> Abstraction (AMState l) -> NamedAMRules l -> Term l -> IO (Graph (AMState l))
abstractGraphPattern absFunc abs rules t = do
    initState <- abs <$> AMState (initConf t) <$> KVar <$> nextVar
    explorationGraph step initState
  where
    step :: AMState l -> IO [(AMState l, TransitionType)]
    step as@(AMState (Conf (NonvalVar _) _) k) = return [(AMState (Conf ValStar (topRedState @l)) k, Explore)]
    step as = map (,Step) <$> map abs <$> stepAm (map (abstractCompFuncs absFunc) rules) as

