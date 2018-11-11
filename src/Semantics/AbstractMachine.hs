{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, UndecidableInstances #-}


module Semantics.AbstractMachine (
    AMState
  , pattern AMState
  , AMRhs
  , AMRule
  , NamedAMRule
  , pattern AM
  , pattern NamedAMRule
  , NamedAMRules

  , amEvaluationSequence
  , amEvaluationTreeDepth
  , amEvaluationTree

  , abstractAmCfg
  ) where

import Control.Monad ( liftM )

import Data.ByteString.Char8 ( ByteString )

import Configuration
import Graph
import Lang
import Matching
import Rose
import Semantics.Abstraction
import Semantics.Context
import Semantics.GeneralMachine
import Term
import TransitionSystem

---------------------------------------------------------------------

type AMState = GenAMState ()

pattern AMState :: Configuration l -> Context l -> AMState l
pattern AMState c k = GenAMState c k ()

type AMRhs = GenAMRhs AMState

type AMRule = GenAMRule ()
type NamedAMRule = NamedGenAMRule ()

pattern AM :: AMState l -> AMRhs l -> AMRule l
pattern AM left right = GenAMRule left right

pattern NamedAMRule :: ByteString -> AMRule l -> NamedAMRule l
pattern NamedAMRule s r = NamedGenAMRule s r

type NamedAMRules l = [NamedAMRule l]

-------------------------------------------------------------------------------------------------------

stepAm1 :: (Lang l) => NamedAMRules l -> AMState l -> IO (Maybe (AMState l))
stepAm1 rules st = runMatchFirst $ stepGenAm rules st

stepAm :: (Lang l) => NamedAMRules l -> AMState l -> IO [AMState l]
stepAm rules st = runMatch $ stepGenAm rules st

initAmState :: (Lang l) => Term l -> AMState l
initAmState t = AMState (initConf t) KHalt

amEvaluationSequence :: (Lang l) => NamedAMRules l -> Term l -> IO [AMState l]
amEvaluationSequence rules t = transitionSequence (stepAm1 rules) (initAmState t)

amEvaluationTreeDepth :: (Lang l, Num a, Eq a) => a -> NamedAMRules l -> Term l -> IO (Rose (AMState l))
amEvaluationTreeDepth depth rules t = transitionTreeDepth (stepAm rules) depth (initAmState t)

amEvaluationTree :: (Lang l) => NamedAMRules l -> Term l -> IO (Rose (AMState l))
amEvaluationTree rules t = transitionTree (stepAm rules) (initAmState t)

----------------------------

abstractAmCfg :: (Lang l) => Abstraction (CompFunc l) -> Abstraction (AMState l) -> NamedAMRules l -> Term l -> IO (Graph (AMState l))
abstractAmCfg absFunc abs rules t = transitionGraph (liftM (map abs) . stepAm (map (abstractCompFuncs absFunc) rules)) (abs $ initAmState t)

-----------------------------
