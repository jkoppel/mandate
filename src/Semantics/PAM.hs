{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PatternSynonyms, UndecidableInstances #-}

-- | Implementation of "phased abstract machines," a transition system corresponding
--   to reduction (Felleisen-Hieb) semantics
--
-- Wait; nevermind. The "lockstep composition" rule (e1 || e2) -> (e1' || e2') is straightforward to implement in PAM,
-- but I don't see how to do it in reduction semantics. So....I guess then PAM is more powerful than Felleisen-Hieb.

module Semantics.PAM (
    Phase(..)
  , PAMState
  , pattern PAMState
  , PAMRhs
  , PAMRule
  , pattern PAM
  , NamedPAMRule
  , pattern NamedPAMRule
  , NamedPAMRules

  , stepPam1

  , pamEvaluationSequence
  , pamEvaluationTreeDepth
  , pamEvaluationTree
  , abstractPamCfg
  ) where

import Control.Monad ( MonadPlus(..), guard, liftM )
import Data.Set ( Set )
import qualified Data.Set as Set

import GHC.Generics ( Generic )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Hashable ( Hashable )

import Configuration
import Debug
import Graph
import Lang
import Matching
import Rose
import Semantics.Abstraction
import Semantics.Context
import Semantics.General
import Semantics.GeneralMachine
import Term
import TransitionSystem


----------------------------- PAM rule type ----------------------------------

data Phase = Up | Down
  deriving (Eq, Ord, Generic)

instance Hashable Phase

instance Show Phase where
  showsPrec _ Up   = showString "up"
  showsPrec _ Down = showString "down"

instance Matchable Phase where
  getVars _ = Set.empty
  refreshVars t = return t
  match (Pattern x) (Matchee y) = guard (x == y)
  fillMatch t = return t

type PAMState = GenAMState Phase

pattern PAMState :: Configuration l -> Context l -> Phase -> PAMState l
pattern PAMState c k p = GenAMState c k p

instance (Show (Configuration l), Show (Context l)) => Show (PAMState l) where
  showsPrec d (PAMState c k p) = showsPrec d (GenAMState c k ()) . showString " " . showsPrec d p

type PAMRhs = GenAMRhs PAMState

type PAMRule = GenAMRule Phase
type NamedPAMRule = NamedGenAMRule Phase

------------------------------- Smart constructors and abbreviations ------------------------------

pattern PAM :: PAMState l -> PAMRhs l -> PAMRule l
pattern PAM left right = GenAMRule left right

pattern NamedPAMRule :: ByteString -> PAMRule l -> NamedPAMRule l
pattern NamedPAMRule s r = NamedGenAMRule s r

type NamedPAMRules l = [NamedPAMRule l]

----------------------------------- Traversals (sans generic programming) ----------------------------

instance (Matchable (Configuration l), NormalizeBoundVars (Context l)) => NormalizeBoundVars (PAMState l) where
  normalizeBoundVars' (PAMState c k p) = PAMState <$> fillMatch c <*> normalizeBoundVars' k <*> return p

instance (Matchable (Configuration l), Matchable (ExtComp l), NormalizeBoundVars (PAMState l)) => NormalizeBoundVars (PAMRhs l) where
  normalizeBoundVars' (GenAMLetComputation cfg comp rhs) = GenAMLetComputation <$> fillMatch cfg
                                                                               <*> fillMatch comp
                                                                               <*> normalizeBoundVars' rhs

  normalizeBoundVars' (GenAMRhs st) = GenAMRhs <$> normalizeBoundVars' st

------------------------------- Single-stepping PAM rules -----------------------------------------------------


-- We can't quite make this a normal PAM rule because you can't match on an arbitrary configuration
useBaseRule :: (Lang l) => PAMState l -> Match (PAMState l)

useBaseRule (PAMState c@(Conf (Node _ _) _) KHalt Up) = do debugM "Step completed; moving to next Step"
                                                           return (PAMState c KHalt Down)
--useBaseRule (PAMState c@(Conf (Val _ _) _) k Down) = do debugM "Value reached; going up"
--                                                        return (PAMState c k Up)
useBaseRule _ = mzero

stepPam1 :: (Lang l) => NamedPAMRules l -> PAMState l -> IO (Maybe (PAMState l))
stepPam1 rules st = runMatchFirst $ useBaseRule st `mplus` stepGenAm rules st

stepPam :: (Lang l) => NamedPAMRules l -> PAMState l -> IO [PAMState l]
stepPam rules st = runMatch $ useBaseRule st `mplus` stepGenAm rules st

initPamState :: (Lang l) => Term l -> PAMState l
initPamState t = PAMState (initConf t) KHalt Down

--------------------------------- PAM executions --------------------------------------------

pamEvaluationSequence :: (Lang l) => NamedPAMRules l -> Term l -> IO [PAMState l]
pamEvaluationSequence rules t = transitionSequence (stepPam1 rules) (initPamState t)

pamEvaluationTreeDepth :: (Lang l, Num a, Eq a) => a -> NamedPAMRules l -> Term l -> IO (Rose (PAMState l))
pamEvaluationTreeDepth depth rules t = transitionTreeDepth (stepPam rules) depth (initPamState t)

pamEvaluationTree :: (Lang l) => NamedPAMRules l -> Term l -> IO (Rose (PAMState l))
pamEvaluationTree rules t = transitionTree (stepPam rules) (initPamState t)

----------------------------

abstractPamCfg :: (Lang l) => Abstraction (CompFunc l) -> Abstraction (PAMState l) -> NamedPAMRules l -> Term l -> IO (Graph (PAMState l))
abstractPamCfg absFunc abs rules t = transitionGraph (liftM (map abs) . stepPam (map (abstractCompFuncs absFunc) rules)) (abs $ initPamState t)

----------------------------

instance ValueIrrelevance Phase where
  valueIrrelevance p = p