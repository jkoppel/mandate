{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, UndecidableInstances #-}

-- | Implementation of "phased abstract machines," a transition system corresponding
--   to reduction (Felleisen-Hieb) semantics
--
-- Wait; nevermind. The "lockstep composition" rule (e1 || e2) -> (e1' || e2') is straightforward to implement in PAM,
-- but I don't see how to do it in reduction semantics. So....I guess then PAM is more powerful than Felleisen-Hieb.

module Semantics.PAM (
    PAMRule
  , NamedPAMRule
  , sosToPam

  , pamEvaluationSequence
  , pamEvaluationTreeDepth
  , pamEvaluationTree
  , abstractPamCfg

  , upRulesInvertible
  ) where

import Control.Monad ( MonadPlus(..), guard, liftM )
import Data.Maybe ( fromJust )
import Data.Monoid ( Monoid(..), )
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
import Semantics.SOS
import Term
import TransitionSystem
import Var


----------------------------- PAM rule type ----------------------------------

data Phase = Up | Down
  deriving (Eq, Ord, Generic)

instance Hashable Phase

flipPhase :: Phase -> Phase
flipPhase Up = Down
flipPhase Down = Up

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

namePAMRule :: ByteString -> PAMRule l -> NamedPAMRule l
namePAMRule = NamedPAMRule

type NamedPAMRules l = [NamedPAMRule l]

----------------------------------- Helpers for SOS-to-PAM conversion -------------------------------

type InfNameStream = [ByteString]

infNameStream :: ByteString -> InfNameStream
infNameStream nam = map (\i -> mconcat [nam, "-", BS.pack $ show i]) [1..]


----------------------------------- Traversals (sans generic programming) ----------------------------

instance (Matchable (Configuration l), NormalizeBoundVars (Context l)) => NormalizeBoundVars (PAMState l) where
  normalizeBoundVars' (PAMState c k p) = PAMState <$> fillMatch c <*> normalizeBoundVars' k <*> return p

instance (Matchable (Configuration l), Matchable (ExtComp l), NormalizeBoundVars (PAMState l)) => NormalizeBoundVars (PAMRhs l) where
  normalizeBoundVars' (GenAMLetComputation cfg comp rhs) = GenAMLetComputation <$> fillMatch cfg
                                                                               <*> fillMatch comp
                                                                               <*> normalizeBoundVars' rhs

  normalizeBoundVars' (GenAMRhs st) = GenAMRhs <$> normalizeBoundVars' st



----------------------------------- SOS to PAM conversion --------------------------------------------

-- | Strips off the first layer of nesting in a context. All computation up to the first
-- KStepTo is converted into a PAM RHS. The remainder, if any, is returned for further conversion into the next rule.
splitFrame :: (Lang l) => PosFrame l -> Context l -> (PAMRhs l, Context l, Maybe (Configuration l, PosFrame l))
splitFrame (KBuild c) k = (GenAMRhs $ PAMState c k Up, k, Nothing)
splitFrame (KStepTo c f@(KInp i pf)) k =
    let f' = KInp i pf in
    let cont = KPush f k in
    let cont' = KPush (KInp i pf) k in
    (GenAMRhs $ PAMState c cont Down, cont', Just (i, pf))
splitFrame (KComputation comp (KInp c pf)) k = let (subRhs, ctx, rest) = splitFrame pf k in
                                               (GenAMLetComputation c comp subRhs, ctx, rest)

sosRuleToPam' :: (Lang l) => InfNameStream -> PAMState l -> Context l -> PosFrame l -> IO [NamedPAMRule l]
sosRuleToPam' (nm:nms) st k fr = do
    debugM $ show fr
    let (rhs, k', frRest) = splitFrame fr k
    restRules <- case frRest of
                   Nothing           -> return []
                   Just (conf', fr') -> sosRuleToPam' nms (PAMState conf' k' Up) k fr'
    rule <- fromJust <$> runMatchUnique (namePAMRule nm <$> (PAM <$> normalizeBoundVars st <*> normalizeBoundVars rhs))
    return (rule : restRules)


sosRuleToPam :: (Lang l) => NamedRule l -> IO [NamedPAMRule l]
sosRuleToPam (NamedRule nam (StepTo conf rhs)) = do
  kv <- nextVar
  let startState = PAMState conf (KVar kv) Down
  sosRuleToPam' (infNameStream nam) startState (KVar kv) (rhsToFrame rhs)


sosToPam :: (Lang l) => NamedRules l -> IO (NamedPAMRules l)
sosToPam rs = concat <$> mapM sosRuleToPam rs


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
abstractPamCfg absFunc abs rules t = transitionGraph (liftM (map abs) . stepPam (map (abstractCompFuncs absFunc) rules))  (abs $ initPamState t)

----------------------------

instance ValueIrrelevance Phase where
  valueIrrelevance p = p

-----------------------------


getPhaseRhs :: PAMRhs l -> Phase
getPhaseRhs (GenAMLetComputation _ _ rhs) = getPhaseRhs rhs
getPhaseRhs (GenAMRhs rhs) = getPhasePAMState rhs

getPhasePAMState :: PAMState l -> Phase
getPhasePAMState (PAMState _ _ p) = p

data ClassifiedPAMRules l = ClassifiedPAMRules { upRules   :: NamedPAMRules l
                                               , compRules :: NamedPAMRules l
                                               , downRules :: NamedPAMRules l
                                               }

classifyPAMRules :: NamedPAMRules l -> ClassifiedPAMRules l
classifyPAMRules rs = if not (null $ filterRulePattern Up Down rs) then
                        error "PAMRules has unexpected up-down rule"
                      else
                        ClassifiedPAMRules { upRules   = filterRulePattern Up   Up   rs
                                           , compRules = filterRulePattern Down Up   rs
                                           , downRules = filterRulePattern Down Down rs
                                           }
  where
    filterRulePattern :: Phase -> Phase -> NamedPAMRules l -> NamedPAMRules l
    filterRulePattern p1 p2 = filter $ \(NamedPAMRule _ (GenAMRule { genAmBefore = before
                                                                   , genAmAfter  = after}))
                                          -> p1 == getPhasePAMState before &&
                                             p2 == getPhaseRhs      after

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM f l = and <$> mapM f l

swapPhase :: PAMState l -> PAMState l
swapPhase (PAMState c k p) = PAMState c k (flipPhase p)

-- Current version: Only for inversion in one transition
--  and assumes the LHS of an up rule has a single variable in term position
--  (and no structure on the conf)
--
-- I think I'm discovering that I actually need unification now rather than matching...
upRulesInvertible :: (Lang l) => NamedPAMRules l -> IO Bool
upRulesInvertible rs = allM upRuleInvertible (upRules $ classifyPAMRules rs)
  where
    -- upRuleInvertible :: NamedPAMRule l -> IO Bool
    upRuleInvertible (NamedPAMRule nm (PAM left (GenAMRhs right))) = do
      debugM $ "Trying to invert rule " ++ BS.unpack nm
      t <- nextVar
      let (PAMState (Conf startT _) _ _) = left
      (startState, upState) <- fmap fromJust $ runMatchUnique $ do
        match (Pattern startT) (Matchee (NonvalVar t))
        startSt <- fillMatch left
        nextSt  <- fillMatch right
        return (startSt, nextSt)

      nextSt <- stepPam1 rs (swapPhase upState)
      case nextSt of
        Nothing -> return False
        Just st -> do res <- fmap fromJust $ runMatchUnique $ alphaEq (swapPhase st) startState
                      debugM $ "Invert successful: " ++ show res
                      return res

