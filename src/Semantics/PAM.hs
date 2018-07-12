{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, UndecidableInstances #-}

-- | Implementation of "phased abstract machines," a transition system corresponding
--   to reduction (Felleisen-Hieb) semantics
--
-- Wait; nevermind. The "lockstep composition" rule (e1 || e2) -> (e1' || e2') is straightforward to implement in PAM,
-- but I don't see how to do it in reduction semantics. So....I guess then PAM is more powerful than Felleisen-Hieb.

module Semantics.PAM (
    PAMRule(..)
  , NamedPAMRule(..)
  , sosToPam

  , pamEvaluationSequence'
  , pamEvaluationSequence

  , pamEvaluationTreeDepth'
  , pamEvaluationTreeDepth
  , pamEvaluationTree
  , abstractPamCfg
  ) where

import Control.Monad ( MonadPlus(..), guard, liftM )
import Data.Maybe ( fromJust, listToMaybe )
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

pattern PAM :: PAMState l -> PAMRhs l -> PAMRule l
pattern PAM left right = GenAMRule left right

pattern NamedPAMRule :: ByteString -> PAMRule l -> NamedPAMRule l
pattern NamedPAMRule s r = NamedGenAMRule s r

namePAMRule :: ByteString -> PAMRule l -> NamedPAMRule l
namePAMRule = NamedPAMRule

type NamedPAMRules l = [NamedPAMRule l]

type InfNameStream = [ByteString]

infNameStream :: ByteString -> InfNameStream
infNameStream nam = map (\i -> mconcat [nam, "-", BS.pack $ show i]) [1..]

splitFrame :: (Lang l) => PosFrame l -> Context l -> IO (PAMRhs l, Context l, Maybe (Configuration l, PosFrame l))
splitFrame (KBuild c) k = return (GenAMRhs $ PAMState c k Up, k, Nothing)
-- TODO: Why is this so ugly?
splitFrame (KStepTo c f@(KInp i pf)) k = fromJust <$> (runMatchUnique $ do
                                         i' <- refreshVars i
                                         pf' <- fillMatch pf
                                         let f' = KInp i' pf'
                                         let cont = KPush f k
                                         let cont' = KPush (KInp i' pf') k
                                         return (GenAMRhs $ PAMState c cont Down, cont', Just (i, pf')))
splitFrame (KComputation comp (KInp c pf)) k = do (subRhs, ctx, rest) <- splitFrame pf k
                                                  return (GenAMLetComputation c comp subRhs, ctx, rest)

sosRuleToPam' :: (Lang l) => InfNameStream -> PAMState l -> Context l -> PosFrame l -> IO [NamedPAMRule l]
sosRuleToPam' (nm:nms) st k fr = do
    (rhs, k', frRest) <- splitFrame fr k
    restRules <- case frRest of
                   Nothing           -> return []
                   Just (conf', fr') -> sosRuleToPam' nms (PAMState conf' k' Up) k fr'
    let rule = namePAMRule nm $ PAM st rhs
    return (rule : restRules)


sosRuleToPam :: (Lang l) => NamedRule l -> IO [NamedPAMRule l]
sosRuleToPam (NamedRule nam (StepTo conf rhs)) = do
  kv <- nextVar
  let startState = PAMState conf (KVar kv) Down
  sosRuleToPam' (infNameStream nam) startState (KVar kv) (rhsToFrame rhs)


sosToPam :: (Lang l) => NamedRules l -> IO (NamedPAMRules l)
sosToPam rs = concat <$> mapM sosRuleToPam rs


-------------------------------------------------------------------------------------------------------


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

pamEvaluationSequence' :: (Lang l) => NamedPAMRules l -> PAMState l -> IO [PAMState l]
pamEvaluationSequence' rules st = transitionSequence (stepPam1 rules) st

pamEvaluationSequence :: (Lang l) => NamedPAMRules l -> Term l -> IO [PAMState l]
pamEvaluationSequence rules t = pamEvaluationSequence' rules (initPamState t)

pamEvaluationTreeDepth' :: (Lang l, Num a, Eq a) => a ->  NamedPAMRules l -> PAMState l -> IO (Rose (PAMState l))
pamEvaluationTreeDepth' depth rules state = transitionTreeDepth (stepPam rules) depth state

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

-----------------------------


--upRulesInvertible :: (Lang l) => NamedPAMRules l -> IO Bool