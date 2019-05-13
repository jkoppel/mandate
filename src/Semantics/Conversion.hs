{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TupleSections, TypeApplications, TypeSynonymInstances #-}

module Semantics.Conversion (
    sosToPam

  , upRulesInvertible

  , pamToAM

  , isDeterministic
  ) where


import Control.Monad ( (>=>), void, forM_, liftM, filterM, forM )
import Control.Monad.Writer ( Writer, tell, execWriter )
import Data.Maybe ( isJust, fromJust )
import Data.Set ( Set )
import qualified Data.Set as Set

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import Configuration
import Debug
import Lang
import Matching
import Semantics.Abstraction
import Semantics.AbstractMachine
import Semantics.Context
import Semantics.GeneralMachine
import Semantics.PAM
import Semantics.SOS
import Term
import Unification
import Var


----------------------------------- Helpers for SOS-to-PAM conversion -------------------------------

type InfNameStream = [ByteString]

infNameStream :: ByteString -> InfNameStream
infNameStream nam = map (\i -> mconcat [nam, "-", BS.pack $ show i]) [1..]


----------------------------------- SOS to PAM conversion --------------------------------------------
-- The business with prepend is a messy way to deal with IO, there's probably a better way to do it.

prepend :: (Lang l) => NamedPAMRule l -> [NamedPAMRule l] -> IO [NamedPAMRule l]
prepend head rest = return (head : rest)

normalize :: (Lang l) => ByteString -> PAMState l -> PAMRhs l -> IO (NamedPAMRule l) 
normalize nm st rhs = fromJust <$> runMatchUnique (NamedPAMRule nm <$> (PAM <$> normalizeBoundVars st <*> normalizeBoundVars rhs))

sosRuleToPam' :: (Lang l) => InfNameStream -> PAMState l -> Context l -> PosFrame l -> IO [NamedPAMRule l]
sosRuleToPam' (nm:nms) st k (KBuild c) = do
    rule <- normalize nm st (GenAMRhs $ PAMState c k Up)
    return [rule]

sosRuleToPam' (nm:nms) st k (KStepTo c f@(KInp i pf)) = do
    rule <- normalize nm st (GenAMRhs $ PAMState c (KPush f k) Down)
    (sosRuleToPam' nms (PAMState i (KPush f k) Up) k pf) >>= prepend rule  

sosRuleToPam' (nm:nms) st k (KComputation comp f@(KInp c pf)) = do
    rule <- normalize nm st (GenAMLetComputation c comp (GenAMRhs $ PAMState c (KPush f k) Down))
    (sosRuleToPam' nms (PAMState c (KPush f k) Down) k pf) >>= prepend rule

sosRuleToPam :: (Lang l) => NamedRule l -> IO [NamedPAMRule l]
sosRuleToPam (NamedRule nam (StepTo conf rhs)) = do
  kv <- nextVar
  let startState = PAMState conf (KVar kv) Down
  sosRuleToPam' (infNameStream nam) startState (KVar kv) (rhsToFrame rhs)

sosToPam :: (Lang l) => NamedRules l -> IO (NamedPAMRules l)
sosToPam rs = concat <$> mapM sosRuleToPam rs



----------------------------------- Pattern matching on PAM ---------------------------------

fillPAMRhs :: (Lang l) => PAMRhs l -> Match (PAMRhs l)
fillPAMRhs (GenAMLetComputation c comp r) = GenAMLetComputation <$> fillMatch c <*> fillMatch comp <*> fillPAMRhs r
fillPAMRhs (GenAMRhs (GenAMState t c p)) = GenAMRhs <$> (GenAMState <$> fillMatch t <*> fillMatch c <*> pure p)


-- The input term must not have overlapping var names as the rule; we're combining namespaces here
specializeUpRuleForStartTerm :: (Lang l) => Term l -> PAMRule l -> IO (PAMRule l)
specializeUpRuleForStartTerm startT (PAM left rhs) = liftM fromJust $ runMatchUnique $ do
  let (GenAMState (Conf leftT leftSt) _ _) = left
  unify leftT startT
  left' <- fillMatch left
  rhs'  <- fillPAMRhs rhs
  return $ PAM left' rhs'

stateUnifies :: (Unifiable (GenAMState t l)) => GenAMState t l -> NamedGenAMRule t l -> IO Bool
stateUnifies st (NamedGenAMRule _ (GenAMRule lhs _)) = isJust <$> runMatchUnique (unify lhs st)

findUnifyingLhs :: (Lang l) => NamedPAMRules l -> PAMState l -> IO (NamedPAMRules l)
findUnifyingLhs rs st = filterM (stateUnifies st) rs


----------------------------------- Transforming PAM ----------------------------------------


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

flipPhase :: Phase -> Phase
flipPhase Up = Down
flipPhase Down = Up

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
    upRuleInvertible (NamedPAMRule nm r) = do
      debugM $ "Trying to invert rule " ++ BS.unpack nm
      t <- nextVar
      (PAM startState (GenAMRhs upState)) <- specializeUpRuleForStartTerm (NonvalVar t) r
      invertRule rs (swapPhase upState) startState

-- Keep taking steps to invert rule until no available matches, or rule is inverted
invertRule :: (Lang l) => NamedPAMRules l -> PAMState l -> PAMState l -> IO Bool
invertRule rs st startState = do
    nextSt <- stepPam1 rs st
    case nextSt of
      Nothing -> return False
      Just nst -> do
          res <- fmap fromJust $ runMatchUnique $ alphaEq (swapPhase nst) startState
          case res of 
              False -> invertRule rs nst startState
              True  -> return res 


-------------------------------------------------------------------------------------------------------

pamStateToAMState :: PAMState l -> AMState l
pamStateToAMState (PAMState t c _) = AMState t c

class DropPhase f g where
  dropPhase :: f l -> g l

instance DropPhase NamedPAMRule NamedAMRule where
  dropPhase (NamedPAMRule nm p) = NamedAMRule nm (dropPhase p)

instance DropPhase PAMRule AMRule where
  dropPhase (PAM left rhs) = AM (dropPhase left) (dropPhase rhs)

instance DropPhase PAMState AMState where
  dropPhase (PAMState c k _) = AMState c k

instance DropPhase PAMRhs AMRhs where
  dropPhase (GenAMLetComputation c comp r) = GenAMLetComputation c comp (dropPhase r)
  dropPhase (GenAMRhs x) = GenAMRhs (dropPhase x)

mergePAMUpRule :: (Lang l) => NamedPAMRules l -> NamedPAMRule l -> IO (NamedAMRules l)
mergePAMUpRule rs (NamedPAMRule nm1 r) = do
    debugM $ "Trying to merge rule " ++ BS.unpack nm1
    t <- nextVar
    (PAM startState (GenAMRhs upState)) <- specializeUpRuleForStartTerm (ValVar t) r

    nextRules <- findUnifyingLhs rs (swapPhase upState)
    forM nextRules $ \(NamedPAMRule nm2 (PAM left2 rhs2)) -> fromJust <$> (runMatchUnique $ do
      unify left2 (swapPhase upState)
      let nm' = BS.concat [nm1, "-", nm2]
      startState' <- fillMatch startState
      rhs2' <- fillPAMRhs rhs2
      return $ NamedAMRule nm' (AM (dropPhase startState') (dropPhase rhs2')))



pamToAM :: (Lang l) => NamedPAMRules l -> IO (NamedAMRules l)
pamToAM rs = do canTrans <- upRulesInvertible rs
                if not canTrans then
                  error "Up-rules not invertible; cannot convert PAM to abstract machine"
                else do
                  --upRulesSplit <- concat <$> mapM partitionContextVals upRules
                  upRules' <- concat <$> mapM (mergePAMUpRule rs) upRules
                  return $ upRules' ++ map dropPhase compRules ++ map dropPhase downRules
  where
    ClassifiedPAMRules {upRules=upRules, compRules=compRules, downRules=downRules} = classifyPAMRules rs


-------------------------------- Checking determinism --------------------------------------------------

distinctPairs :: [a] -> [(a,a)]
distinctPairs [] = []
distinctPairs (x:xs) = map (x,) xs ++ distinctPairs xs

isDeterministic :: forall t l. (Unifiable (GenAMState t l)) => NamedGenAMRules t l -> IO Bool
isDeterministic rules = null <$> (filterM lhsUnifies (distinctPairs rules))
  where
    -- Type signature needed to prevent overgeneralizing constraint
    lhsUnifies :: (Unifiable (GenAMState t l)) => (NamedGenAMRule t l, NamedGenAMRule t l) -> IO Bool
    lhsUnifies (NamedGenAMRule _ (GenAMRule l _), r) = stateUnifies l r
