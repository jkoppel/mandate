{-# LANGUAGE OverloadedStrings #-}

module Semantics.Conversion (
    sosToPam

  , upRulesInvertible

  , pamToAM
  ) where


import Data.Maybe ( fromJust )

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
import Var


----------------------------------- Helpers for SOS-to-PAM conversion -------------------------------

type InfNameStream = [ByteString]

infNameStream :: ByteString -> InfNameStream
infNameStream nam = map (\i -> mconcat [nam, "-", BS.pack $ show i]) [1..]


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
    rule <- fromJust <$> runMatchUnique (NamedPAMRule nm <$> (PAM <$> normalizeBoundVars st <*> normalizeBoundVars rhs))
    return (rule : restRules)


sosRuleToPam :: (Lang l) => NamedRule l -> IO [NamedPAMRule l]
sosRuleToPam (NamedRule nam (StepTo conf rhs)) = do
  kv <- nextVar
  let startState = PAMState conf (KVar kv) Down
  sosRuleToPam' (infNameStream nam) startState (KVar kv) (rhsToFrame rhs)


sosToPam :: (Lang l) => NamedRules l -> IO (NamedPAMRules l)
sosToPam rs = concat <$> mapM sosRuleToPam rs



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


-------------------------------------------------------------------------------------------------------

mergePAMRule :: (Lang l) => NamedPAMRules l -> NamedPAMRule l -> IO (NamedAMRule l)
mergePAMRule rs (NamedPAMRule _ (PAM left rhs)) = error "todo"

pamToAM :: (Lang l) => NamedPAMRules l -> IO (NamedAMRules l)
pamToAM rs = do canTrans <- upRulesInvertible rs
                if not canTrans then
                  error "Up-rules not invertible; cannot convert PAM to abstract machine"
                else
                  mapM (mergePAMRule rs) rs