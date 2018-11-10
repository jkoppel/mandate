{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeSynonymInstances #-}

module Semantics.Conversion (
    sosToPam

  , upRulesInvertible

  , pamToAM
  ) where


import Control.Monad ( (>=>), void, forM_, liftM )
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



----------------------------------- Pattern matching on PAM ---------------------------------

fillPAMRhs :: (Lang l) => PAMRhs l -> Match (PAMRhs l)
fillPAMRhs (GenAMLetComputation c comp r) = GenAMLetComputation <$> fillMatch c <*> fillMatch comp <*> fillPAMRhs r
fillPAMRhs (GenAMRhs (GenAMState t c p)) = GenAMRhs <$> (GenAMState <$> fillMatch t <*> fillMatch c <*> pure p)


-- The input term must not have overlapping var names as the rule; we're combining namespaces here
-- This also only works if the LHS term is a single variable, which is the case in up rules
specializeUpRuleForStartTerm :: (Lang l) => Term l -> PAMRule l -> IO (PAMRule l)
specializeUpRuleForStartTerm startT (PAM left rhs) = liftM fromJust $ runMatchUnique $ do
  let (GenAMState (Conf leftT leftSt) _ _) = left
  match (Pattern leftT) (Matchee startT)
  left' <- fillMatch left
  rhs'  <- fillPAMRhs rhs
  return $ PAM left' rhs'

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM f l = go l
  where
    go []     = return Nothing
    go (x:xs) = do b <- f x
                   if b then return (Just x) else go xs

lhsMatches :: (Lang l) => PAMState l -> NamedPAMRule l -> IO Bool
lhsMatches st (NamedPAMRule _ (PAM lhs _)) = isJust <$> runMatchUnique (match (Pattern lhs) (Matchee st))

findMatchingLhs :: (Lang l) => NamedPAMRules l -> PAMState l -> IO (Maybe (NamedPAMRule l))
findMatchingLhs rs st = findM (lhsMatches st) rs


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

      nextSt <- stepPam1 rs (swapPhase upState)
      case nextSt of
        Nothing -> return False
        Just st -> do res <- fmap fromJust $ runMatchUnique $ alphaEq (swapPhase st) startState
                      debugM $ "Invert successful: " ++ show res
                      return res


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


-- FIXME: Utter deadline hack; I think it's time to add some real generic programming to Mandate
class GetKnownMTVars f where
  getKnownMTVars' :: f l -> Writer (Set MetaVar) ()

instance GetKnownMTVars Context where
  getKnownMTVars' (KVar _) = return ()
  getKnownMTVars' KHalt = return ()
  getKnownMTVars' (KPush f k) = getKnownMTVars' f >> getKnownMTVars' k

instance GetKnownMTVars Frame where
  getKnownMTVars' (KInp _ pf) = getKnownMTVars' pf

instance GetKnownMTVars PosFrame where
  getKnownMTVars' (KBuild (Conf t _)) = void $ traverseTerm outputKnownMTVar t
    where
      outputKnownMTVar t@(ValVar    v) = tell (Set.singleton v) >> return t
      outputKnownMTVar t@(NonvalVar v) = tell (Set.singleton v) >> return t
      outputKnownMTVar t               = return t

getKnownMTVars :: Context l -> Set MetaVar
getKnownMTVars k = execWriter $ getKnownMTVars' k

partitionContextVals :: forall l. (Lang l) => NamedPAMRule l -> IO (NamedPAMRules l)
partitionContextVals (NamedPAMRule nm (PAM (PAMState c k p) rhs)) = liftM (map (NamedPAMRule nm)) $ runMatch $ do
  let cVars = getVars c
  let kTermVarsRaw = freeTermVars k
  let kTermVars = Set.toList ((kTermVarsRaw `Set.difference` cVars) `Set.difference` getKnownMTVars k)

  forM_ kTermVars $ \v -> do
    mt <- matchChoose [ValueOnly, NonvalOnly]
    putVar v (GMetaVar @l v mt)

  PAM <$> (PAMState c <$> fillMatch k <*> pure p) <*> fillPAMRhs rhs


mergePAMUpRule :: (Lang l) => NamedPAMRules l -> NamedPAMRule l -> IO (NamedAMRule l)
mergePAMUpRule rs (NamedPAMRule nm1 r) = do
    debugM $ "Trying to merge rule " ++ BS.unpack nm1
    t <- nextVar
    (PAM startState (GenAMRhs upState)) <- specializeUpRuleForStartTerm (ValVar t) r

    nextRule <- findMatchingLhs rs (swapPhase upState)
    case nextRule of
      Nothing -> return $ NamedAMRule nm1 (AM (dropPhase startState) (GenAMRhs $ dropPhase upState))
      Just (NamedPAMRule nm2 (PAM left2 rhs2)) -> fromJust <$> (runMatchUnique $ do
        match (Pattern left2) (Matchee (swapPhase upState))
        let nm' = BS.concat [nm1, "-", nm2]
        rhs2' <- fillPAMRhs rhs2
        return $ NamedAMRule nm' (AM (dropPhase startState) (dropPhase rhs2')))



pamToAM :: (Lang l) => NamedPAMRules l -> IO (NamedAMRules l)
pamToAM rs = do canTrans <- upRulesInvertible rs
                if not canTrans then
                  error "Up-rules not invertible; cannot convert PAM to abstract machine"
                else do
                  upRulesSplit <- concat <$> mapM partitionContextVals upRules
                  upRules' <- mapM (mergePAMUpRule rs) upRulesSplit
                  return $ upRules' ++ map dropPhase compRules ++ map dropPhase downRules
  where
    ClassifiedPAMRules {upRules=upRules, compRules=compRules, downRules=downRules} = classifyPAMRules rs