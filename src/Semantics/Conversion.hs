{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TupleSections, TypeApplications, TypeSynonymInstances #-}

module Semantics.Conversion (
    sosToPam

  , upRulesInvertible

  , pamToAM

  , isDeterministic
  ) where


import Control.Monad ( (>=>), void, forM_, liftM, filterM, forM )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Writer ( Writer, tell, execWriter )
import Data.List ( concatMap )
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

unboundifyingVars :: (Matchable f, MonadMatchable m) => m f -> m f
unboundifyingVars x = do va <- liftIO mkNormalVarAllocator
                         withVarAllocator va x


sosRuleToPam' :: (Lang l) => InfNameStream -> PAMState l -> Context l -> PosFrame l -> IO [NamedPAMRule l]
sosRuleToPam' (nm:nms) st k (KBuild c) = do
    rule <- normalize nm st (GenAMRhs $ PAMState c k Up)
    return [rule]
sosRuleToPam' (nm:nms) st k (KStepTo c f@(KInp i pf)) = do
    rule <- normalize nm st (GenAMRhs $ PAMState c (KPush f k) Down)
    (i', pf') <- fromJust <$> (runMatchUnique $ unboundifyingVars $ ((,) <$> refreshVars i <*> fillVars pf))
    rules <- sosRuleToPam' nms (PAMState i' (KPush f k) Up) k pf'
    return (rule : rules)
sosRuleToPam' (nm:nms) st k (KComputation comp f@(KInp i pf)) = do
    (i', pf') <- fromJust <$> (runMatchUnique $ unboundifyingVars $ ((,) <$> refreshVars i <*> fillVars pf))
    rule <- normalize nm st (GenAMLetComputation i' comp (GenAMRhs $ PAMState i' (KPush f k) Down))
    rules <- sosRuleToPam' nms (PAMState i' (KPush f k) Down) k pf'
    return (rule : rules)


sosRuleToPam :: (Lang l) => NamedRule l -> IO [NamedPAMRule l]
sosRuleToPam (NamedRule nam (StepTo conf rhs)) = do
  kv <- nextVar
  let startState = PAMState conf (KVar kv) Down
  fr <- liftM fromJust $ runMatchUnique $ rhsToFrame rhs
  debugM $ "Frame is " ++ show fr
  sosRuleToPam' (infNameStream nam) startState (KVar kv) fr

sosToPam :: (Lang l) => NamedRules l -> IO (NamedPAMRules l)
sosToPam rs = concat <$> mapM sosRuleToPam rs



----------------------------------- Pattern matching on PAM ---------------------------------

fillPAMRhs :: (Lang l) => PAMRhs l -> Match (PAMRhs l)
fillPAMRhs (GenAMLetComputation c comp r) = GenAMLetComputation <$> fillMatch c <*> fillMatch comp <*> fillPAMRhs r
fillPAMRhs (GenAMRhs (GenAMState t c p)) = GenAMRhs <$> (GenAMState <$> fillMatch t <*> fillMatch c <*> pure p)


-- The input term must not have overlapping var names as the rule; we're combining namespaces here
specializeRuleForStartTerm :: (Lang l) => Term l -> PAMRule l -> IO (PAMRule l)
specializeRuleForStartTerm startT (PAM left rhs) = liftM fromJust $ runMatchUnique $ do
  let (GenAMState (Conf leftT leftSt) _ _) = left
  unify leftT startT
  left' <- fillMatch left
  rhs'  <- fillPAMRhs rhs
  return $ PAM left' rhs'

stateUnifies :: (Unifiable (GenAMState t l)) => GenAMState t l -> NamedGenAMRule t l -> IO Bool
stateUnifies st (NamedGenAMRule nm (GenAMRule lhs _)) = debugM ("Unifying with " ++ show nm) >> ( isJust <$> runMatchUnique (unify lhs st))

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
      (PAM startState (GenAMRhs upState)) <- specializeRuleForStartTerm (NonvalVar t) r
      invertRule rs (swapPhase upState) startState

-- Keep taking steps to invert rule until no available matches, or rule is inverted
invertRule :: (Lang l) => NamedPAMRules l -> PAMState l -> PAMState l -> IO Bool
invertRule rs st startState = do
    nextSt <- stepPam1 rs st
    case nextSt of
      Nothing -> return False
      Just nst -> do
          res <- fmap fromJust $ runMatchUnique $ alphaEq (mapVars symbolToVar $ swapPhase nst) startState
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
    (PAM startState (GenAMRhs upState)) <- specializeRuleForStartTerm (ValVar t) r

    nextRules <- findUnifyingLhs rs (swapPhase upState)
    forM nextRules $ \(NamedPAMRule nm2 (PAM left2 rhs2)) -> fromJust <$> (runMatchUnique $ do
      unify left2 (swapPhase upState)
      let nm' = BS.concat [nm1, "-", nm2]
      startState' <- fillMatch startState
      rhs2' <- fillPAMRhs rhs2
      debugM $ "Rule is " ++ show startState' ++ " to " ++ show rhs2'
      return $ NamedAMRule nm' (AM (mapVars symbolToVarLax $ dropPhase startState') (mapVars symbolToVarLax $ dropPhase rhs2')))


-- The sosToPAM conversion algorithm will split a rule like
-- (+(v1,v2),u) ~> let n=+(v1,v2) in (n,u)
--
-- into two rules:
-- <+(v1,v2); u | K> -> let n=+(v1,v2) in <n; u | [\('1, '2) -> ('1, '2)] . K>
-- <v; u | [\('1, '2) -> ('1, '2)] . K> -> <v; u | K>
--
-- This makes the algorithm beautiful, but produces a stupid result (and multiple
-- duplicate rules). We fuse and drop these, not
-- even leaving a trace of the name
--
-- This is not as general as it could be; only merges if the second rule has no computations
mergePAMComputationRule :: (Lang l) => NamedPAMRules l -> NamedPAMRule l -> IO (NamedPAMRule l)
mergePAMComputationRule rs rule@(NamedPAMRule nm (PAM l1 (GenAMLetComputation c f (GenAMRhs r1)))) = do
  nextRules <- maximalLHSs =<< findUnifyingLhs rs r1
  case nextRules of
    [NamedPAMRule _ (PAM l2 (GenAMRhs r2))] -> do
        debugM ("Merging computation rule " ++ show nm)
        fromJust' <$> (runMatchUnique $ do
            debugM ("mergeCompRule: About to match " ++ show l2 ++ " and " ++ show r1)
            match (Pattern l2) (Matchee $ symbolizeVars r1)
            debugM "mergeCompRule: match completed"
            r2' <- fillMatch r2
            return $ NamedPAMRule nm (PAM l1 (GenAMLetComputation c f (GenAMRhs (mapVars symbolToVar r2')))))
    xs  -> debugM ("Won't merge rule matches " ++ concatMap getRName xs ++ ": " ++ show rule) >> return rule
mergePAMComputationRule rs x = return x


fromJust' (Just x) = x
fromJust' Nothing = error "Special fromJust"

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ []     = return False
anyM p (x:xs) = do
        q <- p x
        if q then
          return True
         else
          anyM p xs

maximalLHSs :: (Lang l) => NamedPAMRules l -> IO (NamedPAMRules l)
maximalLHSs []     = return []
maximalLHSs (r:rs) = do b <- anyM (r `subsumed`) rs
                        if b then           debugM ("Was subsumed " ++ show r) >> maximalLHSs rs
                             else (r :) <$> (maximalLHSs =<< (filterM (liftM not . (`subsumed` r)) rs))
  where
    subsumed :: (Lang l) => NamedPAMRule l -> NamedPAMRule l -> IO Bool
    subsumed r1@(NamedPAMRule _ (PAM l1 _)) r2@(NamedPAMRule _ (PAM l2 _)) = do
        debugM ("Trying to subsume " ++ show l1 ++ " by " ++ show l2)
        isJust <$> runMatchUnique (match (Pattern l2) (Matchee $ symbolizeVars l1))


getRName (NamedPAMRule nm _) = show nm

refreshNamedRule :: (Lang l, MonadMatchable m) => NamedPAMRule l -> m (NamedPAMRule l)
refreshNamedRule (NamedPAMRule nm (PAM a b)) = NamedPAMRule nm <$> (PAM <$> refreshVarsPreserveType a
                                                                        <*> refreshVarsPreserveType b)

-- Like nub `on` normalizeVars, except there is no standard nub taking
-- an equality operator as a parameter
uniquifyRules :: (Lang l) => NamedPAMRules l -> IO (NamedPAMRules l)
uniquifyRules rs = go rs
  where
    refr :: (Lang l) => NamedPAMRule l -> IO (PAMRule l)
    refr npr = do NamedPAMRule _ r <- liftM fromJust $ runMatchUnique $ refreshNamedRule npr
                  return r

    go :: (Lang l) => NamedPAMRules l -> IO (NamedPAMRules l)
    go [] = return []
    go (r : rs) = do r' <- refr r
                     rs' <- mapM refr rs
                     if elem r' rs' then
                       go rs
                      else
                       (r :) <$> go rs

pamToAM :: (Lang l) => NamedPAMRules l -> IO (NamedAMRules l)
pamToAM rs = do canTrans <- upRulesInvertible rs
                if not canTrans then
                  error "Up-rules not invertible; cannot convert PAM to abstract machine"
                else do
                  --upRulesSplit <- concat <$> mapM partitionContextVals upRules
                  uniqueRs <- uniquifyRules rs

                  -- This is so janky. (Variables really need namespaces; this relies on coincidence of picking different names)
                  distinctNamesRs <- fromJust <$> runMatchUnique (mapM refreshNamedRule uniqueRs)
                  rs' <- mapM (mergePAMComputationRule distinctNamesRs) uniqueRs

                  let ClassifiedPAMRules { upRules=upRules
                                         , compRules=compRules
                                         , downRules=downRules} = classifyPAMRules rs'

                  upRules' <- concat <$> mapM (mergePAMUpRule rs') upRules
                  return $ upRules' ++ map dropPhase compRules ++ map dropPhase downRules


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
