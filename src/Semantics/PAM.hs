{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, UndecidableInstances #-}

-- | Implementation of "phased abstract machines," a transition system corresponding
--   to reduction (Felleisen-Hieb) semantics
--
-- Wait; nevermind. The "lockstep composition" rule (e1 || e2) -> (e1' || e2') is straightforward to implement in PAM,
-- but I don't see how to do it in reduction semantics. So....I guess then PAM is more powerful than Felleisen-Hieb.

module Semantics.PAM (
    PAMRule(..)
  , NamedPAMRule(..)
  , sosToPam

  , stepPam
  , pamEvaluationSequence'
  , pamEvaluationSequence

  , pamEvaluationTreeDepth'
  , pamEvaluationTreeDepth
  , pamEvaluationTree
  , abstractPamCfg
  ) where

import Control.Monad ( MonadPlus(..), liftM )
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

data PAMState l = PAMState { pamConf  :: Configuration l
                           , pamK     :: Context l
                           , pamPhase :: Phase
                           }
  deriving ( Eq, Generic )

instance (Lang l) => Hashable (PAMState l)

instance (Lang l) => Matchable (PAMState l) where
  getVars (PAMState c k p) = getVars c `Set.union` getVars k
  match (Pattern (PAMState c1 k1 p1)) (Matchee (PAMState c2 k2 p2))
    | p1 == p2 = match (Pattern c1) (Matchee c2) >> match (Pattern k1) (Matchee k2)
  match _ _ = mzero
  refreshVars (PAMState c k p) = PAMState <$> refreshVars c <*> refreshVars k <*> pure p
  fillMatch   (PAMState c k p) = PAMState <$> fillMatch   c <*> fillMatch   k <*> pure p

instance (Show (Configuration l), Show (Context l)) => Show (PAMState l) where
  showsPrec d (PAMState c k phase) = showString "<" . showsPrec d c . showString " | " .
                                     showsPrec d k . showString "> " . showsPrec d phase

type PAMRhs = GenAMRhs PAMState

data PAMRule l = PAM { pamBefore :: PAMState l
                     , pamAfter  :: PAMRhs l
                     }

instance (Show (Configuration l), LangBase l) => Show (PAMRule l) where
  showsPrec d (PAM before after) = showsPrec d before . showString "  ---->  " . showsPrec d after

data NamedPAMRule l = NamedPAMRule { pamRuleName :: ByteString
                                   , getPamRule  :: PAMRule l
                                   }

instance (Show (Configuration l), LangBase l) => Show (NamedPAMRule l) where
  showsPrec d (NamedPAMRule nm r) = showString (BS.unpack nm) . showString ":\n" . showsPrec (d+1) r
  showList rs = showRules rs

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

runPamRhs :: (Lang l) => PAMRhs l -> Match (PAMState l)
runPamRhs = runGenAMRhs fillMatch

reduceFrame :: (Lang l) => PAMState l -> Match ()
reduceFrame (PAMState c (KPush (KInp i _) _) phase) = do
  c' <- fillMatch c
  match (Pattern i) (Matchee c')

reduceFrame pat = return ()

usePamRule :: (Lang l) => NamedPAMRule l -> PAMState l -> Match (PAMState l)
usePamRule (NamedPAMRule nm (PAM left right)) st = do
    -- NOTE: I don't really know how to do higher-order matching, but I think what I did is reasonable
    ---- The trick is the special match rule for Frame, which clears bound variables from the context.

    debugM $ "Trying rule " ++ BS.unpack nm ++ " for state " ++ show st
    match (Pattern left) (Matchee st)
    reduceFrame left
    debugM $ "LHS matched: " ++ BS.unpack nm
    ret <- runPamRhs right
    debugM $ "Rule succeeeded:" ++ BS.unpack nm
    return ret

useBaseRule :: (Lang l) => PAMState l -> Match (PAMState l)
useBaseRule (PAMState conf KHalt Up) = do debugM "Step completed; moving to next Step"
                                          return (PAMState conf KHalt Down)
--useBaseRule (PAMState c@(Conf (Val _ _) _) k Down) = do debugM "Value reached; going up"
--                                                        return (PAMState c k Up)
useBaseRule _ = mzero

stepPam1 :: (Lang l) => NamedPAMRules l -> PAMState l -> Match (PAMState l)
stepPam1 allRs st = useBaseRule st `mplus` go allRs
  where
    go []     = mzero
    go (r:rs) = usePamRule r st `mplus` go rs


initPamState :: (Lang l) => Term l -> PAMState l
initPamState t = PAMState (initConf t) KHalt Down

pamEvaluationSequence' :: (Lang l) => NamedPAMRules l -> PAMState l -> IO [PAMState l]
pamEvaluationSequence' rules st = transitionSequence step st
  where
    step = liftM listToMaybe . runMatch . stepPamUnlessDone

    stepPamUnlessDone (PAMState (Conf (Val _ _) _) KHalt Up) = mzero
    stepPamUnlessDone x = stepPam1 rules x

pamEvaluationSequence :: (Lang l) => NamedPAMRules l -> Term l -> IO [PAMState l]
pamEvaluationSequence rules t = pamEvaluationSequence' rules (initPamState t)


stepPam :: (Lang l) => NamedPAMRules l -> PAMState l -> IO [PAMState l]
stepPam allRs st = (++) <$> (runMatch (useBaseRule st)) <*> go allRs
  where
    go []     = return []
    go (r:rs) = (++) <$> (runMatch (usePamRule r st)) <*> go rs

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

instance (ValueIrrelevance (Configuration l), ValueIrrelevance (Context l)) => ValueIrrelevance (PAMState l) where
  valueIrrelevance (PAMState conf ctx phase) = PAMState (valueIrrelevance conf) (valueIrrelevance ctx) phase

class AbstractCompFuncs t l where
  abstractCompFuncs :: Abstraction (CompFunc l) -> t -> t

instance AbstractCompFuncs (NamedPAMRule l) l where
  abstractCompFuncs abs (NamedPAMRule nm r) = NamedPAMRule nm (abstractCompFuncs abs r)

instance AbstractCompFuncs (PAMRule l) l where
  abstractCompFuncs abs (PAM l r) = PAM l (abstractCompFuncs abs r)

instance AbstractCompFuncs (GenAMRhs p l) l where
  abstractCompFuncs abs (GenAMLetComputation c (ExtComp f args) r) = GenAMLetComputation c (ExtComp (abs f) args) r
  abstractCompFuncs _ t@(GenAMRhs _) = t


-----------------------------


--upRulesInvertible :: (Lang l) => NamedPAMRules l -> IO Bool