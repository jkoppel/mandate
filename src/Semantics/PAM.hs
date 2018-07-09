{-# LANGUAGE FlexibleContexts, OverloadedStrings, UndecidableInstances #-}

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

  , happyPath -- TODO: Move
  ) where

import Control.Monad ( MonadPlus(..) )
import Data.Maybe ( fromJust, catMaybes )
import Data.Monoid ( Monoid(..), )
import Data.Set ( Set )
import qualified Data.Set as Set

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import Configuration
import Debug
import Lang
import Matching
import Semantics.Context
import Semantics.General
import Semantics.SOS
import Term
import Var

data Phase = Up | Down
  deriving (Eq, Ord)

instance Show Phase where
  showsPrec _ Up   = showString "up"
  showsPrec _ Down = showString "down"

data AMRhs payload l = AMLetComputation (Configuration l) (ExtComp l) (AMRhs payload l)
                     | AMRhs (payload l)


instance (Lang l, Matchable (payload l)) => Matchable (AMRhs payload l) where
  getVars (AMLetComputation c f r) = getVars c `Set.union` getVars f `Set.union` getVars r
  getVars (AMRhs p) = getVars p

  match (Pattern (AMLetComputation c1 f1 r1)) (Matchee (AMLetComputation c2 f2 r2)) =
      match (Pattern c1) (Matchee c2) >> match (Pattern f1) (Matchee f2) >> match (Pattern r1) (Matchee r2)
  match (Pattern (AMRhs p1)) (Matchee (AMRhs p2)) = match (Pattern p1) (Matchee p2)

  refreshVars (AMLetComputation c f p) = AMLetComputation <$> refreshVars c <*> refreshVars f <*> refreshVars p
  refreshVars (AMRhs p) = AMRhs <$> refreshVars p

  fillMatch (AMLetComputation c f p) = AMLetComputation <$> fillMatch c <*> fillMatch f <*> fillMatch p
  fillMatch (AMRhs p) = AMRhs <$> fillMatch p

instance (Show (Configuration l), Show (payload l), LangBase l) => Show (AMRhs payload l) where
  showsPrec d (AMLetComputation conf c r) = showString "let " . showsPrec d conf .
                                            showString " = " . showsPrec d c . showString " in " .
                                            showsPrec d r
  showsPrec d (AMRhs x) = showsPrec d x


data PAMState l = PAMState { pamConf  :: Configuration l
                           , pamK     :: Context l
                           , pamPhase :: Phase
                           }

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

type PAMRhs = AMRhs PAMState

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
splitFrame (KBuild c) k = return (AMRhs $ PAMState c k Up, k, Nothing)
-- TODO: Why is this so ugly?
splitFrame (KStepTo c f@(KInp i pf)) k = fromJust <$> (runMatch $ do
                                         i' <- refreshVars i
                                         pf' <- fillMatch pf
                                         let f' = KInp i' pf'
                                         let cont = KPush f k
                                         let cont' = KPush (KInp i' pf') k
                                         return (AMRhs $ PAMState c cont Down, cont', Just (i, pf')))
splitFrame (KComputation comp (KInp c pf)) k = do (subRhs, ctx, rest) <- splitFrame pf k
                                                  return (AMLetComputation c comp subRhs, ctx, rest)

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
runPamRhs (AMLetComputation c f r) = do res <- runExtComp f
                                        match (Pattern c) (Matchee res)
                                        runPamRhs r
runPamRhs (AMRhs st) = fillMatch st

reduceFrame :: (Lang l) => PAMState l -> Match ()
reduceFrame (PAMState c (KPush (KInp i _) _) phase) = do
  c' <- fillMatch c
  match (Pattern i) (Matchee c')

reduceFrame pat = return ()

usePamRule :: (Lang l) => NamedPAMRule l -> PAMState l -> Match (PAMState l)
usePamRule (NamedPAMRule nm (PAM left right)) st = do
    -- NOTE: I don't really know how to do higher-order matching, but I think what I did is reasonable
    ---- The trick is the special match rule for Frame, which clears bound variables from the context.

    debugStepM $ "Trying rule " ++ BS.unpack nm ++ " for state " ++ show st
    match (Pattern left) (Matchee st)
    reduceFrame left
    debugStepM $ "LHS matched: " ++ BS.unpack nm
    ret <- runPamRhs right
    debugStepM $ "Rule succeeeded:" ++ BS.unpack nm
    return ret

useBaseRule :: (Lang l) => PAMState l -> Match (PAMState l)
useBaseRule (PAMState conf KHalt Up) = do debugStepM "Step completed; moving to next Step"
                                          return (PAMState conf KHalt Down)
--useBaseRule (PAMState c@(Conf (Val _ _) _) k Down) = do debugStepM "Value reached; going up"
--                                                        return (PAMState c k Up)
useBaseRule _ = mzero

stepPam :: (Lang l) => NamedPAMRules l -> PAMState l -> Match (PAMState l)
stepPam allRs st = useBaseRule st `mplus` go allRs
  where
    go []     = mzero
    go (r:rs) = usePamRule r st `mplus` go rs


initPamState :: (Lang l) => Term l -> PAMState l
initPamState t = PAMState (initConf t) KHalt Down

pamEvaluationSequence' :: (Lang l) => NamedPAMRules l -> PAMState l -> IO [PAMState l]
pamEvaluationSequence' rules st = go st
  where
    go st = (st :) <$> do mst' <- runMatch $ stepPam rules st
                          case mst' of
                            Nothing  -> return []
                            Just st'@(PAMState (Conf (Val _ _) _) KHalt Up) -> return [st']
                            Just st' -> pamEvaluationSequence' rules st'


pamEvaluationSequence :: (Lang l) => NamedPAMRules l -> Term l -> IO [PAMState l]
pamEvaluationSequence rules t = pamEvaluationSequence' rules (initPamState t)


data Rose a = Rose { roseGetElt :: a, roseGetChildren ::  [Rose a] }

addDepth :: Rose a -> Rose (a, Int)
addDepth (Rose a rs) = Rose (a, 1 + maximum ([-1] ++ map (snd.roseGetElt) rs')) rs'
  where
    rs' = map addDepth rs

happyPath :: Rose a -> [a]
happyPath rose = go (addDepth rose)
  where
    go (Rose (a, 0) []) = [a]
    go (Rose (a, d) rs) = a : (go $ head $ filter (\t -> (snd $ roseGetElt t) == d-1) rs)

instance Show a => Show (Rose a) where
  showsPrec d (Rose a rs) = showString (concat $ replicate d "--") . showsPrec 0 a . showString "\n" .
                            foldr (.) id (map (showsPrec (d+1)) rs)

pamEvaluationTreeDepth' :: (Lang l, Num a, Eq a) => a ->  NamedPAMRules l -> PAMState l -> IO (Rose (PAMState l))
pamEvaluationTreeDepth' 0     _     state = return (Rose state [])
pamEvaluationTreeDepth' depth rules state = go state
  where
    go st = Rose st <$> do tries <- sequence $ map runMatch $ [useBaseRule st] ++ (map (flip usePamRule state) rules)
                           mapM (pamEvaluationTreeDepth' (depth-1) rules) $ catMaybes tries


pamEvaluationTreeDepth :: (Lang l, Num a, Eq a) => a -> NamedPAMRules l -> Term l -> IO (Rose (PAMState l))
pamEvaluationTreeDepth depth rules t = pamEvaluationTreeDepth' depth rules (initPamState t)

pamEvaluationTree :: (Lang l) => NamedPAMRules l -> Term l -> IO (Rose (PAMState l))
pamEvaluationTree = pamEvaluationTreeDepth infty
  where
    infty = read "Infinity" :: Float