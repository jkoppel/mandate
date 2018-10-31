{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, UndecidableInstances #-}

module Semantics.SOS (
    StepTo(..)
  , NamedRule(..)
  , Rhs(..)
  , Rules
  , NamedRules

  , stepConf
  , evaluationSequence

  , name
  , mkRule0, mkRule1, mkRule2
  , mkRule3, mkRule4, mkRule5
  , mkRule6
  , mkPairRule1, mkPairRule2

  , checkRule
  , checkRules
  ) where

import Control.Monad ( MonadPlus(..) )
import Data.Typeable ( Typeable )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import Configuration
import Debug
import LangBase
import Matching
import Semantics.General
import Term
import TransitionSystem
import Var

------------------------------------------------------------------------------------------------------------------

-- I don't like the need to spread these Typeable instances.
-- This caps the propagation of constraints. There's no particular reason to put
-- it here vs. elsewhere
data StepTo l where
 StepTo :: (Typeable l) => Configuration l -> Rhs l -> StepTo l

data NamedRule l = NamedRule {ruleName :: ByteString, getRule :: StepTo l}

data Rhs l = Build (Configuration l)
           | LetStepTo (Configuration l) (Configuration l) (Rhs l) -- let (x,mu) = stepto(T,mu) in R
           | LetComputation (Configuration l) (ExtComp l) (Rhs l) -- let x = f(T) in R

type Rules l = [StepTo l]
type NamedRules l = [NamedRule l]

----
-- This is me having fun learning how to write Show instances and use showsPrec.
-- This is not an endorsement of having this kind of human-readable displaying happen
-- in Show, as opposed to in a separate Pretty class

instance (Show (Configuration l), LangBase l) => Show (StepTo l) where
  showsPrec d (StepTo t r) = showString "step(" . showsPrec (d+1) t . showString ") = " . showsPrec (d+1) r
  showList rs = showRules rs

instance (Show (Configuration l), LangBase l) => Show (Rhs l) where
  showsPrec d (Build t) = showsPrec (d+1) t
  showsPrec d (LetStepTo x e r) = showString "let " . showsPrec (d+1) x .
                                  showString " = step(" . showsPrec (d+1) e .
                                  showString ") in " .
                                  showsPrec d r
  showsPrec d (LetComputation x c r) = showString "let " . showsPrec (d+1) x . showString " = " .
                                       showsPrec d c . showString " in " . showsPrec d r


instance (Show (Configuration l), LangBase l) => Show (NamedRule l) where
  showsPrec d (NamedRule nm r) = showString (BS.unpack nm) . showString ":\n" . showsPrec (d+1) r
  showList rs = showRules rs




-------------------------------- Execution ------------------------------

runRhs :: (Matchable (Configuration l), LangBase l) => NamedRules l -> Rhs l -> Match (Configuration l)
runRhs rs (Build c) = fillMatch c
runRhs rs (LetStepTo c1 c2 r) = do c2Filled <- fillMatch c2
                                   debugM $ "Filled match succeeded: " ++ show (confTerm c2Filled)
                                   c2' <- withFreshCtx $ stepConf rs c2Filled
                                   debugM $ "Recursive step suceeded. Result: " ++ show (confTerm c2')
                                   match (Pattern c1) (Matchee c2')
                                   runRhs rs r
runRhs rs (LetComputation c f r) = do res <- runExtComp f
                                      match (Pattern c) (Matchee res)
                                      runRhs rs r


useRule :: (Matchable (Configuration l), LangBase l) => NamedRules l -> NamedRule l -> Configuration l -> Match (Configuration l)
useRule rs (NamedRule nm (StepTo c1 r)) c2 = do
    debugM $ "Trying rule " ++ BS.unpack nm ++ " for term " ++ show (confTerm c2)
    match (Pattern c1) (Matchee c2)
    debugM $ "LHS matched: " ++ BS.unpack nm
    ret <- runRhs rs r
    debugM $ "Rule succeeeded:" ++ BS.unpack nm
    return ret

stepConf :: (Matchable (Configuration l), LangBase l) => NamedRules l -> Configuration l -> Match (Configuration l)
stepConf allRs t = go allRs
  where
    go []     = mzero
    go (r:rs) = useRule allRs r t `mplus` go rs


evaluationSequence :: (Matchable (Configuration l), LangBase l) => NamedRules l -> Configuration l -> IO [Configuration l]
evaluationSequence rules conf = transitionSequence step conf
  where
    step = runMatchUnique . stepConf rules

-------------------------------- Helpers for creating rules ------------------------------

name :: (Monad m) => ByteString -> m (StepTo l) -> m (NamedRule l)
name s r = NamedRule s <$> r

mkRule0 :: StepTo l -> IO (StepTo l)
mkRule0 = return

mkRule1 :: (MetaVar -> StepTo l) -> IO (StepTo l)
mkRule1 f = f <$> nextVar

mkRule2 :: (MetaVar -> MetaVar -> StepTo l) -> IO (StepTo l)
mkRule2 f = f <$> nextVar <*> nextVar

mkRule3 :: (MetaVar -> MetaVar -> MetaVar -> StepTo l) -> IO (StepTo l)
mkRule3 f = f <$> nextVar <*> nextVar <*> nextVar

mkRule4 :: (MetaVar -> MetaVar -> MetaVar -> MetaVar -> StepTo l) -> IO (StepTo l)
mkRule4 f = f <$> nextVar <*> nextVar <*> nextVar <*> nextVar

mkRule5 :: (MetaVar -> MetaVar -> MetaVar -> MetaVar -> MetaVar -> StepTo l) -> IO (StepTo l)
mkRule5 f = f <$> nextVar <*> nextVar <*> nextVar <*> nextVar <*> nextVar

mkRule6 :: (MetaVar -> MetaVar -> MetaVar -> MetaVar -> MetaVar -> MetaVar -> StepTo l) -> IO (StepTo l)
mkRule6 f = f <$> nextVar <*> nextVar <*> nextVar <*> nextVar <*> nextVar <*> nextVar

mkPairRule1 :: ((MetaVar, MetaVar) -> IO (StepTo l)) -> IO (StepTo l)
mkPairRule1 f = do
  car <- nextVar
  cdr <- nextVar
  f (car, cdr)

mkPairRule2 :: ((MetaVar, MetaVar) -> (MetaVar, MetaVar) -> IO (StepTo l)) -> IO (StepTo l)
mkPairRule2 f = do
  car1 <- nextVar
  cdr1 <- nextVar
  car2 <- nextVar
  cdr2 <- nextVar
  f (car1, cdr1) (car2, cdr2)

-------------------------------- Sort checking ------------------------------

-- Current impl does not track MetaVar sorts, nor does it check the RedState
-- Unfortunately, so far, messing up the MetaVar sort is 1/1 of my rule errors, so
-- don't let this give you a false sense of security!

checkRhs :: Signature l -> Rhs l -> ()
checkRhs sig (Build conf) = checkTerm sig (confTerm conf)
checkRhs sig (LetStepTo c1 c2 r) = checkTerm sig (confTerm c1) `seq` checkTerm sig (confTerm c2) `seq` checkRhs sig r
checkRhs sig (LetComputation _ _ r) = checkRhs sig r

checkRule :: Signature l -> StepTo l -> ()
checkRule sig (StepTo conf rhs) = checkTerm sig (confTerm conf) `seq` checkRhs sig rhs

checkRules :: Signature l -> Rules l -> ()
checkRules sig []     = ()
checkRules sig (r:rs) = checkRule sig r `seq` checkRules sig rs