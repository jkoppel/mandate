{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, PatternSynonyms, TypeOperators, UndecidableInstances #-}

module Semantics (
    StepTo(..)
  , NamedRule(..)
  , Rhs(..)
  , Rules
  , NamedRules
  , MRules

  , stepTerm
  , evaluationSequence

  , name
  , mkRule0, mkRule1, mkRule2
  , mkRule3, mkRule4, mkRule5
  , mkRule6

  , checkRule
  , checkRules
  ) where

import Control.DeepSeq ( deepseq )

import Control.Monad ( MonadPlus(..), guard )
import Control.Monad.Trans ( lift )
import Data.List ( intersperse )
import Data.Typeable ( Typeable )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import Configuration
import Debug
import Matching
import Term
import Var

------------------------------------------------------------------------------------------------------------------

-- Match configuration
type MConf l = Configuration l Open

-- I don't like the need to spread these Typeable instances.
-- This caps the propagation of constraints. There's no particular reason to put
-- it here vs. elsewhere
data StepTo l where
 StepTo :: (Typeable l) => MConf l -> Rhs l -> StepTo l

data NamedRule l = NamedRule {ruleName :: ByteString, getRule :: StepTo l}

-- TODO: Can't depend on state
type ExtFunc l r = ([MetaVar], [Term l Closed] -> r)

--TODO: How to get rid of this vacuous Typeable instances?
runExtFunc :: (Typeable l) => ExtFunc l r -> Match r
runExtFunc (vs, f) = f <$> getVars vs

data Rhs l = Build (MConf l)
           | SideCondition (ExtFunc l Bool) (Rhs l)
           | LetStepTo (MConf l) (MConf l) (Rhs l) -- let (x,mu) = stepto(T,mu) in R
           | LetComputation MetaVar (ExtFunc l (Term l Closed)) (Rhs l) -- let x = f(T) in R

type Rules l = [StepTo l]
type NamedRules l = [NamedRule l]
type MRules l = IO (Rules l)


----
-- This is me having fun learning how to write Show instances and use showsPrec.
-- This is not an endorsement of having this kind of human-readable displaying happen
-- in Show, as opposed to in a separate Pretty class

instance (Show (MConf l)) => Show (StepTo l) where
  showsPrec d (StepTo t r) = showString "step(" . showsPrec (d+1) t . showString ") = " . showsPrec (d+1) r

  showList rs = showString "Begin Rules:\n\n" .
                foldr (.) id (intersperse (showString "\n\n") $ map (showsPrec 0) rs) .
                showString "\n\nEnd Rules"

instance (Show (MConf l)) => Show (Rhs l) where
  showsPrec d (Build t) = showsPrec (d+1) t
  showsPrec d (SideCondition _ r) = showString "guard <some func>, " . showsPrec d r
  showsPrec d (LetStepTo x e r) = showString "let " . showsPrec (d+1) x .
                                  showString " = step(" . showsPrec (d+1) e .
                                  showString ") in " .
                                  showsPrec d r
  showsPrec d (LetComputation x (ms, _) r) = showString "let " . showsPrec (d+1) x . showString " = " .
                                             showString "someFunc(" . showsPrec (d+1) ms .
                                             showString ") in " . showsPrec d r


instance (Show (MConf l)) => Show (NamedRule l) where
  showsPrec d (NamedRule nm r) = showString (BS.unpack nm) . showString ":\n" . showsPrec (d+1) r
  showList rs = showString "Begin Rules:\n\n" .
                foldr (.) id (intersperse (showString "\n\n") $ map (showsPrec 0) rs) .
                showString "\nEnd Rules"




-------------------------------- Execution ------------------------------

runRhs :: (Matchable (Configuration l), Typeable l) => NamedRules l -> Rhs l -> Match (Configuration l Closed)
runRhs rs (Build c) = fillMatch c
runRhs rs (SideCondition f r) = do guard =<< runExtFunc f
                                   runRhs rs r
runRhs rs (LetStepTo c1 c2 r) = do c2Filled <- fillMatch c2
                                   debugStepM $ "Filled match succeeded: " ++ show (confTerm c2Filled)
                                   c2' <- lift $ stepTerm rs c2Filled
                                   debugStepM $ "Recursive step suceeded. Result: " ++ show (confTerm c2')
                                   match c1 c2'
                                   runRhs rs r
runRhs rs (LetComputation v f r) = do putVar v =<< runExtFunc f
                                      runRhs rs r


useRule :: (Matchable (Configuration l)) => NamedRules l -> NamedRule l -> Configuration l Closed -> Match (Configuration l Closed)
useRule rs (NamedRule nm (StepTo c1 r)) c2 = do
    debugStepM $ "Trying rule " ++ BS.unpack nm ++ " for term " ++ show (confTerm c2)
    match c1 c2
    debugStepM $ "LHS matched: " ++ BS.unpack nm
    ret <- runRhs rs r
    debugStepM $ "Rule succeeeded:" ++ BS.unpack nm
    return ret

stepTerm :: (Matchable (Configuration l)) => NamedRules l -> Configuration l Closed -> Maybe (Configuration l Closed)
stepTerm allRs t = runMatch $ go allRs
  where
    go []     = mzero
    go (r:rs) = useRule allRs r t `mplus` go rs


evaluationSequence :: (Matchable (Configuration l)) => NamedRules l -> Configuration l Closed -> [Configuration l Closed]
evaluationSequence rules c = c : case stepTerm rules c of
                                   Nothing -> []
                                   Just c' -> evaluationSequence rules c'

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

-------------------------------- Sort checking ------------------------------

-- Current impl does not track MetaVar sorts, nor does it check the RedState
-- Unfortunately, so far, messing up the MetaVar sort is 1/1 of my rule errors, so
-- don't let this give you a false sense of security!

checkRhs :: Signature l -> Rhs l -> ()
checkRhs sig (Build conf) = checkTerm sig (confTerm conf)
checkRhs sig (SideCondition _ r) = checkRhs sig r
checkRhs sig (LetStepTo c1 c2 r) = checkTerm sig (confTerm c1) `seq` checkTerm sig (confTerm c2) `seq` checkRhs sig r
checkRhs sig (LetComputation _ _ r) = checkRhs sig r

checkRule :: Signature l -> StepTo l -> ()
checkRule sig (StepTo conf rhs) = checkTerm sig (confTerm conf) `seq` checkRhs sig rhs

checkRules :: Signature l -> Rules l -> ()
checkRules sig []     = ()
checkRules sig (r:rs) = checkRule sig r `seq` checkRules sig rs