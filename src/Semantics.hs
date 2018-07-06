{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, PatternSynonyms, TypeOperators, UndecidableInstances #-}

module Semantics (
    StepTo(..)
  , Rhs(..)
  , Rules
  , MRules

  , stepTerm

  , mkRule0, mkRule1, mkRule2
  , mkRule3, mkRule4, mkRule5
  ) where

import Control.Monad ( MonadPlus(..), guard )
import Control.Monad.Trans ( lift )
import Data.List ( intersperse )
import Data.Typeable ( Typeable )

import Configuration
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
type MRules l = IO (Rules l)


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


-------------------------------- Execution ------------------------------

runRhs :: (Matchable (Configuration l), Typeable l) => Rules l -> Rhs l -> Match (Configuration l Closed)
runRhs rs (Build c) = fillMatch c
runRhs rs (SideCondition f r) = do guard =<< runExtFunc f
                                   runRhs rs r
runRhs rs (LetStepTo c1 c2 r) = do c2Filled <- fillMatch c2
                                   c2' <- lift $ stepTerm rs c2Filled
                                   match c1 c2'
                                   runRhs rs r
runRhs rs (LetComputation v f r) = do putVar v =<< runExtFunc f
                                      runRhs rs r


useRule :: (Matchable (Configuration l)) => Rules l -> StepTo l -> Configuration l Closed -> Match (Configuration l Closed)
useRule rs (StepTo c1 r) c2 = match c1 c2 >> runRhs rs r

stepTerm :: (Matchable (Configuration l)) => Rules l -> Configuration l Closed -> Maybe (Configuration l Closed)
stepTerm rs t = runMatch $ go rs
  where
    go []     = mzero
    go (r:rs) = useRule rs r t `mplus` go rs


-------------------------------- Helpers for creating rules ------------------------------


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
