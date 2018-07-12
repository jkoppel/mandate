{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Semantics.GeneralMachine (
    GenAMRhs(..)
  , runGenAMRhs
  ) where


import Data.Set ( Set )
import qualified Data.Set as Set

import Configuration
import Lang
import Matching
import Semantics.Abstraction
import Semantics.General



data GenAMRhs payload l = GenAMLetComputation (Configuration l) (ExtComp l) (GenAMRhs payload l)
                        | GenAMRhs (payload l)

instance (Lang l, Matchable (payload l)) => Matchable (GenAMRhs payload l) where
  getVars (GenAMLetComputation c f r) = getVars c `Set.union` getVars f `Set.union` getVars r
  getVars (GenAMRhs p) = getVars p

  match (Pattern (GenAMLetComputation c1 f1 r1)) (Matchee (GenAMLetComputation c2 f2 r2)) =
      match (Pattern c1) (Matchee c2) >> match (Pattern f1) (Matchee f2) >> match (Pattern r1) (Matchee r2)
  match (Pattern (GenAMRhs p1)) (Matchee (GenAMRhs p2)) = match (Pattern p1) (Matchee p2)

  refreshVars (GenAMLetComputation c f p) = GenAMLetComputation <$> refreshVars c <*> refreshVars f <*> refreshVars p
  refreshVars (GenAMRhs p) = GenAMRhs <$> refreshVars p

  fillMatch (GenAMLetComputation c f p) = GenAMLetComputation <$> fillMatch c <*> fillMatch f <*> fillMatch p
  fillMatch (GenAMRhs p) = GenAMRhs <$> fillMatch p

instance (Show (Configuration l), Show (payload l), LangBase l) => Show (GenAMRhs payload l) where
  showsPrec d (GenAMLetComputation conf c r) = showString "let " . showsPrec d conf .
                                               showString " = " . showsPrec d c . showString " in " .
                                               showsPrec d r
  showsPrec d (GenAMRhs x) = showsPrec d x


runGenAMRhs :: (LangBase l, Matchable (Configuration l)) => (p l -> Match a) -> GenAMRhs p l -> Match a
runGenAMRhs g (GenAMLetComputation c f r) = do res <- runExtComp f
                                               match (Pattern c) (Matchee res)
                                               runGenAMRhs g r
runGenAMRhs g (GenAMRhs p) = g p


-------------------------------------------------------------------------

instance AbstractCompFuncs (GenAMRhs p l) l where
  abstractCompFuncs abs (GenAMLetComputation c (ExtComp f args) r) = GenAMLetComputation c (ExtComp (abs f) args) r
  abstractCompFuncs _ t@(GenAMRhs _) = t