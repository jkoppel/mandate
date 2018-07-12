{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Semantics.GeneralMachine (
    AMRhs(..)
  , runAMRhs
  ) where


import Data.Set ( Set )
import qualified Data.Set as Set

import Configuration
import Lang
import Matching
import Semantics.General



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


runAMRhs :: (LangBase l, Matchable (Configuration l)) => (p l -> Match a) -> AMRhs p l -> Match a
runAMRhs g (AMLetComputation c f r) = do res <- runExtComp f
                                         match (Pattern c) (Matchee res)
                                         runAMRhs g r
runAMRhs g (AMRhs p) = g p