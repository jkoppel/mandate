{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, PatternSynonyms, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Semantics (
    RedState
  , GConfiguration(..)
  , Configuration

  , StepTo(..)
  , Rhs(..)
  , Rules
  , MRules

  , mkRule0, mkRule1, mkRule2
  , mkRule3, mkRule4, mkRule5
  ) where

import Data.List ( intersperse )

import Matching
import Term
import Var

------------------------------------------------------------------------------------------------------------------

-------------------------------- Configurations ------------------------------

type family RedState l :: OpenClosed -> *
data GConfiguration l s v = Conf { confTerm :: Term l v, confState :: s v}
  deriving (Eq, Ord)

type Configuration l v = GConfiguration l (RedState l) v

instance {-# OVERLAPPABLE #-} (Show (s v)) => Show (GConfiguration l s v) where
  showsPrec d (Conf t s) = showString "(" . showsPrec d t . showString ", " . showsPrec d s . showString ")"

instance {-# OVERLAPPING #-} Show (GConfiguration l EmptyState v) where
  showsPrec d (Conf t _) = showsPrec d t


------------------------------------------------------------------------------------------------------------------

-- Match configuration
type MConf l = Configuration l Open

data StepTo l = StepTo (MConf l) (Rhs l)

-- TODO: Can't depend on state
type ExtFunc l r = ([MetaVar], [Term l Closed] -> r)

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
