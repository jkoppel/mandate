{-# LANGUAGE DataKinds, FlexibleInstances, KindSignatures, TypeFamilies #-}

module Configuration (
    RedState
  , GConfiguration(..)
  , Configuration

  , EmptyState(..)
  ) where

import Term
import Var

------------------------------------------------------------------------------------------------------------------

type family RedState l :: OpenClosed -> *

data GConfiguration l s v = Conf { confTerm :: Term l v, confState :: s v}
  deriving (Eq, Ord)

type Configuration l = GConfiguration l (RedState l)

data EmptyState (v :: OpenClosed) = EmptyState

instance {-# OVERLAPPABLE #-} (Show (s v)) => Show (GConfiguration l s v) where
  showsPrec d (Conf t s) = showString "(" . showsPrec d t . showString ", " . showsPrec d s . showString ")"

instance {-# OVERLAPPING #-} Show (GConfiguration l EmptyState v) where
  showsPrec d (Conf t _) = showsPrec d t



