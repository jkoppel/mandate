{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, TypeFamilies #-}

module Configuration (
    RedState
  , GConfiguration(..)
  , Configuration

  , EmptyState(..)
  , SimpEnv(..)
  , SimpEnvMap(..)
  , getSimpEnvMap
  ) where

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Typeable ( Typeable )

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


-- Strictly speaking, a real ACI map should be designed in a way so that you could match Gamma1, Gamma2 against an env,
-- and it would try all possible decompositions. However, that seems slow, plus I haven't written it in a way
-- where you could do backtracking on matches (though the API supports it). Really, I don't think you want to be
-- running things where that's necessary
data SimpEnv a b v where
  SimpEnvRest :: (Ord a, Typeable a, Typeable b) => MetaVar -> SimpEnvMap a b Open -> SimpEnv a b Open -- Gamma, bindings
  JustSimpMap :: (Ord a, Typeable a, Typeable b) => SimpEnvMap a b v -> SimpEnv a b v

data SimpEnvMap a b (v :: OpenClosed) where
    SimpEnvMap :: (Ord a) => Map a (b v) -> SimpEnvMap a b v

getSimpEnvMap :: SimpEnvMap a b v-> Map a (b v)
getSimpEnvMap (SimpEnvMap m) = m

-- TODO: Intern


instance (Show a, Show (b v)) => Show (SimpEnvMap a b v) where
  showsPrec d (SimpEnvMap m) = foldr (\(k,v) s -> s . showsPrec (d+1) k . showString ": " . showsPrec (d+1) v) id (Map.toList m)

instance (Show a, Show (b v)) => Show (SimpEnv a b v) where
  showsPrec d (SimpEnvRest v m) = showsPrec d v . showString ", " . showsPrec d m
  showsPrec d (JustSimpMap m)   = showsPrec d m