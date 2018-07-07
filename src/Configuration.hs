{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleInstances, GADTs, KindSignatures, PatternSynonyms, StandaloneDeriving, TypeFamilies, ViewPatterns #-}

module Configuration (
    RedState
  , GConfiguration(..)
  , Configuration

  , EmptyState(..)
  , SimpEnv(..)
  , SimpEnvMap(..)
  , getSimpEnvMap

  , pattern EmptySimpMap
  , pattern SingletonSimpMap
  , pattern EmptySimpEnv
  , pattern WholeSimpEnv
  , pattern AssocOneVal
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
  showsPrec d (Conf t s) = showString "(" . showsPrec d t . showString "; " . showsPrec d s . showString ")"

instance {-# OVERLAPPING #-} Show (GConfiguration l EmptyState v) where
  showsPrec d (Conf t _) = showsPrec d t

instance HasVars EmptyState where
  assumeClosed EmptyState = EmptyState

instance (HasVars s) => HasVars (GConfiguration l s) where
  assumeClosed (Conf t s) = Conf (assumeClosed t) (assumeClosed s)


-- Strictly speaking, a real ACI map should be designed in a way so that you could match Gamma1, Gamma2 against an env,
-- and it would try all possible decompositions. However, that seems slow, plus I haven't written it in a way
-- where you could do backtracking on matches (though the API supports it). Really, I don't think you want to be
-- running things where that's necessary
data SimpEnv a b v where
  SimpEnvRest :: (ForallOC Ord a, ForallOC Eq b, Typeable a, Typeable b) => MetaVar -> SimpEnvMap a b Open -> SimpEnv a b Open -- Gamma, bindings
  JustSimpMap :: (ForallOC Ord a, ForallOC Eq b, Typeable a, Typeable b) => SimpEnvMap a b v -> SimpEnv a b v

deriving instance (Eq (a v), Eq (b v)) => Eq (SimpEnv a b v)

data SimpEnvMap a b (v :: OpenClosed) where
  SimpEnvMap :: (ForallOC Ord a, ForallOC Eq b) => Map (a v) (b v) -> SimpEnvMap a b v

deriving instance (Eq (a v), Eq (b v)) => Eq (SimpEnvMap a b v)

getSimpEnvMap :: SimpEnvMap a b v -> Map (a v) (b v)
getSimpEnvMap (SimpEnvMap m) = m

pattern EmptySimpMap :: (ForallOC Ord a, ForallOC Eq b) => (ForallOC Ord a, ForallOC Eq b) => SimpEnvMap a b v
pattern EmptySimpMap <- SimpEnvMap (Map.null -> True) where
  EmptySimpMap = SimpEnvMap Map.empty

pattern SingletonSimpMap :: (ForallOC Ord a, ForallOC Eq b) => (ForallOC Ord a, ForallOC Eq b) => a v -> b v -> SimpEnvMap a b v
pattern SingletonSimpMap a b <- SimpEnvMap (Map.toList -> [(a,b)]) where
  SingletonSimpMap a b = SimpEnvMap (Map.singleton a b)

pattern EmptySimpEnv :: (ForallOC Ord a, ForallOC Eq b, Typeable a, Typeable b) => (ForallOC Ord a, ForallOC Eq b, Typeable a, Typeable b) => SimpEnv a b v
pattern EmptySimpEnv = JustSimpMap EmptySimpMap

pattern WholeSimpEnv :: (ForallOC Ord a, ForallOC Eq b, Typeable a, Typeable b) => (ForallOC Ord a, ForallOC Eq b, Typeable a, Typeable b) => MetaVar -> SimpEnv a b Open
pattern WholeSimpEnv v = SimpEnvRest v EmptySimpMap

pattern AssocOneVal :: (ForallOC Ord a, ForallOC Eq b, Typeable a, Typeable b) => (ForallOC Ord a, ForallOC Eq b, Typeable a, Typeable b) => MetaVar -> a Open -> b Open -> SimpEnv a b Open
pattern AssocOneVal v a b = SimpEnvRest v (SingletonSimpMap a b)

-- TODO: Intern


instance (Show (a v), Show (b v)) => Show (SimpEnvMap a b v) where
  showsPrec d (SimpEnvMap m) = foldr (\(k,v) s -> s . showsPrec (d+1) k . showString ": " . showsPrec (d+1) v) id (Map.toList m)

instance (Show (a v), Show (b v)) => Show (SimpEnv a b v) where
  showsPrec d (SimpEnvRest v m) = if Map.null (getSimpEnvMap m) then
                                    showsPrec d v
                                  else
                                    showsPrec d v . showString ", " . showsPrec d m
  showsPrec d (JustSimpMap m)   = showsPrec d m

instance (HasVars a, HasVars b) => HasVars (SimpEnvMap a b) where
  assumeClosed (SimpEnvMap m) = SimpEnvMap $ Map.mapKeys assumeClosed $ Map.map assumeClosed m

instance (HasVars a, HasVars b) => HasVars (SimpEnv a b) where
  assumeClosed (SimpEnvRest v _) = error ("Assuming SimpEnv is closed, but has rest variable " ++ show v)
  assumeClosed (JustSimpMap m)   = JustSimpMap $ assumeClosed m