{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, PatternSynonyms, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, ViewPatterns #-}

module Configuration (
  -- These should be here, but are defined in LangBase. Re-exporting
    GConfiguration(..)
  , confTerm
  , confState
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

import Data.Typeable ( Typeable, eqT )

import LangBase
import Term
import Var

------------------------------------------------------------------------------------------------------------------

data EmptyState = EmptyState
  deriving ( Eq, Ord, Show )


-- Look ma! No overlapping instances.
-- Unfortunately, this is not quite enough to avoid the need to use StandaloneDeriving all over.
-- Need more dedicated OO techniques for that.
instance (Show s) => Show (GConfiguration l s) where
  showsPrec d (Conf t s) = case eqT @s @EmptyState of
                             Just _  -> showsPrec d t
                             Nothing -> showString "(" . showsPrec d t . showString "; " . showsPrec d s . showString ")"


-- Strictly speaking, a real ACI map should be designed in a way so that you could match Gamma1, Gamma2 against an env,
-- and it would try all possible decompositions. However, that seems slow, plus I haven't written it in a way
-- where you could do backtracking on matches (though the API supports it). Really, I don't think you want to be
-- running things where that's necessary
data SimpEnv a b where
  SimpEnvRest :: (Ord a, Eq b, Typeable a, Typeable b) => MetaVar -> SimpEnvMap a b -> SimpEnv a b -- Gamma, bindings
  JustSimpMap :: (Ord a, Eq b, Typeable a, Typeable b) => SimpEnvMap a b -> SimpEnv a b

deriving instance (Eq a, Eq b) => Eq (SimpEnv a b)

data SimpEnvMap a b where
  SimpEnvMap :: (Ord a, Eq b) => Map a b -> SimpEnvMap a b

deriving instance (Eq a, Eq b) => Eq (SimpEnvMap a b)

getSimpEnvMap :: SimpEnvMap a b -> Map a b
getSimpEnvMap (SimpEnvMap m) = m

pattern EmptySimpMap :: () => (Ord a, Eq b) => SimpEnvMap a b
pattern EmptySimpMap <- SimpEnvMap (Map.null -> True) where
  EmptySimpMap = SimpEnvMap Map.empty

pattern SingletonSimpMap :: () => (Ord a, Eq b) => a -> b -> SimpEnvMap a b
pattern SingletonSimpMap a b <- SimpEnvMap (Map.toList -> [(a,b)]) where
  SingletonSimpMap a b = SimpEnvMap (Map.singleton a b)

pattern EmptySimpEnv :: () => (Ord a, Eq b, Typeable a, Typeable b) => SimpEnv a b
pattern EmptySimpEnv = JustSimpMap EmptySimpMap

pattern WholeSimpEnv :: () => (Ord a, Eq b, Typeable a, Typeable b) => MetaVar -> SimpEnv a b
pattern WholeSimpEnv v = SimpEnvRest v EmptySimpMap

pattern AssocOneVal :: () => (Ord a, Eq b, Typeable a, Typeable b) => MetaVar -> a -> b -> SimpEnv a b
pattern AssocOneVal v a b = SimpEnvRest v (SingletonSimpMap a b)

-- TODO: Intern


instance (Show a, Show b) => Show (SimpEnvMap a b) where
  showsPrec d EmptySimpMap   = id
  showsPrec d (SimpEnvMap m) = let (first:rest) = Map.toList m in
                               foldr (\kv s -> s . showString ", " . showAssoc kv) (showAssoc first) rest
    where
      showAssoc (k, v) = showsPrec (d+1) k . showString ": " . showsPrec (d+1) v

instance (Show a, Show b) => Show (SimpEnv a b) where
  showsPrec d (SimpEnvRest v m) = if Map.null (getSimpEnvMap m) then
                                    showsPrec d v
                                  else
                                    showsPrec d v . showString ", " . showsPrec d m
  showsPrec d (JustSimpMap m)   = showsPrec d m