{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, GADTs, PatternSynonyms, ScopedTypeVariables,
             StandaloneDeriving, TupleSections, TypeApplications, TypeFamilies, UndecidableInstances, ViewPatterns #-}

module Configuration (
    GConfiguration(..)
  , confTerm
  , confState

  , mapKeysM

  , EmptyState(..)
  , SimpEnv(..)
  , SimpEnvMap(..)
  , getSimpEnvMap

  , normalizeEnvMap

  , pattern EmptySimpMap
  , pattern SingletonSimpMap
  , pattern EmptySimpEnv
  , pattern WholeSimpEnv
  , pattern AssocOneVal
  , Configuration.lookup, Configuration.size
  ) where

import qualified Data.Map as Map
import Data.Map ( lookup, size, Map )
import Data.Typeable ( Typeable, eqT )

import GHC.Generics ( Generic )

import Data.Hashable ( Hashable(..) )

import Lattice
import Matching.Class
import Term
import Var


-- | The configuration is the object of the transition system defined by
-- a language's semantics. It contains a term together with the extra information
-- (i.e.: environment) computed and transformed when running a program.
data GConfiguration s l where
  Conf :: (Typeable l, Unifiable (Term l), Hashable s, Unifiable s) => Term l -> s -> GConfiguration s l

deriving instance Typeable (GConfiguration s l)
deriving instance Eq (GConfiguration s l)
deriving instance (Ord (Term l), Ord s) => Ord (GConfiguration s l)

instance Hashable (GConfiguration s l) where
  hashWithSalt s (Conf t st) = s `hashWithSalt` t `hashWithSalt` s

confTerm :: GConfiguration s l -> Term l
confTerm (Conf t _) = t

confState :: GConfiguration s l -> s
confState (Conf _ s) = s


instance (Meetable s, Eq (GConfiguration s l)) => Meetable (GConfiguration s l) where
  meet (Conf a1 b1) (Conf a2 b2) = Conf <$> meet a1 a2 <*> meet b1 b2


-- | Combining terms with the auxiliary state for the language
--
-- The core datatypes of this file are GConfiguration and Configuration. However, for reasons of avoiding
-- circular dependencies, these are actually defined in LangBase.hs / GConfiguration.hs

------------------------------------------ Pretty-printing ---------------------------------------------------------


data EmptyState = EmptyState
  deriving ( Eq, Ord, Show, Generic )

instance Hashable EmptyState

instance Meetable EmptyState where
  meet EmptyState EmptyState = Just EmptyState

-- For languages with no additional state in their configuration, we want to display
-- the configuration as foo(a,b), not as (foo(a,b) ; empty state).
--
-- Look ma! No overlapping instances.
instance Show (GConfiguration s l) where
  showsPrec d (Conf t s) = case eqT @s @EmptyState of
                             Just _  -> showsPrec d t
                             Nothing -> showString "(" . showsPrec d t . showString "; " . showsPrec d s . showString ")"


------------------------------------------ Map utils -------------------------------------------------------

-- FIXME: I think I can write this better using traverseWithKey
-- Curses for this not existing; will give a not-so-efficient implementation
mapKeysM :: (Ord k2, Applicative m) => (k1 -> m k2) -> Map k1 a -> m (Map k2 a)
mapKeysM f mp = Map.fromList <$> traverse (\(k,v) -> (,v) <$> f k) (Map.toList mp)

------------------------------------------ SimpEnv ---------------------------------------------------------


-- | A simple representation of environments (e.g.: for local variables) in semantics.
-- A closed `SimpEnv` is a map of variable names to values. An open one may have
-- meta-variables in value position, but not in key position. Most importantly, in open `SimpEnv`'s, there
-- may be at most one ACI (associative/commutative/idempotent) metavariable.
-- So, an open `SimpEnv` can represent terms like  "Gamma, x : a" often found in blackboard descriptions of semantics.
--
-- Note that "more open" `SimpEnv`'s with meta-variables in key position may appear, but, if matched,
-- will be treated as fixed terms.
--
-- Strictly speaking, a real ACI map should be designed in a way so that you could match Gamma1, Gamma2 against an env,
-- and it would try all possible decompositions. However, that seems slow, plus I haven't written it in a way
-- where you could do backtracking on matches (though the API supports it). Really, I don't think you want to be
-- running things where that's necessary
data SimpEnv a b where
  SimpEnvRest :: (Ord a, Eq b, Typeable a, Typeable b) => MetaVar -> SimpEnvMap a b -> SimpEnv a b -- Gamma, bindings
  JustSimpMap :: (Ord a, Eq b, Typeable a, Typeable b) => SimpEnvMap a b -> SimpEnv a b

deriving instance (Eq a, Eq b) => Eq (SimpEnv a b)

-- | The portion of a `SimpEnv` which does not contain any ACI variables
data SimpEnvMap a b where
  SimpEnvMap :: (Ord a, Eq b) => Map a b -> SimpEnvMap a b

deriving instance (Eq a, Eq b) => Eq (SimpEnvMap a b)

getSimpEnvMap :: SimpEnvMap a b -> Map a b
getSimpEnvMap (SimpEnvMap m) = m


-- TODO: Implement real meet
instance (Meetable a, Meetable b) => Meetable (SimpEnvMap a b) where
  meet = meetDefault

-- TODO: Implement real one
instance (Meetable (SimpEnvMap a b), Eq (SimpEnv a b)) => Meetable (SimpEnv a b) where
  meet = meetDefault

normalizeEnvMap :: (Ord a, Meetable a, Meetable b) => SimpEnvMap a b -> SimpEnvMap a b
normalizeEnvMap (SimpEnvMap mp) = SimpEnvMap $ Map.fromList $ maximalElts $ Map.toList mp
  where
    mpList = Map.toList mp

--------------------- SimpEnv: Smart constructors ----------------------


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

--------------------- SimpEnv: Pretty printing / hashing ----------------------


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


instance (Hashable a, Hashable b) => Hashable (SimpEnvMap a b) where
  hashWithSalt s (SimpEnvMap m) = s `hashWithSalt` (Map.toList m)

instance (Hashable a, Hashable b) => Hashable (SimpEnv a b) where
  hashWithSalt s (SimpEnvRest v m) = s `hashWithSalt` v `hashWithSalt` m
  hashWithSalt s (JustSimpMap m)   = s `hashWithSalt` m

-- TODO: Hash-cons SimpEnv

lookup :: Ord k => k -> SimpEnv k a -> Maybe a
lookup k (SimpEnvRest _ _) = Nothing
lookup k (JustSimpMap (SimpEnvMap m)) = Map.lookup k m

size :: SimpEnv k a -> Integer
size (SimpEnvRest _ _) = 0
size (JustSimpMap (SimpEnvMap m)) = toInteger $ Map.size m
