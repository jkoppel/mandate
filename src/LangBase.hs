{-# LANGUAGE FlexibleContexts, GADTs, StandaloneDeriving, TypeFamilies #-}

module LangBase (
    GConfiguration(..)
  , confTerm
  , confState
  , Configuration

  , LangBase(..)
  ) where

import Data.ByteString.Char8 ( ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

import Data.Hashable ( Hashable(..) )

import MatchEffect
import Term

--------------------------------------------------------------------------------------------------------

-- | The 'LangBase' typeclass contains information associated with a language that do not involve configurations
-- or semantics. Most modules should rely on 'Lang' instead.

-- The following definitions really should be in Configuration, but.....breaking circular dependencies

data GConfiguration s l where
  Conf :: Typeable s => Term l -> s -> GConfiguration s l

deriving instance (Eq (Term l), Eq s) => Eq (GConfiguration s l)
deriving instance (Ord (Term l), Ord s) => Ord (GConfiguration s l)

instance (Hashable s) => Hashable (GConfiguration s l) where
  hashWithSalt s (Conf t st) = s `hashWithSalt` t `hashWithSalt` s

confTerm :: GConfiguration s l -> Term l
confTerm (Conf t _) = t

confState :: GConfiguration s l -> s
confState (Conf _ s) = s

-- | The configuration is the object of the transition system defined by
-- a language's semantics. It contains a term together with the extra information
-- (i.e.: environment) computed and transformed when running a program.
type Configuration l = GConfiguration (RedState l) l

-- This file is how we break the circular dependence between Lang and Semantics
-- Semantics are defined relative to a language, but

class (Typeable l, Typeable (RedState l), Eq (CompFunc l), Eq (RedState l), Show (RedState l), Hashable (CompFunc l)) => LangBase l where
  -- | The "reduction state" of all extra information that is maintained about a program when executing it.
  -- E.g.: the mutable store, installed exception handlers, etc
  type RedState l :: *


  -- | A defunctionalized representation of all semantic/"meta-level" functions associated with a language.
  --
  -- For instance, in a language with addition/multiplication, the syntactic "+" and "*" nodes will likely be implemented
  -- using Haskell's meta-level addition and multiplication operators. The language implementation may define CompFunc
  -- to have constructors "AddOp" and "MulOp", and define them separately by implementing `runCompFunc`.
  -- This way, SOS rules may be given entirely as first-order terms (no lambdas).
  --
  -- Note that all semantics functions must be well-behaved when given abstract terms

  data CompFunc l

  -- | Gives a human-readable name for the input meta-level operations. Used when displaying rules.
  compFuncName :: CompFunc l -> ByteString

  -- TODO: Funcs need to be able to depend on state
  -- |
  runCompFunc  :: CompFunc l -> [Term l] -> MatchEffect (Configuration l)

instance LangBase l => Show (CompFunc l) where
  show = BS.unpack . compFuncName