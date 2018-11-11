{-# LANGUAGE FlexibleContexts, GADTs, TypeFamilies #-}

module LangBase (
    LangBase(..)
   , Configuration
  ) where

import Data.ByteString.Char8 ( ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

import Data.Hashable ( Hashable(..) )

import Matching
import MatchEffect
import Term
import GConfiguration

--------------------------------------------------------------------------------------------------------

-- | The 'LangBase' typeclass contains information associated with a language that do not involve configurations
-- or semantics. Most modules should rely on 'Lang' instead.


-- This definition really should be in Configuration, but.....breaking circular dependencies

-- | The configuration is the object of the transition system defined by
-- a language's semantics. It contains a term together with the extra information
-- (i.e.: environment) computed and transformed when running a program.
type Configuration l = GConfiguration (RedState l) l

-- This file is how we break the circular dependence between Lang and Semantics
-- Semantics are defined relative to a language, but

class (Typeable l, Typeable (RedState l), Eq (CompFunc l), Eq (StatefulFunc l), Eq (RedState l), Show (RedState l), Hashable (CompFunc l), Hashable (StatefulFunc l)) => LangBase l where
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
  data StatefulFunc l

  -- | Gives a human-readable name for the input meta-level operations. Used when displaying rules.
  compFuncName :: CompFunc l -> ByteString
  statefulFuncName :: StatefulFunc l -> ByteString

  runCompFunc  :: CompFunc l -> [Term l] -> MatchEffect (Configuration l)
  runStatefulFunc  :: StatefulFunc l -> [Configuration l] -> MatchEffect (Configuration l)

instance LangBase l => Show (CompFunc l) where
  show = BS.unpack . compFuncName

instance LangBase l => Show (StatefulFunc l) where
  show = BS.unpack . statefulFuncName