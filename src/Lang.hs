{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Lang (
    Lang(..)
  , Configuration

  , checkTermL
  ) where

import Data.ByteString.Char8 ( ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Hashable ( Hashable )
import Data.Typeable

import Configuration
import MatchEffect
import Matching
import Term
import Unification

-- | The configuration is the object of the transition system defined by
-- a language's semantics. It contains a term together with the extra information
-- (i.e.: environment) computed and transformed when running a program.
type Configuration l = GConfiguration (RedState l) l

class (Typeable l, Eq (CompFunc l), Hashable (CompFunc l), Unifiable (Configuration l)) => Lang l where

  -- | The "reduction state" of all extra information that is maintained about a program when executing it.
  -- E.g.: the mutable store, installed exception handlers, etc
  type RedState l :: *


  -- | A defunctionalized representation of all semantic/"meta-level" functions associated with a language.
  --
  -- For instance, in a language with addition/multiplication, the syntactic "+" and "*" nodes will likely be implemented
  -- using Haskell's meta-level addition and multiplication operators. The language implementation may define CompFunc
  -- to have constructors "AddOp" and "MulOp", and define them separately by implementing `runCompFunc`.
  -- This way, SOS rules may be given entirely as first-order terms (no lambdas).
  data CompFunc l

  -- | Gives a human-readable name for the input meta-level operations. Used when displaying rules.
  compFuncName :: CompFunc l -> ByteString

  runCompFunc  :: CompFunc l -> [Configuration l] -> MatchEffect (Configuration l)


  -- | A language's syntax definition
  signature :: Signature l

  -- | Gives the initial execution configuration for a term.
  -- E.g.: for a stateful language, initializes the execution to have an empty mutable store
  -- Can then begin execution on this configuration.
  initConf :: Term l -> Configuration l


instance Lang l => Show (CompFunc l) where
  show = BS.unpack . compFuncName

-- Checks whether a term in a language is syntactically valid according to
-- the syntax definition of that language
checkTermL :: (Lang l) => Term l -> ()
checkTermL = checkTerm signature