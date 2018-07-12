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

-- These really should be in Configuration, but.....breaking circular dependencies

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

type Configuration l = GConfiguration (RedState l) l

-- This file is how we break the circular depnedence between Lang and Semantics
-- Semantics are defined relative to a language, but

class (Typeable l, Typeable (RedState l), Eq (CompFunc l), Eq (RedState l), Show (RedState l), Hashable (CompFunc l)) => LangBase l where
  type RedState l :: *

  -- TODO: Funcs/sideconds need to be able to depend on state
  data CompFunc l
  compFuncName :: CompFunc l -> ByteString

  runCompFunc  :: CompFunc l -> [Term l] -> MatchEffect (Configuration l)

instance LangBase l => Show (CompFunc l) where
  show = BS.unpack . compFuncName