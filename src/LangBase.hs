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

import MatchEffect
import Term

-- These really should be in Configuration, but.....breaking circular dependencies

data GConfiguration l s where
  Conf :: Typeable s => Term l -> s -> GConfiguration l s

deriving instance (Eq (Term l), Eq s) => Eq (GConfiguration l s)
deriving instance (Ord (Term l), Ord s) => Ord (GConfiguration l s)

confTerm :: GConfiguration l s -> Term l
confTerm (Conf t _) = t

confState :: GConfiguration l s -> s
confState (Conf _ s) = s

type Configuration l = GConfiguration l (RedState l)

-- This file is how we break the circular depnedence between Lang and Semantics
-- Semantics are defined relative to a language, but

class (Typeable l, Typeable (RedState l), Eq (CompFunc l), Eq (RedState l)) => LangBase l where
  type RedState l :: *

  -- TODO: Funcs/sideconds need to be able to depend on state
  data CompFunc l
  compFuncName :: CompFunc l -> ByteString

  runCompFunc  :: CompFunc l -> [Term l] -> MatchEffect (Configuration l)

instance LangBase l => Show (CompFunc l) where
  show = BS.unpack . compFuncName