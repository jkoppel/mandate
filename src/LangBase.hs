{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module LangBase (
    LangBase(..)
  ) where

import Data.ByteString.Char8 ( ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

import MatchEffect
import Term

-- This file is how we break the circular depnedence between Lang and Semantics
-- Semantics are defined relative to a language, but

class (Typeable l, Eq (CompFunc l), Eq (RedState l)) => LangBase l where
  type RedState l :: *

  -- TODO: Funcs/sideconds need to be able to depend on state
  data CompFunc l
  compFuncName :: CompFunc l -> ByteString
  runCompFunc  :: CompFunc l -> [Term l] -> MatchEffect (Term l)

instance LangBase l => Show (CompFunc l) where
  show = BS.unpack . compFuncName