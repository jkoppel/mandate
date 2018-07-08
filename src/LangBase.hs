{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies #-}

module LangBase (
    LangBase(..)
  ) where

import Data.ByteString.Char8 ( ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

import MatchEffect
import Term
import Var


-- This file is how we break the circular depnedence between Lang and Semantics
-- Semantics are defined relative to a language, but

class (Typeable l) => LangBase l where
  type RedState l :: OpenClosed -> *

  -- TODO: Funcs/sideconds need to be able to depend on state
  data CompFunc l
  compFuncName :: CompFunc l -> ByteString
  runCompFunc  :: CompFunc l -> [Term l Closed] -> MatchEffect (Term l Closed)

  data SideCond l
  sideCondName :: SideCond l -> ByteString
  runSideCond  :: SideCond l -> [Term l Closed] -> MatchEffect Bool

instance LangBase l => Show (CompFunc l) where
  show = BS.unpack . compFuncName

instance LangBase l => Show (SideCond l) where
  show = BS.unpack . sideCondName