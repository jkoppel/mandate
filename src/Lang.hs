{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies #-}

module Lang (
    module LangBase

  , Lang(..)
  , Configuration(..)
  ) where

import Configuration
import LangBase
import Matching
import Semantics
import Term

class (LangBase l, Matchable (Configuration l)) => Lang l where
  signature :: Signature l
  rules :: IO (NamedRules l)