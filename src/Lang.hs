{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies #-}

module Lang (
    Lang(..)
  , Configuration(..)
  ) where

import Configuration
import Matching
import Semantics
import Term

class (Matchable (Configuration l)) => Lang l where
  signature :: Signature l
  rules :: IO (NamedRules l)