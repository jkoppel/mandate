{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies #-}

module Lang (
    Lang(..)
  , Configuration(..)
  ) where

import Configuration
import Matching
import Semantics
import Term

class (Matchable (RedState l)) => Lang l where
  signature :: Signature l
  rules :: MRules l