{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies #-}

module Lang (
    Lang(..)
  , Configuration(..)
  ) where

import Matching
import Semantics
import Term

class (Matchable (RedState l Closed)) => Lang l where
  signature :: Signature l
  rules :: MRules l