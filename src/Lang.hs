{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies #-}

module Lang (
    module LangBase

  , Lang(..)
  , Configuration(..)

  , evaluationSequenceL
  , checkTermL
  ) where

import Configuration
import LangBase
import Matching
import Semantics
import Term
import Var

class (LangBase l, Matchable (Configuration l)) => Lang l where
  signature :: Signature l
  rules :: IO (NamedRules l)

evaluationSequenceL :: (Lang l) => Configuration l Closed -> IO [Configuration l Closed]
evaluationSequenceL conf = rules >>= \rs -> evaluationSequence rs conf

checkTermL :: (Lang l) => Term l v -> ()
checkTermL = checkTerm signature