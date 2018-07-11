{-# LANGUAGE FlexibleContexts #-}

module Lang (
    module LangBase

  , Lang(..)
  , Configuration

  , evaluationSequenceL
  , checkTermL
  ) where

import Data.Hashable ( Hashable )

import Configuration
import LangBase
import Matching
import Semantics.SOS
import Term

class (LangBase l, Hashable (Configuration l), Matchable (Configuration l)) => Lang l where
  signature :: Signature l
  rules :: IO (NamedRules l)

  initConf :: Term l -> Configuration l

evaluationSequenceL :: (Lang l) => Configuration l -> IO [Configuration l]
evaluationSequenceL conf = rules >>= \rs -> evaluationSequence rs conf

checkTermL :: (Lang l) => Term l -> ()
checkTermL = checkTerm signature