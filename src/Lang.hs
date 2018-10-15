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


-- |
class (LangBase l, Hashable (Configuration l), Matchable (Configuration l)) => Lang l where

  -- | A language's syntax definition
  signature :: Signature l

  -- | The structural operational semantics for this language
  rules :: IO (NamedRules l)

  -- | Gives the initial execution configuration for a term.
  -- E.g.: for a stateful language, initializes the execution to have an empty mutable store
  -- Can then begin execution on this configuration.
  initConf :: Term l -> Configuration l


evaluationSequenceL :: (Lang l) => Configuration l -> IO [Configuration l]
evaluationSequenceL conf = rules >>= \rs -> evaluationSequence rs conf

-- Checks whether a term in a language is syntactically valid according to
-- the syntax definition of that language
checkTermL :: (Lang l) => Term l -> ()
checkTermL = checkTerm signature