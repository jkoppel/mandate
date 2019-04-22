{-# LANGUAGE FlexibleContexts #-}

module Lang (
    module LangBase

  , Lang(..)
  , Configuration

  , checkTermL
  ) where

import Data.Hashable ( Hashable )

import Configuration
import LangBase
import Matching
import Term
import Unification

class (LangBase l, Unifiable (Configuration l)) => Lang l where

  -- | A language's syntax definition
  signature :: Signature l

  -- | Gives the initial execution configuration for a term.
  -- E.g.: for a stateful language, initializes the execution to have an empty mutable store
  -- Can then begin execution on this configuration.
  initConf :: Term l -> Configuration l

-- Checks whether a term in a language is syntactically valid according to
-- the syntax definition of that language
checkTermL :: (Lang l) => Term l -> ()
checkTermL = checkTerm signature