{-# LANGUAGE DataKinds, TypeFamilies #-}

module LangBase (
    LangBase(..)
  ) where

import Var


-- This file is how we break the circular depnedence between Lang and Semantics
-- Semantics are defined relative to a language, but

class LangBase l where
  type RedState l :: OpenClosed -> *