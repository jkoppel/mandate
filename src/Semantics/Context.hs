{-# LANGUAGE DataKinds, GADTs #-}

module Semantics.Context (
    PosFrame(..)
  , Frame(..)
  , Context(..)
  ) where

import Configuration
import Semantics.General
import Term
import Var

-- TODO: Intern

data PosFrame l v where
  KBuild       :: Configuration l v                 -> PosFrame l v
  KStepTo      :: Configuration l v                 -> PosFrame l v
  KSideCond    :: ExtCond l v       -> PosFrame l v -> PosFrame l v
  KComputation :: ExtComp l v       ->    Frame l v -> PosFrame l v

-- Rule for frames: all PosFrame's may have no free variables except those bound by a surrounding KInp

data Frame l v where
  KInp    :: MConf l -> PosFrame l Open -> Frame l Open

data Context l v where
  Halt :: Context l v
  Push :: Frame l v -> Context l v -> Context l v
  KVar :: MetaVar -> Context l Open