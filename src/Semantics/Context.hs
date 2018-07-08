{-# LANGUAGE DataKinds, GADTs #-}

module Semantics.Context (
    PosFrame(..)
  , Frame(..)
  , Context(..)
  , rhsToFrame
  ) where

import Configuration
import Lang
import Semantics.General
import Semantics.SOS
import Term
import Var

-- TODO: Intern

data PosFrame l v where
  KBuild       :: Configuration l v                 -> PosFrame l v
  KSideCond    :: ExtCond l v       -> PosFrame l v -> PosFrame l v
  KStepTo      :: Configuration l v ->    Frame l v -> PosFrame l v

  -- Hackiness alert: Currently, computations output terms....but frames can only take confs as input
  -- So, we're doing this with pure hacks for now
  KComputation :: ExtComp l v       ->    Frame l v -> PosFrame l v

-- Rule for frames: all PosFrame's may have no free variables except those bound by a surrounding KInp

data Frame l v where
  KInp    :: MConf l -> PosFrame l Open -> Frame l Open

data Context l v where
  KHalt :: Context l v
  KPush :: Frame l v -> Context l v -> Context l v
  KVar :: MetaVar -> Context l Open


rhsToFrame :: (Lang l) => Rhs l -> PosFrame l Open
rhsToFrame (Build c)                     = KBuild c
rhsToFrame (SideCondition cond rhs)      = KSideCond cond (rhsToFrame rhs)
rhsToFrame (LetStepTo out arg rhs')      = KStepTo arg (KInp out (rhsToFrame rhs'))
rhsToFrame (LetComputation out comp rhs) = KComputation comp (KInp (initConf (MetaVar out)) (rhsToFrame rhs))
