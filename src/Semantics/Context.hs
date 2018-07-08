{-# LANGUAGE DataKinds, GADTs #-}

module Semantics.Context (
    PosFrame(..)
  , Frame(..)
  , Context(..)
  , splitRhs
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
  KStepTo      :: Configuration l v                 -> PosFrame l v

  -- Hackiness alert: Currently, computations output terms....but frames can only take confs as input
  -- So, we're doing this with pure hacks for now
  KComputation :: ExtComp l v       ->    Frame l v -> PosFrame l v

-- Rule for frames: all PosFrame's may have no free variables except those bound by a surrounding KInp

data Frame l v where
  KInp    :: MConf l -> PosFrame l Open -> Frame l Open

data Context l v where
  Halt :: Context l v
  Push :: Frame l v -> Context l v -> Context l v
  KVar :: MetaVar -> Context l Open


splitRhs :: (Lang l) => Rhs l -> (PosFrame l Open, Maybe (MConf l, Rhs l))
splitRhs (Build c) = (KBuild c, Nothing)
splitRhs (SideCondition cond rhs) = let (pf, rest) = splitRhs rhs in
                                    (KSideCond cond pf, rest)
splitRhs (LetStepTo out arg rhs') = (KStepTo arg, Just (out, rhs'))
splitRhs (LetComputation out comp rhs) = let (pf, rest) = splitRhs rhs in
                                         (KComputation comp (KInp (initConf (MetaVar out)) pf), rest)
