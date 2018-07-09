{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

module Semantics.Context (
    PosFrame(..)
  , Frame(..)
  , Context(..)
  , rhsToFrame
  ) where

import Control.Monad ( mzero )

import Configuration
import Lang
import Matching
import Semantics.General
import Semantics.SOS
import Term
import Var

-- TODO: Intern

data PosFrame l = KBuild  !(Configuration l)
                | KStepTo !(Configuration l) !(Frame l)

                -- Hackiness alert: Currently, computations output terms....but frames can only take confs as input
                -- So, we're doing this with pure hacks for now
                | KComputation !(ExtComp l) !(Frame l)

deriving instance (Eq   (Configuration l), LangBase l) => Eq   (PosFrame l)
deriving instance (Show (Configuration l), LangBase l) => Show (PosFrame l)

-- Rule for frames: all PosFrame's may have no free variables except those bound by a surrounding KInp
data Frame l = KInp !(Configuration l) !(PosFrame l)

deriving instance (Eq   (Configuration l), LangBase l) => Eq   (Frame l)
deriving instance (Show (Configuration l), LangBase l) => Show (Frame l)

data Context l = KHalt
               | KPush !(Frame l) !(Context l)
               | KVar !MetaVar

deriving instance (Eq   (Configuration l), LangBase l) => Eq   (Context l)
deriving instance (Show (Configuration l), LangBase l) => Show (Context l)


rhsToFrame :: (Lang l) => Rhs l -> PosFrame l
rhsToFrame (Build c)                     = KBuild c
rhsToFrame (LetStepTo out arg rhs')      = KStepTo arg (KInp out (rhsToFrame rhs'))
rhsToFrame (LetComputation out comp rhs) = KComputation comp (KInp (initConf (MetaVar out)) (rhsToFrame rhs))

-- TODO: The code in here reflects the confusion about "Closed" terms
instance (Lang l) => Matchable (PosFrame l) where
  match (Pattern (KBuild       c1   )) (Matchee (KBuild       c2   )) = match (Pattern c1) (Matchee c2)
  match (Pattern (KStepTo      c1 f1)) (Matchee (KStepTo      c2 f2)) = match (Pattern c1) (Matchee c2) >> match (Pattern f1) (Matchee f2)
  match (Pattern (KComputation c1 f1)) (Matchee (KComputation c2 f2)) =    matchExtComp (Pattern c1) (Matchee c2)
                                                                        >> match     (Pattern f1)    (Matchee f2)
  match _ _ = mzero

  refreshVars (KBuild c) = KBuild <$> refreshVars c
  refreshVars (KStepTo c f) = KStepTo <$> refreshVars c <*> refreshVars f
  refreshVars (KComputation (c, args) f) = mapM refreshVars args >>= \args' -> KComputation (c, args') <$> refreshVars f

  fillMatch (KBuild       c  ) = KBuild       <$> fillMatch c
  fillMatch (KStepTo      c f) = KStepTo      <$> fillMatch c <*> fillMatch f
  fillMatch (KComputation c f) = KComputation <$> fillMatchExtComp c <*> fillMatch f

instance (Lang l) => Matchable (Frame l) where
  match (Pattern (KInp c1 pf1)) (Matchee (KInp c2 pf2)) = match (Pattern c1) (Matchee c2) >> match (Pattern pf1) (Matchee pf2)
  refreshVars (KInp c pf) = KInp <$> refreshVars c <*> refreshVars pf
  fillMatch   (KInp c pf) = KInp <$> fillMatch   c <*> fillMatch   pf

instance (Lang l) => Matchable (Context l) where
  match (Pattern  KHalt)        (Matchee  KHalt)        = return ()
  match (Pattern (KPush f1 c1)) (Matchee (KPush f2 c2)) = match (Pattern f1) (Matchee f2) >> match (Pattern c1) (Matchee c2)
  match (Pattern (KVar v1))     (Matchee (KVar v2))     = putVar v1 v2
  match _ _ = mzero

  refreshVars KHalt       = return KHalt
  refreshVars (KPush f c) = KPush <$> refreshVars f <*> refreshVars c
  refreshVars (KVar v)    = KVar  <$> refreshVar v

  fillMatch KHalt       = return KHalt
  fillMatch (KPush f c) = KPush <$> fillMatch f <*> fillMatch c
  fillMatch (KVar v)    = getVarMaybe v (return . KVar) (return $ KVar v)
