{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Semantics.Abstraction (
    Abstraction
  , ValueIrrelevance(..)
  ) where

import qualified Data.Map as Map

import Configuration
import Semantics.Context
import Semantics.General
import Term

type Abstraction t = t -> t

class ValueIrrelevance t where
  valueIrrelevance :: t -> t

instance (ValueIrrelevance s) => ValueIrrelevance (GConfiguration s l) where
  valueIrrelevance (Conf t s) = Conf (valueIrrelevance t) (valueIrrelevance s)

instance ValueIrrelevance (Term l) where
  valueIrrelevance = mapTerm valToStar
    where
      valToStar (Val _ _) = ValStar
      valToStar t         = t

instance ValueIrrelevance EmptyState where
  valueIrrelevance EmptyState = EmptyState


-- TODO: What should this do for keys?
instance (ValueIrrelevance b) => ValueIrrelevance (SimpEnvMap a b) where
  valueIrrelevance (SimpEnvMap m) = SimpEnvMap (Map.map valueIrrelevance m)

instance (ValueIrrelevance b) => ValueIrrelevance (SimpEnv a b) where
  valueIrrelevance (JustSimpMap m)   = JustSimpMap (valueIrrelevance m)
  valueIrrelevance (SimpEnvRest v m) = SimpEnvRest v (valueIrrelevance m)

instance (ValueIrrelevance (Configuration l)) => ValueIrrelevance (ExtComp l) where
  valueIrrelevance (ExtComp f args) = ExtComp f (map valueIrrelevance args)

instance (ValueIrrelevance (Configuration l)) => ValueIrrelevance (PosFrame l) where
  valueIrrelevance (KBuild       c  ) = KBuild       (valueIrrelevance c)
  valueIrrelevance (KStepTo      c f) = KStepTo      (valueIrrelevance c) (valueIrrelevance f)
  valueIrrelevance (KComputation c f) = KComputation (valueIrrelevance c) (valueIrrelevance f)

instance (ValueIrrelevance (Configuration l)) => ValueIrrelevance (Frame l) where
  valueIrrelevance (KInp args pf) = KInp args (valueIrrelevance pf) -- args are in negative position

instance (ValueIrrelevance (Configuration l)) => ValueIrrelevance (Context l) where
  valueIrrelevance  KHalt      = KHalt
  valueIrrelevance (KPush f c) = KPush (valueIrrelevance f) (valueIrrelevance c)
  valueIrrelevance (KVar v)    = KVar v