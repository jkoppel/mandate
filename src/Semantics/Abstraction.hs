{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}

module Semantics.Abstraction (
    Abstraction
  , AbstractCompFuncs(..)
  , Irrelevance(..)
  , IrrelevanceType(..)
  ) where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Data.Interned ( unintern )

import Configuration
import Lang
import Semantics.Context
import Semantics.General
import Term

type Abstraction t = t -> t

class AbstractCompFuncs t l where
  abstractCompFuncs :: Abstraction (CompFunc l) -> t -> t

--------------------------------------------------------------------------------------------------------

data IrrelevanceType = ValueIrr | SortIrr Sort deriving (Show, Eq)

class Irrelevance t where
  irrelevance :: IrrelevanceType -> t -> t

instance Irrelevance () where
  irrelevance _ () = ()

instance (Irrelevance s, Lang l) => Irrelevance (GConfiguration s l) where
  irrelevance irr (Conf t s) = Conf (irrelevance irr t) (irrelevance irr s)

instance (Lang l) => Irrelevance (Term l) where
  irrelevance ValueIrr = mapTerm valToStar
    where
      valToStar (Val _ _) = ValStar
      valToStar t         = t

  irrelevance (SortIrr sort) = mapTerm exprToStar
    where
      exprToStar t  = case sortOfTerm signature t of
                        Just s@(Sort _) -> if s == sort then ValStar else t
                        _ -> t


instance Irrelevance EmptyState where
  irrelevance _ EmptyState = EmptyState

-- TODO: What should this do for keys?
instance (Irrelevance b) => Irrelevance (SimpEnvMap a b) where
  irrelevance irr (SimpEnvMap m) = SimpEnvMap (Map.map (irrelevance irr) m)

instance (Irrelevance b) => Irrelevance (SimpEnv a b) where
  irrelevance irr (JustSimpMap m)   = JustSimpMap (irrelevance irr m)
  irrelevance irr (SimpEnvRest v m) = SimpEnvRest v (irrelevance irr m)

instance (Irrelevance (CompFunc l), Irrelevance (Configuration l)) => Irrelevance (ExtComp l) where
  irrelevance irr (ExtComp f args) = ExtComp (irrelevance irr f) (map (irrelevance irr) args)

instance (Irrelevance (ExtComp l), Irrelevance (Configuration l)) => Irrelevance (PosFrame l) where
  irrelevance irr (KBuild       c  ) = KBuild       (irrelevance irr c)
  irrelevance irr (KStepTo      c f) = KStepTo      (irrelevance irr c) (irrelevance irr f)
  irrelevance irr (KComputation c f) = KComputation (irrelevance irr c) (irrelevance irr f)

instance (Irrelevance (ExtComp l), Irrelevance (Configuration l)) => Irrelevance (Frame l) where
  irrelevance irr (KInp args pf) = KInp args (irrelevance irr pf) -- args are in negative position

instance (Irrelevance (ExtComp l), Irrelevance (Configuration l)) => Irrelevance (Context l) where
  irrelevance _  KHalt      = KHalt
  irrelevance irr (KPush f c) = KPush (irrelevance irr f) (irrelevance irr c)
  irrelevance _ (KVar v)    = KVar v