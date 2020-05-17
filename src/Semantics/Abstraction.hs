{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}

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
import Var

import Data.Interned.ByteString ( InternedByteString(..) )

type Abstraction t = t -> t

class AbstractCompFuncs t l where
  abstractCompFuncs :: Abstraction (CompFunc l) -> t -> t

--------------------------------------------------------------------------------------------------------

data IrrelevanceType = ValueIrr | SortIrr Sort | VarNotIrr !InternedByteString deriving (Show, Eq)

class Irrelevance t where
  irrelevance :: IrrelevanceType -> t -> t

instance Irrelevance () where
  irrelevance _ () = ()

instance (Irrelevance s, Lang l) => Irrelevance (GConfiguration s l) where
  irrelevance irr (Conf t s) = Conf (irrelevance irr t) (irrelevance irr s)


-- Ugly special case: Don't abstract boundvars. Not especially principled.
instance (Lang l) => Irrelevance (Term l) where
  irrelevance ValueIrr = mapTerm valToStar
    where
      valToStar x@(GMetaVar v _) | getVarType v == BoundVar = x
      valToStar (Val _ _) = ValStar
      valToStar t         = t

  irrelevance (VarNotIrr name) = mapTerm valToStar
    where
      valToStar x@(GMetaVar v _) | getVarType v == BoundVar = x
      valToStar t@(Val "true" _) = t
      valToStar t@(Val "false" _) = t
      valToStar t@(Val "EVal" [StrNode "StrConst" _]) = t -- another hack; no effect on graph shape,
                                                          -- but keeping this info is helpful
      valToStar (Val _ _) = ValStar
      valToStar t         = t

  irrelevance (SortIrr sort) = mapTerm (sortToStar . sortSubtermsToStar)
    where
      sortToStar x@(GMetaVar v _) | getVarType v == BoundVar = x
      sortToStar t  = case sortOfTerm signature t of
                        Just s@(Sort _) -> if s == sort then ValStar else t
                        _ -> t

      deferentialGetSort x st = maybe st id $ sortOfTerm @l signature x

      processNode (x@(GMetaVar v _), _) | getVarType v == BoundVar = x
      processNode (x, st) = if deferentialGetSort x st == sort then ValStar else x

      sortSubtermsToStar (Node s ts) = let (NodeSig _ stSorts _) = getSigNode (signature @l) s in
                                       Node s $ map processNode (zip ts stSorts)
      sortSubtermsToStar (Val  s ts) = let (ValSig  _ stSorts _) = getSigNode (signature @l) s in
                                       Val s  $ map processNode (zip ts stSorts)
      sortSubtermsToStar x           = x



instance Irrelevance EmptyState where
  irrelevance _ EmptyState = EmptyState

-- TODO: What should this do for keys?
instance (Lang l) => Irrelevance (SimpEnvMap (Term l) (Term l)) where
  irrelevance irr@(VarNotIrr name) (SimpEnvMap m) = SimpEnvMap
    (Map.mapWithKey (\key t -> case key of
                (StrNode _ n) -> if n == name then t else irrelevance ValueIrr t
                _ -> irrelevance irr t)
             m)
  irrelevance irr (SimpEnvMap m) = SimpEnvMap (Map.map (irrelevance irr) m)
  irrelevance irr (SimpEnvMap m) = SimpEnvMap m

instance (Lang l) => Irrelevance (SimpEnv (Term l) (Term l)) where
  irrelevance irr (JustSimpMap m)   = JustSimpMap (irrelevance irr m)
  irrelevance irr (SimpEnvRest v m) = SimpEnvRest v (irrelevance irr m)

instance (Irrelevance a, Irrelevance b) => Irrelevance (a, b) where
  irrelevance irr (a,b) = (irrelevance irr a, irrelevance irr b)

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