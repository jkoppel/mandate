{-# LANGUAGE DeriveGeneric, FlexibleContexts, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}

module Semantics.Context (
    PosFrame(..)
  , Frame(..)
  , Context(..)
  , rhsToFrame

  , NormalizeBoundVars(..)
  , normalizeBoundVars
  , FreeTermVars(..)
  , alphaEq
  ) where

import Control.Monad ( mzero, forM_ )
import Data.Set ( Set )
import qualified Data.Set as Set

import GHC.Generics ( Generic )

import Data.Hashable ( Hashable )

import Control.Monad.HT ( liftJoin2 )

import Configuration
import Debug
import Lattice
import Lang
import Matching
import Semantics.General
import Semantics.SOS
import Unification
import Var


-- | Contexts / continuations. These correspond to the "K" in a CEK machine.
-- They are constructed from the suffix of an SOS rule.
--
-- A context is composed of a sequence of frames. Each frame corresponds to a step of computation,
-- whose result will be passed to the next frame.


---------------------------------------


-- | The positive part of a frame, i.e.: no binders on the outside
data PosFrame l = KBuild  !(Configuration l)
                | KStepTo !(Configuration l) !(Frame l)
                | KComputation !(ExtComp l) !(Frame l)
  deriving ( Eq, Generic )

-- | A frame awaits the result of some computation as input, binds it to a variable, and
-- then performs another computation.
--
-- `KInp c p` is displayed as `[\c -> p]`
--
--
-- Rule for frames: all PosFrame's may have no free variables except those bound by a surrounding KInp
data Frame l = KInp !(Configuration l) -- this is a binder
                    !(PosFrame l)
  deriving ( Eq, Generic )


data Context l = KHalt
               | KPush !(Frame l) !(Context l)
               | KVar !MetaVar                  -- E.g.: the "K" in the pattern `[\x -> x +5 ] . K`
  deriving ( Eq, Generic )

--------------------------------------- Equality, printing, hashing ------------------------------------------


instance (Hashable (Configuration l), Lang l) => Hashable (PosFrame l)
instance (Hashable (Configuration l), Lang l) => Hashable (Frame l)
instance (Hashable (Configuration l), Lang l) => Hashable (Context l)

-- TODO: Intern

instance (Show (Configuration l), Lang l) => Show (PosFrame l) where
  showsPrec d (KBuild t) = showsPrec (d+1) t
  showsPrec d (KStepTo c f) = showString "step(" . showsPrec (d+1) c .
                                showString ") => " .
                                showsPrec d f
  showsPrec d (KComputation c r) = showsPrec d c . showString " => " . showsPrec d r


instance (Show (Configuration l), Lang l) => Show (Frame l) where
  showsPrec d (KInp c pf) = showString "[\\" . showsPrec d c . showString " -> " . showsPrec d pf . showString "]"

instance (Show (Configuration l), Lang l) => Show (Context l) where
  showsPrec d KHalt = showString "[]"
  showsPrec d (KPush f c) = showsPrec d f . showString "." . showsPrec d c
  showsPrec d (KVar v) = showsPrec d v

----------------------------------- Constructing contexts -------------------------------

boundifyingVars :: (Matchable f, MonadMatchable m) => m f -> m f
boundifyingVars = withVarAllocator mkBoundVarAllocator

-- | Performs a CPS conversion of on SOS RHS into a context.
-- Since our SOS rules are already essentially in A-normal form, this is easy.
rhsToFrame :: (Lang l, MonadMatchable m) => Rhs l -> m (PosFrame l)
rhsToFrame (Build c)                      = return $ KBuild c
rhsToFrame (LetStepTo out arg rhs')       = boundifyingVars $ KStepTo      arg  <$> (KInp <$> refreshVars out <*> (fillVars =<< rhsToFrame rhs'))
rhsToFrame (LetComputation out comp rhs') = boundifyingVars $ KComputation comp <$> (KInp <$> refreshVars out <*> (fillVars =<< rhsToFrame rhs'))


--------------------------------- Machinery for higher-order terms ----------------------

class NormalizeBoundVars f where
  normalizeBoundVars' :: (MonadMatchable m) => f -> m f

normalizeBoundVars :: (MonadMatchable m, NormalizeBoundVars f) => f -> m f
normalizeBoundVars f = withFreshCtx $ withVarAllocator mkBoundVarAllocator $ normalizeBoundVars' f

instance (Matchable (Configuration l), Matchable (ExtComp l)) => NormalizeBoundVars (PosFrame l) where
  normalizeBoundVars' (KBuild c) = KBuild <$> fillMatch c
  normalizeBoundVars' (KStepTo c f) = KStepTo <$> fillMatch c <*> normalizeBoundVars' f
  normalizeBoundVars' (KComputation c f) = KComputation <$> fillMatch c <*> normalizeBoundVars' f

instance (Matchable (Configuration l), NormalizeBoundVars (PosFrame l)) => NormalizeBoundVars (Frame l) where
  normalizeBoundVars' (KInp arg pf) = withSubCtx $ do
      refreshVars arg
      KInp <$> fillMatch arg <*> normalizeBoundVars' pf

instance (Matchable (Configuration l), Matchable (Context l), NormalizeBoundVars (Frame l)) => NormalizeBoundVars (Context l) where
  normalizeBoundVars' KHalt = return KHalt
  normalizeBoundVars' (KPush f c) = KPush <$> normalizeBoundVars' f <*> fillMatch c
  normalizeBoundVars' k@(KVar _) = fillMatch k


alphaEq :: (MonadMatchable m, NormalizeBoundVars f, Eq f, Show f) => f -> f -> m Bool
alphaEq a b = (==) <$> (normalizeBoundVars a) <*> (normalizeBoundVars b)


-- Free variables for a normalized (CPS-converted) context
class FreeTermVars f where
  freeTermVars :: (Lang l) => f l -> Set MetaVar

instance FreeTermVars Context where
  freeTermVars KHalt       = Set.empty
  freeTermVars (KVar _)    = Set.empty
  freeTermVars (KPush f c) = freeTermVars f `Set.union` freeTermVars c

instance FreeTermVars Frame where
  freeTermVars (KInp c pf) = freeTermVars pf `Set.difference` getVars c

instance FreeTermVars PosFrame where
  freeTermVars (KBuild c) = getVars c
  freeTermVars (KStepTo c f) = error "freeTermVars requires a CPS'd context; got a KStepTo"
  freeTermVars (KComputation comp f) = error "freeTermVars requires a CPS'd context; got a KComputation"


-------------------------------------- Matching ------------------------------------------

instance (Eq (PosFrame l)) => Meetable (PosFrame l) where
  meet      = error "Nonlinear pattern match done with context variable; meet not implemented"
  isMinimal = error "Storing PosFrame as key in env"

instance (Eq (Frame l)) => Meetable (Frame l) where
  meet      = error "Nonlinear pattern match done with context variable; meet not implemented"
  isMinimal = error "Storing Frame as key in env"

instance (Eq (Context l)) => Meetable (Context l) where
  meet      = error "Nonlinear pattern match done with context variable; meet not implemented"
  isMinimal = error "Storing Context as key in env"

instance (Lang l, Matchable (Configuration l)) => Matchable (PosFrame l) where
  getVars (KBuild       c  ) = getVars c
  getVars (KStepTo      c f) = getVars c `Set.union` getVars f
  getVars (KComputation c f) = getVars c `Set.union` getVars f

  match (Pattern (KBuild       c1   )) (Matchee (KBuild       c2   )) = match (Pattern c1) (Matchee c2)
  match (Pattern (KStepTo      c1 f1)) (Matchee (KStepTo      c2 f2)) = match (Pattern c1) (Matchee c2) >> match (Pattern f1) (Matchee f2)
  match (Pattern (KComputation c1 f1)) (Matchee (KComputation c2 f2)) = match (Pattern c1) (Matchee c2) >> match (Pattern f1) (Matchee f2)
  match _ _ = mzero

  mapVarsM f (KBuild       c   ) = KBuild       <$> mapVarsM f c
  mapVarsM f (KStepTo      c fr) = KStepTo      <$> mapVarsM f c <*> mapVarsM f fr
  mapVarsM f (KComputation c fr) = KComputation <$> mapVarsM f c <*> mapVarsM f fr

  fillMatch (KBuild       c  ) = KBuild       <$> fillMatch c
  fillMatch (KStepTo      c f) = KStepTo      <$> fillMatch c <*> fillMatch f
  fillMatch (KComputation c f) = KComputation <$> fillMatch c <*> fillMatch f

instance (Lang l, Matchable (Configuration l)) => Matchable (Frame l) where
  getVars (KInp c pf) = getVars c `Set.union` getVars pf

  -- The vars in the input are bound. Note that we clear them!
  match (Pattern (KInp c1 pf1)) (Matchee (KInp c2 pf2)) = do
    match (Pattern c1) (Matchee c2)
    match (Pattern pf1) (Matchee pf2)
    forM_ (Set.toList $ getVars c1) $ \v -> clearVar v

  mapVarsM f (KInp c pf) = KInp <$> mapVarsM f c <*> mapVarsM f pf

  fillMatch   (KInp c pf) = withSubCtx $ do
    forM_ (Set.toList $ getVars c) $ \v -> clearVar v
    KInp c <$> fillMatch pf

instance (Lang l, Matchable (Configuration l)) => Matchable (Context l) where
  getVars KHalt       = Set.empty
  getVars (KPush f c) = getVars f `Set.union` getVars c
  getVars (KVar v)    = Set.singleton v

  match (Pattern  KHalt)        (Matchee  KHalt)        = return ()
  match (Pattern (KPush f1 c1)) (Matchee (KPush f2 c2)) = match (Pattern f1) (Matchee f2) >> match (Pattern c1) (Matchee c2)
  match (Pattern (KVar v))      (Matchee x)             = putVar v x
  match _ _ = mzero

  mapVarsM f KHalt        = return KHalt
  mapVarsM f (KPush fr c) = KPush <$> mapVarsM f fr <*> mapVarsM f c
  mapVarsM f (KVar v)     = KVar  <$> f v

  fillMatch KHalt       = return KHalt
  fillMatch (KPush f c) = KPush <$> fillMatch f <*> fillMatch c
  fillMatch (KVar v)    = getVarMaybe v return (return $ KVar v)

-------------------------------------- Unification ----------------------------------------

instance (Lang l, Unifiable (Configuration l)) => Unifiable (PosFrame l) where
  unify (KBuild c1)        (KBuild c2)        = unify c1 c2
  unify (KStepTo c1 f1)    (KStepTo c2 f2)    = unify c1 c2 >> unify f1 f2
  unify (KComputation _ _) (KComputation _ _) = error "Not implemented: Unifying two KLetComputation's"
  unify _                  _                  = mzero

instance (Lang l, Unifiable (Configuration l)) => Unifiable (Frame l) where
  unify (KInp c1 pf1) (KInp c2 pf2) = do
    debugM "Unifying frame arg"
    unify c1 c2
    debugM "Unifying posframe"
    liftJoin2 unify (fillMatch pf1) (fillMatch pf2)
    debugM "Unified posframe"
    forM_ (Set.toList $ getVars c1) $ \v -> clearVar v

instance (Lang l, Unifiable (Configuration l)) => Unifiable (Context l) where
  unify KHalt         KHalt         = return ()
  unify (KVar v)      x             = elimVar v x
  unify x             (KVar v)      = elimVar v x
  unify (KPush f1 c1) (KPush f2 c2) = unify f1 f2 >> liftJoin2 unify (fillMatch c1) (fillMatch c2)
  unify _             _             = mzero