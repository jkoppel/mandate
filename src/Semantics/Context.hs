{-# LANGUAGE DeriveGeneric, FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}

module Semantics.Context (
    PosFrame(..)
  , Frame(..)
  , Context(..)
  , rhsToFrame

  , NormalizeBoundVars(..)
  , normalizeBoundVars
  , alphaEq
  ) where

import Control.Monad ( mzero, forM_ )
import Data.Set ( Set )
import qualified Data.Set as Set

import GHC.Generics ( Generic )

import Data.Hashable ( Hashable )

import Configuration
import LangBase
import Matching
import Semantics.General
import Semantics.SOS
import Term
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
  deriving ( Generic )

deriving instance (Eq (Configuration l), LangBase l) => Eq (PosFrame l)

-- | A frame awaits the result of some computation as input, binds it to a variable, and
-- then performs another computation.
--
-- `KInp c p` is displayed as `[\c -> p]`
--
--
-- Rule for frames: all PosFrame's may have no free variables except those bound by a surrounding KInp
data Frame l = KInp !(Configuration l) -- this is a binder
                    !(PosFrame l)
  deriving ( Generic )

deriving instance (Eq (Configuration l), LangBase l) => Eq (Frame l)


data Context l = KHalt
               | KPush !(Frame l) !(Context l)
               | KVar !MetaVar                  -- E.g.: the "K" in the pattern `[\x -> x +5 ] . K`
  deriving ( Generic )

deriving instance (Eq (Frame l)) => Eq (Context l)

--------------------------------------- Equality, printing, hashing ------------------------------------------


instance (Hashable (Configuration l), LangBase l) => Hashable (PosFrame l)
instance (Hashable (Configuration l), LangBase l) => Hashable (Frame l)
instance (Hashable (Configuration l), LangBase l) => Hashable (Context l)

-- TODO: Intern

instance (Show (Configuration l), LangBase l) => Show (PosFrame l) where
  showsPrec d (KBuild t) = showsPrec (d+1) t
  showsPrec d (KStepTo c f) = showString "step(" . showsPrec (d+1) c .
                                showString ") => " .
                                showsPrec d f
  showsPrec d (KComputation c r) = showsPrec d c . showString " => " . showsPrec d r


instance (Show (Configuration l), LangBase l) => Show (Frame l) where
  showsPrec d (KInp c pf) = showString "[\\" . showsPrec d c . showString " -> " . showsPrec d pf . showString "]"

instance (Show (Configuration l), LangBase l) => Show (Context l) where
  showsPrec d KHalt = showString "[]"
  showsPrec d (KPush f c) = showsPrec d f . showString "." . showsPrec d c
  showsPrec d (KVar v) = showsPrec d v

----------------------------------- Constructing contexts -------------------------------

-- | Performs a CPS conversion of on SOS RHS into a context.
-- Since our SOS rules are already essentially in A-normal form, this is easy.
rhsToFrame :: (LangBase l) => Rhs l -> PosFrame l
rhsToFrame (Build c)                      = KBuild c
rhsToFrame (LetStepTo out arg rhs')       = KStepTo arg (KInp out (rhsToFrame rhs'))
rhsToFrame (LetComputation out comp rhs') = KComputation comp (KInp out (rhsToFrame rhs'))


--------------------------------- Machinery for higher-order terms ----------------------

class NormalizeBoundVars f where
  normalizeBoundVars' :: (MonadMatchable m) => f -> m f

normalizeBoundVars :: (MonadMatchable m, NormalizeBoundVars f) => f -> m f
normalizeBoundVars f = withFreshCtx $ withVarAllocator mkNegativeVarAllocator $ normalizeBoundVars' f

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

-------------------------------------- Matching ------------------------------------------

instance (LangBase l, Matchable (Configuration l)) => Matchable (PosFrame l) where
  getVars (KBuild       c  ) = getVars c
  getVars (KStepTo      c f) = getVars c `Set.union` getVars f
  getVars (KComputation c f) = getVars c `Set.union` getVars f

  match (Pattern (KBuild       c1   )) (Matchee (KBuild       c2   )) = match (Pattern c1) (Matchee c2)
  match (Pattern (KStepTo      c1 f1)) (Matchee (KStepTo      c2 f2)) = match (Pattern c1) (Matchee c2) >> match (Pattern f1) (Matchee f2)
  match (Pattern (KComputation c1 f1)) (Matchee (KComputation c2 f2)) = match (Pattern c1) (Matchee c2) >> match (Pattern f1) (Matchee f2)
  match _ _ = mzero

  refreshVars (KBuild       c  ) = KBuild       <$> refreshVars c
  refreshVars (KStepTo      c f) = KStepTo      <$> refreshVars c <*> refreshVars f
  refreshVars (KComputation c f) = KComputation <$> refreshVars c <*> refreshVars f

  fillMatch (KBuild       c  ) = KBuild       <$> fillMatch c
  fillMatch (KStepTo      c f) = KStepTo      <$> fillMatch c <*> fillMatch f
  fillMatch (KComputation c f) = KComputation <$> fillMatch c <*> fillMatch f

instance (LangBase l, Matchable (Configuration l)) => Matchable (Frame l) where
  getVars (KInp c pf) = getVars c `Set.union` getVars pf

  -- The vars in the input are bound. Note that we clear them!
  match (Pattern (KInp c1 pf1)) (Matchee (KInp c2 pf2)) = do
    match (Pattern c1) (Matchee c2)
    match (Pattern pf1) (Matchee pf2)
    forM_ (Set.toList $ getVars c1) $ \v -> clearVar v

  refreshVars (KInp c pf) = KInp <$> refreshVars c <*> refreshVars pf

  fillMatch   (KInp c pf) = withSubCtx $ do
    forM_ (Set.toList $ getVars c) $ \v -> clearVar v
    KInp c <$> fillMatch pf

instance (LangBase l, Matchable (Configuration l)) => Matchable (Context l) where
  getVars KHalt       = Set.empty
  getVars (KPush f c) = getVars f `Set.union` getVars c
  getVars (KVar v)    = Set.singleton v

  match (Pattern  KHalt)        (Matchee  KHalt)        = return ()
  match (Pattern (KPush f1 c1)) (Matchee (KPush f2 c2)) = match (Pattern f1) (Matchee f2) >> match (Pattern c1) (Matchee c2)
  match (Pattern (KVar v))      (Matchee x)             = putVar v x
  match _ _ = mzero

  refreshVars KHalt       = return KHalt
  refreshVars (KPush f c) = KPush <$> refreshVars f <*> refreshVars c
  refreshVars (KVar v)    = KVar  <$> refreshVar id v

  fillMatch KHalt       = return KHalt
  fillMatch (KPush f c) = KPush <$> fillMatch f <*> fillMatch c
  fillMatch (KVar v)    = getVarMaybe v return (return $ KVar v)
