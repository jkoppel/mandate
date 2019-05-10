{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module Matching.Class (
    MonadMatchable(..)
  , Matchee(..)
  , Pattern(..)
  , Matchable(..)

  , MonadUnify(..)
  , Unifiable(..)
  ) where


import Control.Monad ( MonadPlus(..) )
import Control.Monad.IO.Class ( MonadIO )

import Data.Foldable ( fold )
import Data.Set ( Set )
import Data.Typeable ( Typeable )

import Lattice
import Var


-------------------------------------------------------------------------------------------------------------------


class (MonadPlus m, MonadVarAllocator m, MonadIO m) => MonadMatchable m where
  hasVar :: MetaVar -> m Bool
  putVar :: (Matchable a) => MetaVar -> a -> m ()
  clearVar :: MetaVar -> m ()
  overrideVar :: (Matchable a) => MetaVar -> a -> m ()
  getVarMaybe :: Matchable a => MetaVar -> (a -> m b) -> m b -> m b
  getVarDefault :: Matchable a => MetaVar -> m a -> m a
  getVar :: Matchable a => MetaVar -> m a

  modifyVars :: (forall a. (Matchable a) => MetaVar -> a -> m a) -> m ()

  withSubCtx   :: m x -> m x
  withFreshCtx :: m x -> m x

  withVarAllocator :: VarAllocator -> m x -> m x

  debugVars :: m ()

  overrideVar m x = clearVar m >> putVar m x
  getVarDefault v = getVarMaybe v return
  getVar var = getVarDefault var mzero


------------------------------------------------------------------
------
------ A matching engine for type x is given fundamentally by one function:
------    match :: Pattern x -> x -> m (Pattern x -> m x)
------
------ This is weaker than the current match/fillMatch interface (and less compositional). Note that getVars is only used internally
------ (i.e.: would be a protected method in Java), and refreshVars is just me not knowing how to do higher-order matching
------ (which I think should still sit behind the same interface).
------------------------------------------------------------------


-- These exist to make arguments more clear now that we're not putting Open/Closed in the types
-- We do have the option of giving these a smart constructor which checks closed-ness for a more sophisticated definition
-- of closedness which includes binders
-- TODO: "Matchee?" There is a standard name for this, right?
newtype Matchee f = Matchee f
newtype Pattern f = Pattern f

-- NOTE: I think I need to write SYB/Uniplate infrastructure for vars-containing terms
class (Show f, Typeable f, Meetable f) => Matchable f where
  -- | Returns all meta-syntactic variables contained in an `f`
  getVars :: f -> Set MetaVar

  -- | `match p m` matches the pattern p against m in the current match context.
  -- It uses the failure effect on failure. On success, it binds all metavars in `p` to the corresponding subterms
  -- of `m`.
  --
  --
  -- Extra precondition:
  -- Variables in an open term may be in "template" or "pattern" position. This distinction is not made syntactially.
  -- Currently, the only example of a template variable is the LHSs of bindings in SimpEnvMap.
  --
  -- Prior to calling match, all template variables should be filled in, either because there were none, or
  -- by calling fillMatch. This includes recursive calls to match in match instances.
  --

  -- While you can call (match t1 t2) where t2 is open, if you do, all variables in t2 will be considered closed,
  -- i.e.: distinctly numbered variables will be assumed to stand for distinct terms. In other words,
  -- the term f(x,y) will not match the pattern f(z,z).
  match :: (MonadMatchable m) => Pattern f -> Matchee f -> m ()

  -- | Renames all meta-syntactic variables in the argument with newly allocated variables, binding the old
  -- variables to the new ones
  --
  -- This is used when wrapping a term inside a binder, to prevent the bound variable from shadowing
  -- existing variables. It is particularly used when converting an SOS rule to PAM rules. Consider the following SOS rule:
  --
  --    plus-cong-1:
  --    step(+(0t, 1)) = let 2 = step(0t) in +(2, 1)
  --
  -- Naively, it would be converted into the following two PAM rules:
  --
  --   plus-cong-1-1:
  --   <+(0t, 1) | 9> down  ---->  <0t | [\2 -> +(2, 1)].3> down
  --
  --   plus-cong-1-2:
  --    <2 | [\2 -> +(2, 1)].3> up  ---->  <+(2, 1) | 3> up
  --
  -- This is problematic, because the bound variable "2" in the context on the LHS shadows the variable "2"
  -- used to match the term. Instead, a new name is generated for the bound variable, yielding the following PAM rule:
  --
  --   plus-cong-1-2:
  --    <2 | [\4 -> +(4, 1)].3> up  ---->  <+(2, 1) | 3> up
  refreshVars :: (MonadMatchable m) => f -> m f

  -- | Replaces all metavariables in the argument with their matched values in the current match context
  --
  -- If all metavariables in the argument have been bound, then the return value
  -- will be closed
  fillMatch :: (MonadMatchable m) => f -> m f


class (MonadMatchable m) => MonadUnify m where
  elimVar :: (Matchable a) => MetaVar -> a -> m ()


class (Matchable f) => Unifiable f where
  unify :: (MonadUnify m) => f -> f -> m ()