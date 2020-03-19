{-# LANGUAGE FlexibleContexts, PatternSynonyms, Rank2Types #-}

module Matching.Class (
    MonadMatchable(..)
  , Matchee
  , pattern Matchee
  , Pattern(..)
  , Matchable(..)

  , mapVars
  , symbolizeVars

  , MonadUnify(..)
  , Unifiable(..)

  ) where


import Control.Monad ( MonadPlus(..) )
import Control.Monad.Identity ( runIdentity )
import Control.Monad.IO.Class ( MonadIO )

import Data.Foldable ( fold )
import Data.Set ( Set )
import Data.Typeable ( Typeable )

import Debug
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
newtype Matchee f  = Matchee' f
newtype Pattern f  = Pattern f

-- Note: Implementation will constantly wrap things in this,
-- producing quadratic overhead.
--
-- The way to eliminate it is to add a constructor with a precondition that a
-- term has already been symbolized, and then make that version only available
-- internally within the Matching engine.
--
-- On the other hand, now that I've better figured out the "term with/without variables"
-- distinction, I could also add that back to the type level, as it was in the earliest
-- revisions of Mandate.
pattern Matchee :: (Matchable x) => x -> Matchee x
pattern Matchee x <- Matchee' x where
  Matchee x = Matchee' (assertIsSyms x)

assertIsSyms :: (Matchable x) => x -> x
assertIsSyms x = if isDebug then
                   mapVars (changeVarType assertIsSymOrBound) x
                 else
                   x
  where
    assertIsSymOrBound NormalVar = error "NormalVar where only symbols/bound-vars expected"
    assertIsSymOrBound BoundVar  = BoundVar
    assertIsSymOrBound SymbolVar = SymbolVar


mapVars :: (Matchable f) => (MetaVar -> MetaVar) -> f -> f
mapVars f = runIdentity . mapVarsM (return . f)

symbolizeVars :: (Matchable f) => f -> f
symbolizeVars = mapVars varToSymbolLax

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

  mapVarsM :: (Monad m) => (MetaVar -> m MetaVar) -> f -> m f


  -- | Replaces all metavariables in the argument with their matched values in the current match context
  --
  -- If all metavariables in the argument have been bound, then the return value
  -- will be closed
  fillMatch :: (MonadMatchable m) => f -> m f


class (MonadMatchable m) => MonadUnify m where
  elimVar :: (Matchable a) => MetaVar -> a -> m ()


class (Matchable f) => Unifiable f where
  unify :: (MonadUnify m) => f -> f -> m ()