{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ScopedTypeVariables, StandaloneDeriving, UndecidableInstances #-}

module Semantics.Context (
    PosFrame(..)
  , Frame(..)
  , Context(..)
  , rhsToFrame
  ) where

import Control.Monad ( mzero )
import Control.Monad.IO.Class ( liftIO )
import Unsafe.Coerce ( unsafeCoerce )

import Configuration
import Lang
import Matching
import Semantics.General
import Semantics.SOS
import Term
import Var

-- TODO: Intern

data PosFrame l v where
  KBuild       :: !(Configuration l v)                    -> PosFrame l v
  KSideCond    :: !(ExtCond l v)       -> !(PosFrame l v) -> PosFrame l v
  KStepTo      :: !(Configuration l v) -> !(Frame l v)    -> PosFrame l v

  -- Hackiness alert: Currently, computations output terms....but frames can only take confs as input
  -- So, we're doing this with pure hacks for now
  KComputation :: !(ExtComp l v)       -> !(Frame l v )   -> PosFrame l v

deriving instance (Eq (Configuration l v), Eq (ExtCond l v), Eq (Frame l v), LangBase l) => Eq (PosFrame l v)
deriving instance (Show (Configuration l v), Show (ExtCond l v), Show (Frame l v), LangBase l) => Show (PosFrame l v)

-- Rule for frames: all PosFrame's may have no free variables except those bound by a surrounding KInp

data Frame l v where
  KInp :: !(MConf l) -> !(PosFrame l Open) -> Frame l Open


deriving instance (Eq (MConf l), Eq (PosFrame l Open)) => Eq (Frame l v)
deriving instance (Show (MConf l), Show (PosFrame l Open)) => Show (Frame l v)

data Context l v where
  KHalt :: Context l v
  KPush :: !(Frame l v) -> !(Context l v) -> Context l v
  KVar  :: !MetaVar -> Context l Open

deriving instance (Eq (Frame l v)) => Eq (Context l v)
deriving instance (Show (Frame l v)) => Show (Context l v)


rhsToFrame :: (Lang l) => Rhs l -> PosFrame l Open
rhsToFrame (Build c)                     = KBuild c
rhsToFrame (SideCondition cond rhs)      = KSideCond cond (rhsToFrame rhs)
rhsToFrame (LetStepTo out arg rhs')      = KStepTo arg (KInp out (rhsToFrame rhs'))
rhsToFrame (LetComputation out comp rhs) = KComputation comp (KInp (initConf (MetaVar out)) (rhsToFrame rhs))


instance (HasVars (Configuration l)) => HasVars (PosFrame l) where
  assumeClosed (KBuild c)                 = KBuild (assumeClosed c)
  assumeClosed (KSideCond (c, args) pf)   = KSideCond (c, map assumeClosed args) (assumeClosed pf)
  assumeClosed (KStepTo c f)              = KStepTo (assumeClosed c) (assumeClosed f)
  assumeClosed (KComputation (c, args) f) = KComputation (c, map assumeClosed args) (assumeClosed f)

  asOpen = unsafeCoerce

instance (HasVars (Configuration l)) => HasVars (Frame l) where
  assumeClosed (KInp _ _) = error "Assuming context is closed, but has an input frame"
  asOpen = unsafeCoerce

instance (HasVars (Configuration l)) => HasVars (Context l) where
  assumeClosed KHalt = KHalt
  assumeClosed (KPush f c) = KPush (assumeClosed f) (assumeClosed c)
  assumeClosed (KVar _) = error "Assuming context is closed, but has a KVar"

  asOpen = unsafeCoerce


-- TODO: The code in here reflects the confusion about "Closed" terms
instance (Lang l) => Matchable (PosFrame l) where
  match (KBuild c1) (KBuild c2) = match c1 c2
  match (KSideCond (c1, args1) pf1) (KSideCond (c2, args2) pf2)
    | c1 == c2 && length args1 == length args2 = sequence (zipWith match args1 args2) >> match pf1 pf2
  match (KStepTo c1 f1) (KStepTo c2 f2) = match c1 c2 >> match f1 f2
  match (KComputation (c1, args1) f1) (KComputation (c2, args2) f2)
    | c1 == c2 && length args1 == length args2 = sequence (zipWith match args1 args2) >> match f1 f2
  match _ _ = mzero

  refreshVars (KBuild c) = KBuild <$> refreshVars c
  refreshVars (KSideCond (c, args) pf) = mapM refreshVars args >>= \args' -> KSideCond (c, args') <$> refreshVars pf
  refreshVars (KStepTo c f) = KStepTo <$> refreshVars c <*> refreshVars f
  refreshVars (KComputation (c, args) f) = mapM refreshVars args >>= \args' -> KComputation (c, args') <$> refreshVars f

  partiallyFillMatch (KBuild c) = KBuild <$> partiallyFillMatch c
  partiallyFillMatch (KSideCond (c, args) pf) = mapM partiallyFillMatch args >>= \args' -> KSideCond (c, args') <$> partiallyFillMatch pf
  partiallyFillMatch (KStepTo c f) = KStepTo <$> partiallyFillMatch c <*> partiallyFillMatch f
  partiallyFillMatch (KComputation (c, args) f) = mapM partiallyFillMatch args >>= \args' -> KComputation (c, args') <$> partiallyFillMatch f

  fillMatch _ = error "Trying to fill a PosFrame to closed, but all PosFrame's are open"

instance (Lang l) => Matchable (Frame l) where
  match (KInp c1 pf1) (KInp c2 pf2) = match c1 c2 >> match pf1 pf2
  refreshVars (KInp c pf) = KInp <$> refreshVars c <*> refreshVars pf
  partiallyFillMatch (KInp c pf) = KInp <$> partiallyFillMatch c <*> partiallyFillMatch pf

  fillMatch _ = error "Trying to fill a Frame to closed, but all Frame's are open"

instance (Lang l) => Matchable (Context l) where
  match KHalt KHalt = return ()
  match (KPush f1 c1) (KPush f2 c2) = match f1 f2 >> match c1 c2
  match (KVar v1) (KVar v2) = putVar v1 (KVar v2 :: Context l Open)
  match _ _ = mzero

  refreshVars KHalt = return KHalt
  refreshVars (KPush f c) = KPush <$> refreshVars f <*> refreshVars c
  refreshVars (KVar v) = do v' <- liftIO nextVar
                            putVar v (KVar v' :: Context l Open)
                            return $ KVar v'

  partiallyFillMatch KHalt = return KHalt
  partiallyFillMatch (KPush f c) = KPush <$> partiallyFillMatch f <*> partiallyFillMatch c
  partiallyFillMatch (KVar v) = getVarMaybe v (return . asOpen) (return $ KVar v)


  fillMatch _ = error "Trying to fill a Context to closed, but all Context's are open"