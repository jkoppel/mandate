{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}

module Unification (
    MonadUnify(..)
  , Unifiable(..)
  ) where

import Control.Monad ( guard, forM_, MonadPlus(..) )

import Data.Map ( Map, (!) )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Typeable ( Typeable )

import Configuration
import Matching
import Matching.Class
import Term
import Var

--------------------------------------------------------------------------------------------------

occursCheck :: (Matchable m) => MetaVar -> m -> Bool
occursCheck v m = v `Set.member` getVars m

instance (MonadMatchable m) => MonadUnify m where
  elimVar v x = do guard (not $ occursCheck v x)

                   -- Eliminating only one variable. Why? Because that's how the textbook gives it,
                   -- and I don't want to think hard about this ATM
                   modifyVars (\v x -> withFreshCtx $ do putVar v x
                                                         fillMatch x)
                   putVar v x


unifyTerm' :: forall l m. (Unifiable (Term l), MonadUnify m) => Term l -> Term l -> m ()
unifyTerm' (Node f1 ts1) (Node f2 ts2) = do guard (f1 == f2)
                                            forM_ (zip ts1 ts2) $ \(t1, t2) -> unify t1 t2
unifyTerm' (Val  f1 ts1) (Val  f2 ts2) = do guard (f1 == f2)
                                            forM_ (zip ts1 ts2) $ \(t1, t2) -> unify t1 t2
unifyTerm' (IntNode s1 i1) (IntNode s2 i2) = guard (s1 == s2) >> guard (i1 == i2)
unifyTerm' (StrNode s1 x1) (StrNode s2 x2) = guard (s1 == s2) >> guard (x1 == x2)

unifyTerm'   (ValVar v) t@(Val  _ _) = elimVar v t
unifyTerm' t@(Val _ _)    (ValVar v) = elimVar v t

unifyTerm'   (NonvalVar v) t@(Node _ _)    = elimVar v t
unifyTerm' t@(Node _ _)      (NonvalVar v) = elimVar v t

unifyTerm' (MetaVar v) t@(Val     _ _) = elimVar v t
unifyTerm' (MetaVar v) t@(Node    _ _) = elimVar v t
unifyTerm' (MetaVar v) t@(IntNode _ _) = elimVar v t
unifyTerm' (MetaVar v) t@(StrNode _ _) = elimVar v t
unifyTerm' t@(Val     _ _) (MetaVar v) = elimVar v t
unifyTerm' t@(Node    _ _) (MetaVar v) = elimVar v t
unifyTerm' t@(IntNode _ _) (MetaVar v) = elimVar v t
unifyTerm' t@(StrNode _ _) (MetaVar v) = elimVar v t

unifyTerm' (MetaVar    v1)   t@(GMetaVar v2 _) = elimVar v1 t
unifyTerm' t@(GMetaVar v1 _)   (MetaVar  v2)   = elimVar v2 t

unifyTerm' (ValVar    v) t@(ValVar    _) = elimVar v t
unifyTerm' (NonvalVar v) t@(NonvalVar _) = elimVar v t

unifyTerm' (GMetaVar v1 mt1) t@(GMetaVar v2 mt2) =
  case mt1 `matchTypeMeet` mt2 of
    Just mtMeet -> if v1 == v2 then
                     return () -- FIXME: Can I get rid of this case
                   else
                     elimVar v1 (GMetaVar @l v2 mtMeet)

    Nothing -> mzero

-- No unification on star nodes


unifyTerm' _ _ = mzero

instance (Matchable (Term l)) => Unifiable (Term l) where
  unify a b = do a' <- fillMatch a
                 b' <- fillMatch b
                 unifyTerm' a b

instance (Unifiable a, Unifiable b, Matchable (a,b)) => Unifiable (a, b) where
  unify a b = do
          unify (fst a) (fst b)
          unify (snd a) (snd b)

instance {-# OVERLAPPABLE #-} (Typeable (GConfiguration s l)) => Unifiable (GConfiguration s l) where
  unify (Conf t1 s1) (Conf t2 s2) = unify t1 t2 >> unify s1 s2

-- Hack to prevent over-eagerly expanding (Matchable (Configuration l)) constraints
data UnusedLanguage
instance {-# OVERLAPPING #-} (Unifiable s) => Unifiable (GConfiguration s UnusedLanguage) where
  unify = error "Unifying UnusedLanguage"

instance Unifiable EmptyState where
  unify EmptyState EmptyState = return ()

instance Unifiable () where
  unify () () = return ()


--------------------------- SimpEnvMap unification ----------------------------

instance {-# OVERLAPPABLE #-} (Matchable a, Unifiable b) => Unifiable (SimpEnvMap a b) where
  unify (SimpEnvMap m1) (SimpEnvMap m2) = do
    m1' <- mapKeysM fillMatch m1
    m2' <- mapKeysM fillMatch m2

    if Map.keys m1 /= Map.keys m2 then
      mzero
    else
      sequence_ $ Map.mapWithKey (\k v -> unify v (m2 ! k)) m1'

instance {-# OVERLAPPING #-} (Matchable UnusedLanguage) => Unifiable (SimpEnvMap UnusedLanguage UnusedLanguage) where
  unify = error "Unifying SimpEnvMap of UnusedLanguage"


unifySimpEnv :: (MonadUnify m, Matchable a, Unifiable b, Unifiable (SimpEnvMap a b)) => SimpEnv a b -> SimpEnv a b -> m ()
unifySimpEnv (WholeSimpEnv v1) m2                = elimVar v1 m2
unifySimpEnv m1                (WholeSimpEnv v2) = elimVar v2 m1
unifySimpEnv (JustSimpMap m1)  (JustSimpMap m2)  = unify m1 m2

unifySimpEnv (SimpEnvRest v m1) (JustSimpMap m2) = do
    let (mp1, mp2) = (getSimpEnvMap m1, getSimpEnvMap m2)
    let diff = Map.difference mp2 mp1
    elimVar v (JustSimpMap $ SimpEnvMap diff)
    unify m1 (SimpEnvMap $ Map.intersection mp2 mp1)

unifySimpEnv (SimpEnvRest _ _) (SimpEnvRest _ _) = error "Not implemented: Unifying two maps with map vars"

instance (Matchable a, Unifiable b, Matchable (SimpEnv a b)) => Unifiable (SimpEnv a b) where
  unify a b = do a' <- fillMatch a
                 b' <- fillMatch b
                 unifySimpEnv a b