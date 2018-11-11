{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Unification (
    MonadUnify(..)
  , Unifiable(..)
  ) where

import Control.Monad ( guard, forM_, MonadPlus(..) )

import Data.Set ( Set )
import qualified Data.Set as Set

import Matching
import Term
import Var

--------------------------------------------------------------------------------------------------

occursCheck :: (Matchable m) => MetaVar -> m -> Bool
occursCheck v m = v `Set.member` getVars m

class (MonadMatchable m) => MonadUnify m where
  elimVar :: (Matchable a, Eq a) => MetaVar -> a -> m ()

instance (MonadMatchable m) => MonadUnify m where
  elimVar v x = do guard (not $ occursCheck v x)

                   -- Eliminating only one variable. Why? Because that's how the textbook gives it,
                   -- and I don't want to think hard about this ATM
                   modifyVars (\v x -> withFreshCtx $ do putVar v x
                                                         fillMatch x)
                   putVar v x

class (Matchable f) => Unifiable f where
  unify :: (MonadUnify m) => f -> f -> m ()


unifyTerm' :: (Unifiable (Term l), MonadUnify m) => Term l -> Term l -> m ()
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

unifyTerm' (GMetaVar v1 mt1) t@(GMetaVar v2 mt2) = do guard (matchTypeCompat mt1 mt2)
                                                      if (v1 == v2) then
                                                        return ()
                                                       else
                                                        elimVar v1 t

-- No unification on star nodes


unifyTerm' _ _ = mzero

instance (Matchable (Term l)) => Unifiable (Term l) where
  unify a b = do a' <- fillMatch a
                 b' <- fillMatch b
                 unifyTerm' a b
