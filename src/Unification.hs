{-# LANGUAGE EmptyDataDecls, EmptyDataDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}

module Unification (
    MonadUnify(..)
  , Unifiable(..)
  ) where

import Control.Monad ( guard, when, forM_, MonadPlus(..) )

import Data.List ( sort )
import Data.Map ( Map, (!) )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Typeable ( Typeable )

import Control.Monad.HT ( liftJoin2 )

import Configuration
import Debug
import Lattice
import Matching
import Matching.Class
import Term
import Var

--------------------------------------------------------------------------------------------------

-----
--
-- We have some serious scoping issues: variables in rules can leak into terms,
-- variables in binders can be the same between two terms. (They should not be treated the same,
-- but are.)
--
-- (Don't currently know a better place to put this)


-- FIXME: This isn't quite right. No good way currently to narrow a variable's match type
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
                                            forM_ (zip ts1 ts2) $ \(t1, t2) ->
                                              liftJoin2 unify (fillMatch t1) (fillMatch t2)
unifyTerm' (Val  f1 ts1) (Val  f2 ts2) = do guard (f1 == f2)
                                            forM_ (zip ts1 ts2) $ \(t1, t2) ->
                                              liftJoin2 unify (fillMatch t1) (fillMatch t2)
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

unifyTerm' (GMetaVar v1 mt1) t@(GMetaVar v2 mt2) =
  case mt1 `meet` mt2 of
    Just mtMeet -> if v1 == v2 then
                     return () -- FIXME: Can I get rid of this case
                   else
                     let [vMin, vMax] = sort [v1, v2] in
                     elimVar vMin (GMetaVar @l vMax mtMeet)

    Nothing -> mzero

-- For this case, maybe we should be binding v1 to a new variable,
-- but this was easiier
unifyTerm' (GMetaVar v1 mt1) (GStar mt2)    = case mt1 `meet` mt2 of
                                                Just mtMeet -> elimVar v1 (GStar @l mtMeet)
                                                Nothing -> mzero
unifyTerm' t1@(GStar _)   t2@(GMetaVar _ _) = unifyTerm' t2 t1

unifyTerm' (GStar mt1) (GStar mt2) = case mt1 `meet` mt2 of
                                       Just _  -> return ()
                                       Nothing -> mzero

unifyTerm' _ _ = mzero

instance (Matchable (Term l)) => Unifiable (Term l) where
  unify a b = do a' <- fillMatch a
                 b' <- fillMatch b
                 unifyTerm' a b

instance (Unifiable a, Unifiable b, Matchable (a,b)) => Unifiable (a, b) where
  unify a b = do
          unify (fst a) (fst b)
          liftJoin2 unify (fillMatch (snd a)) (fillMatch (snd b))

instance {-# OVERLAPPABLE #-} (Matchable (GConfiguration s l)) => Unifiable (GConfiguration s l) where
  unify (Conf t1 s1) (Conf t2 s2) = unify t1 t2 >> debugM "Unified conf term" >> unify s1 s2

-- Hack to prevent over-eagerly expanding (Matchable (Configuration l)) constraints
data UnusedLanguage deriving ( Eq )

instance Meetable UnusedLanguage where
  meet      = error "Using Meetable instance for UnusedLanguage"
  isMinimal = error "Using Meetable instance for UnusedLanguage"

instance UpperBound UnusedLanguage where
  top = error "Using UpperBound instance for UnusedLanguage"
  upperBound = error "Using UpperBound instance for UnusedLanguage"

instance {-# OVERLAPPING #-} (Unifiable s) => Unifiable (GConfiguration s UnusedLanguage) where
  unify = error "Unifying UnusedLanguage"

instance Unifiable EmptyState where
  unify EmptyState EmptyState = return ()

instance Unifiable () where
  unify () () = return ()


--------------------------- SimpEnvMap unification ----------------------------

-- Duplicate of forMap in Matching.hs; missing a home
forMap :: Map k a -> (k -> a -> b) -> Map k b
forMap = flip Map.mapWithKey

-- See comment about Match implementation in Matching.hs
instance {-# OVERLAPPABLE #-} (Matchable a, Unifiable b, Matchable (SimpEnvMap a b), UpperBound b)
                                => Unifiable (SimpEnvMap a b) where
  unify (SimpEnvMap m1) (SimpEnvMap m2) = do
    m1' <- mapKeysM fillMatch m1
    m2' <- mapKeysM fillMatch m2
    debugM $ "Doing inner map unify of " ++ show m1' ++ " and " ++ show m2'

    sequence_ $ forMap m1' $ \k1 v1 ->
      let matching = Map.filterWithKey (\k2 _ -> (k1 `meet` k2) /= Nothing) m2' in
      debugM (show k1 ++ " matches " ++ (show $ Map.keys matching)) >>
      case Map.elems matching of
        []     -> mzero
        (x:xs) -> unify v1 (foldl upperBound x xs)

instance {-# OVERLAPPING #-} (Matchable UnusedLanguage) => Unifiable (SimpEnvMap UnusedLanguage UnusedLanguage) where
  unify = error "Unifying SimpEnvMap of UnusedLanguage"


-- TODO: Some of these cases are probably redundant
unifySimpEnv :: forall a b m. ( MonadUnify m, Matchable a, Unifiable b, Matchable (SimpEnv a b)
                              , Unifiable (SimpEnvMap a b), UpperBound b)
                                 => SimpEnv a b -> SimpEnv a b -> m ()
unifySimpEnv (WholeSimpEnv v1) (WholeSimpEnv v2) = if v1 == v2 then
                                                     return ()
                                                   else
                                                     let [vMin, vMax] = sort [v1, v2] in
                                                     elimVar vMin (WholeSimpEnv @a @b vMax)
unifySimpEnv (WholeSimpEnv v1) m2                = elimVar v1 m2
unifySimpEnv m1                (WholeSimpEnv v2) = elimVar v2 m1
unifySimpEnv (JustSimpMap m1)  (JustSimpMap m2)  = unify m1 m2

unifySimpEnv (SimpEnvRest v m1) (JustSimpMap m2) = do
    let (mp1, mp2) = (getSimpEnvMap m1, getSimpEnvMap m2)
    let (restMap, innerMap) = partitionAbstractMap mp1 mp2
    elimVar v (JustSimpMap $ SimpEnvMap restMap)
    unify m1 (SimpEnvMap innerMap)

unifySimpEnv t1@(JustSimpMap _) t2@(SimpEnvRest _ _) = unifySimpEnv t2 t1

unifySimpEnv t1@(SimpEnvRest v1 m1) t2@(SimpEnvRest v2 m2) =
    if v1 <= v2 then do
      let (mp1, mp2) = (getSimpEnvMap m1, getSimpEnvMap m2)
      let (restMap, innerMap) = partitionAbstractMap mp1 mp2

      -- Idea for implementation: The var would need to contain an upper bound of all the known key/val pairs
      when (v1 == v2) $ error "Unifying two maps with the same variable not implemented"

      elimVar v1 (SimpEnvRest v2 (SimpEnvMap restMap))
      unify m1 (SimpEnvMap innerMap)
    else
      unifySimpEnv t2 t1


instance (Matchable a, Unifiable b, Matchable (SimpEnv a b), Unifiable (SimpEnvMap a b), UpperBound b)
            => Unifiable (SimpEnv a b) where
  unify a b = do a' <- fillMatch a
                 b' <- fillMatch b
                 debugM "Unifying simpenv"
                 unifySimpEnv a b
                 debugM "Unified simpenv"