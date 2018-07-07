{-# LANGUAGE DataKinds, EmptyDataDecls, FlexibleContexts, FlexibleInstances, GADTs, TupleSections, UndecidableInstances, ViewPatterns #-}

module Matching (
    MonadMatchable(..)
  , getVars
  , Match
  , runMatch

  , Matchable(..)

  , EmptyState(..)
  ) where

import Control.Monad ( MonadPlus(..), guard )
import Control.Monad.State ( MonadState(..), StateT, evalStateT, modify )
import Control.Monad.Trans ( lift )

import Data.Dynamic ( Dynamic, toDyn, fromDynamic)
import Data.Map ( Map, (!) )
import qualified Data.Map as Map
import Data.Typeable ( Typeable )

import Configuration
import Term
import Var

newtype MatchState = MatchState { getMatchState :: Map MetaVar Dynamic }

modMatchState :: (Map MetaVar Dynamic -> Map MetaVar Dynamic) -> MatchState -> MatchState
modMatchState f = MatchState . f . getMatchState

-- Maybe is on inside; means state will reset on backtrack
type Match = StateT MatchState Maybe

runMatch :: Match a -> Maybe a
runMatch m = evalStateT m (MatchState Map.empty)

class (MonadPlus m) => MonadMatchable m where
  putVar :: (Typeable a, Eq a) => MetaVar -> a -> m ()
  getVarMaybe :: Typeable a => MetaVar -> (a -> m b) -> m b -> m b
  getVarDefault :: Typeable a => MetaVar -> m a -> m a
  getVar :: Typeable a => MetaVar -> m a
  withFreshCtx :: m a -> m a

  getVarDefault v = getVarMaybe v return

instance (MonadState MatchState m, MonadPlus m) => MonadMatchable m where
  putVar var val = do curVal <- getVarMaybe var (return.Just) (return Nothing)
                      case curVal of
                        Nothing   -> modify (modMatchState $ Map.insert var (toDyn val))
                        Just val' -> guard (val == val')

  getVarMaybe var f def = do maybeDyn <- Map.lookup var <$> getMatchState <$> get
                             case (maybeDyn :: Maybe Dynamic) of
                               Nothing -> def
                               Just x  -> maybe def f (fromDynamic x)

  getVar var = getVarDefault var mzero

  withFreshCtx m = do old <- get
                      put $ MatchState Map.empty
                      res <- m
                      put old
                      return res


getVars :: (MonadMatchable m, Typeable (Term l)) => [MetaVar] -> m [Term l Closed]
getVars = mapM getVar

class (HasVars f) => Matchable f where
  -- Extra precondition:
  -- Variables in an open term may be in "template" or "pattern" position. This distinction is not made syntactially.
  -- Currently, the only example of a template variable is the LHSs of bindings in SimpEnvMap.
  --
  -- Prior to calling match, all template variables must be filled in, either because there were none, or
  -- by calling partiallyFillMatch. This includes recursive calls to match in match instances.
  --
  match :: (MonadMatchable m) => f Open -> f Closed -> m ()

  partiallyFillMatch :: (MonadMatchable m) => f Open -> m (f Open)
  fillMatch :: (MonadMatchable m)  => f Open -> m (f Closed)


fillMatchTermGen :: (MonadMatchable m, Typeable (Term l)) => (MetaVar -> m (Term l v)) -> Term l Open -> m (Term l v)
fillMatchTermGen f (Node s ts)   = Node s <$> (mapM (fillMatchTermGen f) ts)
fillMatchTermGen f (IntNode s i) = return (IntNode s i) -- This could just be unsafeCoerce....or hopefully just coerce
fillMatchTermGen f (MetaVar v)   = f v

instance (Typeable (Term l)) => Matchable (Term l) where
  match (Node s1 ts1)   (Node s2 ts2)
    | (s1 == s2)                        = sequence_ $ zipWith match ts1 ts2
  match (IntNode s1 i1) (IntNode s2 i2)
    | (s1 == s2) && (i1 == i2)          = return ()
  match (MetaVar v)     t               = putVar v t
  match _               _               = mzero


  partiallyFillMatch = fillMatchTermGen (\v -> getVarDefault v (return $ MetaVar v))
  fillMatch = fillMatchTermGen getVar


instance {-# OVERLAPPABLE #-} (Matchable (Term l), Matchable s) => Matchable (GConfiguration l s) where
  match (Conf t1 s1) (Conf t2 s2) = do match t1 t2
                                       s1' <- partiallyFillMatch s1
                                       match s1' s2

  partiallyFillMatch (Conf t s) = Conf <$> partiallyFillMatch t <*> partiallyFillMatch s
  fillMatch          (Conf t s) = Conf <$>          fillMatch t <*>          fillMatch s

-- Hack to prevent over-eagerly expanding (Matchable (Configuration l)) constraints
data UnusedLanguage
instance {-# OVERLAPPING #-} (HasVars s) => Matchable (GConfiguration UnusedLanguage s) where
  match = error "Matching UnusedLanguage"
  partiallyFillMatch = error "Matching UnusedLanguage"
  fillMatch = error "Matching UnusedLanguage"

instance Matchable EmptyState where
  match _ _ = return ()
  partiallyFillMatch EmptyState = return EmptyState
  fillMatch EmptyState = return EmptyState

-- Description: We implemented a restricted version of ACI
-- matching specialized to the kinds of patterns used in operational semantics,
-- e.g.: \Gamma, x=v. Our implementation imposes the following restrictions:
-- the match pattern must be linear, all but one pattern variable may only match one element,
-- and all bindings in the match pattern must have a concrete key.
--
-- These restrictions are actually desirable because general ACI-matching is NP-hard,
-- and operational semantics are generally written to not require solving NP-hard problems
-- to compute a single step of reduction. With these restrictions, on the other hand,
-- assuming constant-time map operations, matching can easily be implemented in time linear
-- in the size of the matched term and pattern.
--
-- This restricted version is not sufficient to encode type systems such as the basic presentation of the linear lambda calculus,
-- which may match a linear context against a pattern of the form \Delta, \Delta' (i.e.: try an arbitrary context split).
-- However, it is sufficient to implement the "resource-tracking" version of the type system of the linear lambda calculus,
-- as well as the operational semantics of the linear lambda calculus (which are almost the same as that of the
-- simply-typed lambda calculus with sums and products).



-- Curses for this not existing; will give a not-so-efficient implementation
mapKeysM :: (Ord k2, Applicative m) => (k1 -> m k2) -> Map k1 a -> m (Map k2 a)
mapKeysM f mp = Map.fromList <$> traverse (\(k,v) -> (,v) <$> f k) (Map.toList mp)

-- |
-- Important notes on matching maps:
--  partiallyFillMatch will not touch "Rest" variables

instance (Matchable a, Matchable b) => Matchable (SimpEnvMap a b) where
  match (SimpEnvMap (assumeClosedKeys -> m1)) (SimpEnvMap m2) =
    if Map.keys m1 /= assumeClosedList (Map.keys m2) then
      mzero
    else
      -- Use (!) because guaranteed has key
      sequence_ $ Map.mapWithKey (\k v -> match v (m2 ! k)) m1

  partiallyFillMatch (SimpEnvMap m) = SimpEnvMap <$> (mapKeysM partiallyFillMatch =<< mapM partiallyFillMatch m)
  fillMatch (SimpEnvMap m) = SimpEnvMap <$> (mapKeysM fillMatch =<< mapM fillMatch m)

instance (Matchable a, Matchable b) => Matchable (SimpEnv a b) where
  match (SimpEnvRest v m1) (JustSimpMap m2) = do
    let (mp1, mp2) = (assumeClosedKeys (getSimpEnvMap m1), getSimpEnvMap m2)
    putVar v (SimpEnvMap $ Map.difference mp2 mp1)
    match m1 (SimpEnvMap $ Map.intersection mp2 mp1)

  match (JustSimpMap m1) (JustSimpMap m2) = match m1 m2

  partiallyFillMatch (SimpEnvRest v m) = SimpEnvRest v <$> partiallyFillMatch m
  partiallyFillMatch (JustSimpMap m)   = JustSimpMap   <$> partiallyFillMatch m

  fillMatch (SimpEnvRest v m1) = do m2 <- getVar v
                                    m1' <- fillMatch m1

                                    -- Order of Map.union is important.
                                    -- When there is conflict, will prefer the explicitly given keys
                                    return $ JustSimpMap $ SimpEnvMap (Map.union (getSimpEnvMap m1') (getSimpEnvMap m2))

  fillMatch (JustSimpMap m) = JustSimpMap <$> fillMatch m


