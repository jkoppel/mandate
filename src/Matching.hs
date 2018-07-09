{-# LANGUAGE DataKinds, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, InstanceSigs, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, TupleSections, TypeApplications, UndecidableInstances, ViewPatterns #-}

module Matching (
    MonadMatchable(..)
  , getVars
  , Match
  , runMatch

  , Matchable(..)
  , module MatchEffect
  , runMatchEffect

  , EmptyState(..)
  ) where

import Control.Monad ( MonadPlus(..), guard )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( MonadState(..), StateT, evalStateT, modify )
import Control.Monad.Trans.Maybe ( MaybeT(..) )

import Data.Dynamic ( Dynamic, toDyn, fromDynamic)
import Data.Map ( Map, (!) )
import qualified Data.Map as Map
import Data.Typeable ( Typeable )

import Configuration
import Debug
import MatchEffect
import Term
import Var

data MatchState (v :: OpenClosed) where
  MatchState :: Typeable v => Map MetaVar Dynamic -> MatchState v

getMatchState :: MatchState v -> Map MetaVar Dynamic
getMatchState (MatchState m) = m

modMatchState :: (Map MetaVar Dynamic -> Map MetaVar Dynamic) -> MatchState v -> MatchState v
modMatchState f (MatchState m) = MatchState $ f m


-- Maybe is on inside; means state will reset on backtrack
type Match v = StateT (MatchState v) (MaybeT IO)

runMatch :: (Typeable v) => Match v a -> IO (Maybe a)
runMatch m = runMaybeT $ evalStateT m (MatchState Map.empty)

runMatchEffect :: MatchEffect a -> Match v a
runMatchEffect (MatchEffect x) = liftIO x

-- TODO: Now that I've generalized this to not only take closed stuff,
-- I don't know what I get by restricting entries to "a v" instead of arbitrary Dynamic's
class (MonadPlus m, MonadIO m, Typeable v) => MonadMatchable (v :: OpenClosed) m | m -> v where
  putVar :: (Typeable a, Eq (a v)) => MetaVar -> a v -> m ()
  getVarMaybe :: Typeable a => MetaVar -> (a v -> m b) -> m b -> m b
  getVarDefault :: Typeable a => MetaVar -> m (a v) -> m (a v)
  getVar :: Typeable a => MetaVar -> m (a v)
  withFreshCtx :: m x -> m x

  getVarDefault v = getVarMaybe v return
  getVar var = getVarDefault var mzero

instance (MonadState (MatchState v) m, MonadIO m, MonadPlus m, Typeable v) => MonadMatchable v m where
  putVar var val = do curVal <- getVarMaybe var (return.Just) (return Nothing)
                      case curVal of
                        Nothing   -> do debugStepM $ "Setting var " ++ show var
                                        modify (modMatchState $ Map.insert var (toDyn val))
                        Just val' -> guard (val == val')

  getVarMaybe var f def = do maybeDyn <- Map.lookup var <$> getMatchState <$> get
                             case (maybeDyn :: Maybe Dynamic) of
                               Nothing -> def
                               Just x  -> maybe def f (fromDynamic x)

  withFreshCtx m = do old <- get
                      put $ MatchState Map.empty
                      res <- m
                      put old
                      return res


getVars :: (MonadMatchable v m, Typeable (Term l)) => [MetaVar] -> m [Term l v]
getVars = mapM getVar

class (HasVars f, ForallOC Show f) => Matchable f where
  -- Extra precondition:
  -- Variables in an open term may be in "template" or "pattern" position. This distinction is not made syntactially.
  -- Currently, the only example of a template variable is the LHSs of bindings in SimpEnvMap.
  --
  -- Prior to calling match, all template variables must be filled in, either because there were none, or
  -- by calling partiallyFillMatch. This includes recursive calls to match in match instances.
  --

  -- While you can call (match t1 t2) where t2 is open, if you do, all variables in t2 will be considered closed,
  -- i.e.: distinctly numbered variables will be assumed to stand for distinct terms. In other words,
  -- the term f(x,y) will not match the pattern f(z,z).
  match :: (MonadMatchable v m) => f Open -> f v -> m ()

  refreshVars :: (MonadMatchable Open m) => f Open -> m (f Open)

  partiallyFillMatch :: (MonadMatchable v m) => f Open -> m (f Open)
  fillMatch :: (MonadMatchable Closed m)  => f Open -> m (f Closed)


fillMatchTermGen :: (MonadMatchable v' m, Typeable (Term l)) => (MetaVar -> m (Term l v)) -> Term l Open -> m (Term l v)
fillMatchTermGen f (Node s ts)   = Node s <$> (mapM (fillMatchTermGen f) ts)
fillMatchTermGen f (Val  s ts)   = Val s <$> (mapM (fillMatchTermGen f) ts)
fillMatchTermGen f (IntNode s i) = return (IntNode s i) -- This could just be unsafeCoerce....or hopefully just coerce
fillMatchTermGen f (MetaVar v)   = f v

instance (Typeable (Term l)) => Matchable (Term l) where
  match (Node s1 ts1)   (Node s2 ts2)
    | (s1 == s2)                        = sequence_ $ zipWith match ts1 ts2
  match (Val  s1 ts1)   (Val  s2 ts2)
    | (s1 == s2)                        = sequence_ $ zipWith match ts1 ts2
  match (IntNode s1 i1) (IntNode s2 i2)
    | (s1 == s2) && (i1 == i2)          = return ()
  match (MetaVar v)     t               = putVar v t
  match _               _               = mzero

  refreshVars = fillMatchTermGen (\v -> getVarMaybe v return (refresh v))
    where
      refresh :: (MonadMatchable Open m) => MetaVar -> m (Term l Open)
      refresh v = do v' <- liftIO nextVar
                     let mv = MetaVar v'
                     putVar v mv
                     return mv

  partiallyFillMatch = fillMatchTermGen (\v -> getVarMaybe v (return . asOpen) (return $ MetaVar v))
  fillMatch = fillMatchTermGen getVar


instance {-# OVERLAPPABLE #-} (Matchable (Term l), Matchable s) => Matchable (GConfiguration l s) where
  match (Conf t1 s1) (Conf t2 s2) = do match t1 t2
                                       s1' <- partiallyFillMatch s1
                                       debugStepM $ "Partially filled state: " ++ show s1'
                                       match s1' s2

  refreshVars        (Conf t s) = Conf <$>        refreshVars t <*>        refreshVars s
  partiallyFillMatch (Conf t s) = Conf <$> partiallyFillMatch t <*> partiallyFillMatch s
  fillMatch          (Conf t s) = Conf <$>          fillMatch t <*>          fillMatch s

-- Hack to prevent over-eagerly expanding (Matchable (Configuration l)) constraints
data UnusedLanguage
instance {-# OVERLAPPING #-} (HasVars s, ForallOC Show s) => Matchable (GConfiguration UnusedLanguage s) where
  match = error "Matching UnusedLanguage"
  refreshVars = error "Matching UnusedLanguage"
  partiallyFillMatch = error "Matching UnusedLanguage"
  fillMatch = error "Matching UnusedLanguage"

instance Matchable EmptyState where
  match _ _ = return ()
  refreshVars EmptyState = return EmptyState
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
--  Also cannot refresh rest vars.
--
-- If you fail to close all keys before matching, then you'll get weird behavior

instance (Matchable a, Matchable b) => Matchable (SimpEnvMap a b) where
  -- TODO: Can drop this sig (and the InstanceSigs and ScopedTypeVariables extensions) after
  -- getting quantified class constraints
  match :: forall v m. (MonadMatchable v m) => SimpEnvMap a b Open -> SimpEnvMap a b v -> m ()
  match (SimpEnvMap m1) (SimpEnvMap m2) =
    -- TODO: After we get quantified constraints, can drop the asOpenList
    if asOpenList (Map.keys m1) /= asOpenList (Map.keys m2) then
      mzero
    else
      -- Use (!) because guaranteed has key
      -- TODO: After we get quantified constraints, not needed
      case testOpenClosed @v of
        IsOpen   -> sequence_ $ Map.mapWithKey (\k v -> match v (m2 ! k)) (keysAsOC m1)
        IsClosed -> sequence_ $ Map.mapWithKey (\k v -> match v (m2 ! k)) (keysAsOC m1)

  refreshVars        (SimpEnvMap m) = SimpEnvMap <$> (mapKeysM refreshVars        =<< mapM refreshVars        m)
  partiallyFillMatch (SimpEnvMap m) = SimpEnvMap <$> (mapKeysM partiallyFillMatch =<< mapM partiallyFillMatch m)
  fillMatch          (SimpEnvMap m) = SimpEnvMap <$> (mapKeysM fillMatch          =<< mapM fillMatch          m)

matchSimpEnvOpen :: (Matchable a, Matchable b, MonadMatchable Open m) => SimpEnv a b Open -> SimpEnv a b Open -> m ()
matchSimpEnvOpen (SimpEnvRest v m1) (JustSimpMap m2) = do
  let (mp1, mp2) = (keysAsOC (getSimpEnvMap m1), getSimpEnvMap m2)
  let diff = Map.difference mp2 mp1
  debugStepM $ "Matching maps " ++ show mp1 ++ " and " ++ show mp2
  debugStepM $ "Binding var " ++ show v ++ " to " ++ show diff
  putVar v (SimpEnvMap diff)
  match m1 (SimpEnvMap $ Map.intersection mp2 mp1)

matchSimpEnvClosed :: (Matchable a, Matchable b, MonadMatchable Closed m) => SimpEnv a b Open -> SimpEnv a b Closed -> m ()
matchSimpEnvClosed (SimpEnvRest v m1) (JustSimpMap m2) = do
  let (mp1, mp2) = (keysAsOC (getSimpEnvMap m1), getSimpEnvMap m2)
  let diff = Map.difference mp2 mp1
  debugStepM $ "Matching maps " ++ show mp1 ++ " and " ++ show mp2
  debugStepM $ "Binding var " ++ show v ++ " to " ++ show diff
  putVar v (SimpEnvMap diff)
  match m1 (SimpEnvMap $ Map.intersection mp2 mp1)

instance (Matchable a, Matchable b) => Matchable (SimpEnv a b) where
  -- TODO: Can drop this sig (and the InstanceSigs and ScopedTypeVariables extensions) after
  -- getting quantified class constraints
  match :: forall v m. (MonadMatchable v m) => SimpEnv a b Open -> SimpEnv a b v -> m ()
  match e1@(SimpEnvRest v m1) e2@(JustSimpMap m2) =
    case testOpenClosed @v of
      IsOpen   -> matchSimpEnvOpen   e1 e2
      IsClosed -> matchSimpEnvClosed e1 e2
  match (JustSimpMap m1) (JustSimpMap m2) = match m1 m2
  --match (SimpEnvRest v1 m1) (SimpEnvRest v2 m2) = putVar v1 (MetaVar v2) >> match m1 m2

  refreshVars (JustSimpMap m)   = JustSimpMap   <$> refreshVars m
  refreshVars (SimpEnvRest v m) = SimpEnvRest v <$> refreshVars m


  partiallyFillMatch (SimpEnvRest v m) = SimpEnvRest v <$> partiallyFillMatch m
  partiallyFillMatch (JustSimpMap m)   = JustSimpMap   <$> partiallyFillMatch m

  fillMatch (SimpEnvRest v m1) = do m2 <- getVar v
                                    m1' <- fillMatch m1

                                    -- Order of Map.union is important.
                                    -- When there is conflict, will prefer the explicitly given keys
                                    return $ JustSimpMap $ SimpEnvMap (Map.union (getSimpEnvMap m1') (getSimpEnvMap m2))

  fillMatch (JustSimpMap m) = JustSimpMap <$> fillMatch m


