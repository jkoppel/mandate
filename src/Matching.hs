{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, GADTs, ScopedTypeVariables, TupleSections, TypeApplications, UndecidableInstances #-}

module Matching (
    MonadMatchable(..)
  , getVars
  , refreshVar
  , Match
  , runMatch

  , Pattern(..)
  , Matchee(..)

  , Matchable(..)
  , matchList
  , refreshVarsList
  , fillMatchList
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
import LangBase
import MatchEffect
import Term
import Var

data MatchState = MatchState (Map MetaVar Dynamic)

getMatchState :: MatchState -> Map MetaVar Dynamic
getMatchState (MatchState m) = m

modMatchState :: (Map MetaVar Dynamic -> Map MetaVar Dynamic) -> MatchState -> MatchState
modMatchState f (MatchState m) = MatchState $ f m


-- Maybe is on inside; means state will reset on backtrack
type Match = StateT MatchState (MaybeT IO)

runMatch :: Match a -> IO (Maybe a)
runMatch m = runMaybeT $ evalStateT m (MatchState Map.empty)

runMatchEffect :: MatchEffect a -> Match a
runMatchEffect (MatchEffect x) = liftIO x

class (MonadPlus m, MonadIO m) => MonadMatchable m where
  hasVar :: MetaVar -> m Bool
  putVar :: (Typeable a, Eq a) => MetaVar -> a -> m ()
  getVarMaybe :: Typeable a => MetaVar -> (a -> m b) -> m b -> m b
  getVarDefault :: Typeable a => MetaVar -> m a -> m a
  getVar :: Typeable a => MetaVar -> m a
  withFreshCtx :: m x -> m x

  getVarDefault v = getVarMaybe v return
  getVar var = getVarDefault var mzero

instance {-# OVERLAPPABLE #-} (MonadState MatchState m, MonadIO m, MonadPlus m) => MonadMatchable m where
  hasVar var = Map.member var <$> getMatchState <$> get
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

-- Hack to prevent eagerly matching the above instance
data UnusedMonad a
instance {-# OVERLAPPING #-} (MonadPlus UnusedMonad, MonadIO UnusedMonad) => MonadMatchable UnusedMonad where
  putVar = error "Using UnusedMonad"
  getVarMaybe = error "Using UnusedMonad"
  withFreshCtx = error "Using UnusedMonad"

getVars :: (MonadMatchable m, LangBase l) => [MetaVar] -> m [Term l]
getVars = mapM getVar

refreshVar :: (MonadMatchable m, Typeable a, Eq a) => (MetaVar -> a) -> MetaVar -> m MetaVar
refreshVar f v = do v' <- liftIO nextVar
                    putVar v (f v')
                    return v'

-- These exist to make arguments more clear now that we're not putting Open/Closed in the types
-- We do have the option of giving these a smart constructor which checks closed-ness for a more sophisticated definition
-- of closedness which includes binders
-- TODO: "Matchee?" There is a standard name for this, right?
newtype Matchee f = Matchee f
newtype Pattern f = Pattern f

class (Show f) => Matchable f where
  -- TODO: Review these preconditions; I think some no longer hold
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
  match :: (MonadMatchable m) => Pattern f -> Matchee f -> m ()
  refreshVars :: (MonadMatchable m) => f -> m f
  fillMatch :: (MonadMatchable m) => f -> m f

matchList :: (Matchable f, MonadMatchable m) => Pattern [f] -> Matchee [f] -> m ()
matchList (Pattern xs1) (Matchee xs2) = sequence_ $ zipWith match (map Pattern xs1) (map Matchee xs2)

refreshVarsList :: (Matchable f, MonadMatchable m) => [f] -> m [f]
refreshVarsList = mapM refreshVars

fillMatchList :: (Matchable f, MonadMatchable m) => [f] -> m [f]
fillMatchList = mapM fillMatch


fillMatchTermGen :: (MonadMatchable m, LangBase l) => (MetaVar -> m (Term l)) -> Term l -> m (Term l)
fillMatchTermGen f (Node s ts)   = Node s <$> (mapM (fillMatchTermGen f) ts)
fillMatchTermGen f (Val  s ts)   = Val s <$> (mapM (fillMatchTermGen f) ts)
fillMatchTermGen f (IntNode s i) = return (IntNode s i) -- This could just be unsafeCoerce....or hopefully just coerce
fillMatchTermGen f (StrNode s x) = return (StrNode s x) -- This could just be unsafeCoerce....or hopefully just coerce
fillMatchTermGen f (MetaVar v)   = f v

instance (LangBase l) => Matchable (Term l) where
  match (Pattern (Node s1 ts1))   (Matchee (Node s2 ts2))
    | (s1 == s2)                              = matchList (Pattern ts1) (Matchee ts2)
  match (Pattern (Val  s1 ts1))   (Matchee (Val  s2 ts2))
    | (s1 == s2)                              = matchList (Pattern ts1) (Matchee ts2)
  match (Pattern (IntNode s1 i1)) (Matchee (IntNode s2 i2))
    | (s1 == s2) && (i1 == i2)                = return ()
  match (Pattern (MetaVar v))     (Matchee t) = putVar v t
  match _               _                     = mzero

  refreshVars = fillMatchTermGen (\v -> getVarMaybe v return (refresh v))
    where
      refresh :: (MonadMatchable m, LangBase l) => MetaVar -> m (Term l)
      refresh v = MetaVar <$> refreshVar (MetaVar @l) v
  fillMatch = fillMatchTermGen (\v -> getVarMaybe v return (return $ MetaVar v))


instance {-# OVERLAPPABLE #-} (Matchable (Term l), Matchable s) => Matchable (GConfiguration l s) where
  match (Pattern (Conf t1 s1)) (Matchee (Conf t2 s2)) = do
      match (Pattern t1) (Matchee t2)
      match (Pattern s1) (Matchee s2)

  refreshVars (Conf t s) = Conf <$> refreshVars t <*> refreshVars s
  fillMatch   (Conf t s) = Conf <$> fillMatch   t <*> fillMatch   s

-- Hack to prevent over-eagerly expanding (Matchable (Configuration l)) constraints
data UnusedLanguage
instance {-# OVERLAPPING #-} (Show s) => Matchable (GConfiguration UnusedLanguage s) where
  match = error "Matching UnusedLanguage"
  refreshVars = error "Matching UnusedLanguage"
  fillMatch = error "Matching UnusedLanguage"

instance Matchable EmptyState where
  match _ _ = return ()
  refreshVars EmptyState = return EmptyState
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
  match (Pattern (SimpEnvMap m1)) (Matchee (SimpEnvMap m2)) = do
    m1' <- mapKeysM fillMatch m1
    if Map.keys m1' /= Map.keys m2 then
      mzero
    else
      -- Use (!) because guaranteed has key
      sequence_ $ Map.mapWithKey (\k v -> match (Pattern v) (Matchee (m2 ! k))) m1'

  refreshVars (SimpEnvMap m) = SimpEnvMap <$> (mapKeysM refreshVars =<< mapM refreshVars m)
  fillMatch   (SimpEnvMap m) = SimpEnvMap <$> (mapKeysM fillMatch   =<< mapM fillMatch   m)


declareTypesEq :: (Monad m) => a -> a -> m ()
declareTypesEq _ _ = return ()

instance (Matchable a, Matchable b, Typeable a) => Matchable (SimpEnv a b) where
  match (Pattern p@(SimpEnvRest v m1)) (Matchee m@(JustSimpMap m2)) = do
    vFilled <- getVarMaybe v (return.Just) (return Nothing)
    case vFilled of
      Just (SimpEnvMap vm) -> do declareTypesEq (SimpEnvMap vm) m1
                                 p' <- fillMatch p
                                 match (Pattern p') (Matchee m)
      _ -> do
        m1' <- fillMatch m1
        let (mp1, mp2) = (getSimpEnvMap m1', getSimpEnvMap m2)
        let diff = Map.difference mp2 mp1
        debugStepM $ "Matching maps " ++ show mp1 ++ " and " ++ show mp2
        debugStepM $ "Binding var " ++ show v ++ " to " ++ show diff
        putVar v (SimpEnvMap diff)
        match (Pattern m1) (Matchee (SimpEnvMap $ Map.intersection mp2 mp1))

  match (Pattern (JustSimpMap m1)) (Matchee (JustSimpMap m2)) = match (Pattern m1) (Matchee m2)
  match (Pattern (SimpEnvRest v1 m1)) (Matchee (SimpEnvRest v2 m2)) = putVar v1 v2 >> match (Pattern m1) (Matchee m2)

  -- TODO: Do we need a case for matching SimpMap with EnvRest? (Beware infinite recursion if so)

  refreshVars (JustSimpMap m)   = JustSimpMap <$> refreshVars m
  refreshVars (SimpEnvRest v m) = SimpEnvRest <$> refreshVar id v <*> refreshVars m

  fillMatch (SimpEnvRest v m1) = do
    mapped <- hasVar v
    if mapped then do
      filledVar <- getVarMaybe v (return.Just) (return Nothing)
      filledMap <- getVarMaybe v (return.Just) (return Nothing)

      case (filledVar, filledMap) of
        (Just v', Nothing) -> SimpEnvRest v' <$> fillMatch m1
        (Nothing, Just m2) -> do m1' <- fillMatch m1

                                 -- Order of Map.union is important.
                                 -- When there is conflict, will prefer the explicitly given keys
                                 return $ JustSimpMap $ SimpEnvMap (Map.union (getSimpEnvMap m1') (getSimpEnvMap m2))
     else
      SimpEnvRest v <$> fillMatch m1

  fillMatch (JustSimpMap m) = JustSimpMap <$> fillMatch m


