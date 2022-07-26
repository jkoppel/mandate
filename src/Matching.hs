{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, GADTs, PatternSynonyms, Rank2Types, ScopedTypeVariables, StandaloneDeriving, TypeApplications, UndecidableInstances #-}

module Matching (
    MonadMatchable(..)
  , refreshVarsPreserveType
  , refreshVars
  , fillVars
  , Match
  , matchChoose
  , runMatch
  , runMatchFirst
  , runMatchUnique

  , Pattern(..)
  , Matchee(..)
  , pattern Matchee

  , Matchable(..)
  , mapVars
  , symbolizeVars
  , matchList
  , fillMatchList
  , module MatchEffect
  , runMatchEffect

  , ConstMatchable(..)

  , EmptyState(..)
  ) where

import Control.Monad ( MonadPlus(..), guard, forM_, (=<<), msum )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( MonadState(..), StateT, evalStateT, modify )
import Control.Monad.Trans ( lift )

import Data.Foldable ( fold )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Typeable ( Typeable, cast )

import Control.Monad.Logic ( LogicT(..), runLogicT, observeAllT )

import Configuration
import Debug
import Lattice
import MatchEffect
import Term
import Var

import Matching.Class

data AnyMatchable where
  AnyMatchable :: (Matchable m) => m -> AnyMatchable

instance Show AnyMatchable where
  showsPrec d (AnyMatchable x) = showsPrec d x

data MatchState = MatchState { ms_varMap   :: Map MetaVar AnyMatchable
                             , ms_varAlloc :: VarAllocator
                             }

modVarMap :: (Map MetaVar AnyMatchable -> Map MetaVar AnyMatchable) -> MatchState -> MatchState
modVarMap f m = m { ms_varMap = f (ms_varMap m) }


-- LogicT is on inside; means state will reset on backtrack
type Match = StateT MatchState (LogicT IO)

runMatch :: Match a -> IO [a]
runMatch m = do va <- mkNormalVarAllocator
                observeAllT $ evalStateT m (MatchState Map.empty va)

runMatchFirst :: Match a -> IO (Maybe a)
runMatchFirst m = do va <- mkNormalVarAllocator
                     runLogicT (evalStateT m (MatchState Map.empty va)) (const . return . Just) (return Nothing)

runMatchUnique :: Match a -> IO (Maybe a)
runMatchUnique m = do xs <- runMatch m
                      case xs of
                        []  -> return Nothing
                        [a] -> return (Just a)
                        _   -> error "Called runMatchUnique, but had many results"

runMatchEffect :: MatchEffect a -> Match a
runMatchEffect (MatchEffect x) = lift x


instance MonadVarAllocator Match where
  allocVarM = do ms <- get
                 let (mv, va') = allocVar (ms_varAlloc ms)
                 put $ ms { ms_varAlloc = va' }
                 return mv

matchChoose :: (MonadMatchable m) => [a] -> m a
matchChoose as = msum (map return as)

withModCtxState :: (MonadState MatchState m) => (MatchState -> MatchState) -> m x -> m x
withModCtxState f m = do old <- get
                         modify f
                         res <- m
                         put old
                         return res


hoistMaybe :: MonadPlus m => Maybe a -> m a
hoistMaybe = maybe mzero return

instance {-# OVERLAPPABLE #-} (MonadState MatchState m, MonadVarAllocator m, MonadIO m, MonadPlus m) => MonadMatchable m where
  hasVar var = Map.member var <$> ms_varMap <$> get
  putVar var val = do curVal <- getVarMaybe var (return.Just) (return Nothing)
                      case curVal of
                        Nothing   -> do debugM $ "Setting var " ++ show var ++ " to " ++ show val
                                        modify (modVarMap $ Map.insert var (AnyMatchable val))
                        Just val' -> do newVal <- hoistMaybe $ meet val val'
                                        debugM $ "Met " ++ show val ++ " and " ++ show val'
                                        modify (modVarMap $ Map.insert var (AnyMatchable newVal))

  clearVar var = modify (modVarMap $ Map.delete var)

  getVarMaybe var f def = do maybeAM <- Map.lookup var <$> ms_varMap <$> get
                             case (maybeAM :: Maybe AnyMatchable) of
                               Nothing              -> def
                               Just (AnyMatchable x) -> maybe def f (cast x)

  modifyVars f = do m <- get
                    vm' <- Map.traverseWithKey (\v (AnyMatchable x) -> AnyMatchable <$> f v x) (ms_varMap m)
                    put (m {ms_varMap = vm' })

  withSubCtx          = withModCtxState id
  withFreshCtx        = withModCtxState (\ms -> ms { ms_varMap   = Map.empty })
  withVarAllocator va = withModCtxState (\ms -> ms { ms_varAlloc = va})

  debugVars = debugM =<< (show <$> ms_varMap <$> get)

-- Hack to prevent eagerly matching the above instance
data UnusedMonad a
instance {-# OVERLAPPING #-} (MonadPlus UnusedMonad, MonadVarAllocator UnusedMonad, MonadIO UnusedMonad) => MonadMatchable UnusedMonad where
  hasVar = error "Using UnusedMonad"
  putVar = error "Using UnusedMonad"
  clearVar = error "Using UnusedMonad"
  getVarMaybe = error "Using UnusedMonad"
  withSubCtx = error "Using UnusedMonad"
  withFreshCtx = error "Using UnusedMonad"
  withVarAllocator = error "Using UnusedMonad"
  debugVars = error "Using UnusedMonads"

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

refreshVarsGen :: (MonadMatchable m, Matchable a) => (MetaVar -> m MetaVar) -> a -> m a
refreshVarsGen alloc = mapVarsM (\v -> getVarMaybe v (\(ConstMatchable v') -> return v') (refreshVar v))
  where
    refreshVar v = do v' <- alloc v
                      putVar v (ConstMatchable v')
                      return v'

refreshVarsPreserveType :: (MonadMatchable m, Matchable a) => a -> m a
refreshVarsPreserveType = refreshVarsGen allocVarOfTypeM


refreshVars :: (MonadMatchable m, Matchable a) => a -> m a
refreshVars = refreshVarsGen (const allocVarM)

fillVars :: (Matchable f, MonadMatchable m) => f -> m f
fillVars = mapVarsM (\v -> getVarMaybe v (\(ConstMatchable v') -> return v') (return v))

matchList :: (Matchable f, MonadMatchable m) => [f] -> [f] -> m ()
matchList xs1 xs2 = sequence_ $ zipWith match (map Pattern xs1) (map Matchee xs2)

fillMatchList :: (Matchable f, MonadMatchable m) => [f] -> m [f]
fillMatchList = mapM fillMatch


mapVarsMTerm :: (Monad m, Typeable l) => (MetaVar -> MatchType -> m (Term l)) -> Term l -> m (Term l)
mapVarsMTerm f (Node s ts)     = Node s <$> (mapM (mapVarsMTerm f) ts)
mapVarsMTerm f (Val  s ts)     = Val s  <$> (mapM (mapVarsMTerm f) ts)
mapVarsMTerm f (IntNode s i)   = return (IntNode s i)
mapVarsMTerm f (StrNode s x)   = return (StrNode s x)
mapVarsMTerm f (GMetaVar v mt) = f v mt
mapVarsMTerm f (GStar mt)      = return (GStar mt)



varTypeMatchCompat :: VarType -> VarType -> Bool
varTypeMatchCompat NormalVar  SymbolVar = True
varTypeMatchCompat BoundVar   BoundVar  = True
varTypeMatchCompat SymbolVar  _         = error "Symbol var found in pattern"
varTypeMatchCompat _          NormalVar = error "Normal var found in pattern"
varTypeMatchCompat BoundVar  _          = False
varTypeMatchCompat _         BoundVar   = False

instance (Typeable l) => Matchable (Term l) where
  getVars (Node _ ts)    = fold $ map getVars ts
  getVars (Val _ ts)     = fold $ map getVars ts
  getVars (IntNode _ _)  = Set.empty
  getVars (StrNode _ _)  = Set.empty
  getVars (GMetaVar v _) = Set.singleton v
  getVars (GStar _)      = Set.empty

  match (Pattern (Node s1 ts1))   (Matchee (Node s2 ts2))
    | (s1 == s2)                              = matchList ts1 ts2
  match (Pattern (Val  s1 ts1))   (Matchee (Val  s2 ts2))
    | (s1 == s2)                              = matchList ts1 ts2
  match (Pattern (IntNode s1 i1)) (Matchee (IntNode s2 i2))
    | (s1 == s2) && (i1 == i2)                = return ()
  match (Pattern (StrNode s1 x1)) (Matchee (StrNode s2 x2))
    | (s1 == s2) && (x1 == x2)                = return ()

  match (Pattern (GMetaVar v mt1))  (Matchee (GStar       mt2)) = do
      guard (getVarType v == NormalVar)
      case mt1 `meet` mt2 of
        Just mtMeet -> putVar v (GStar @l mtMeet)
        Nothing     -> mzero

  -- FIXME: I'm still not really sure if this should fail to match if mt2 > mt1, or if it should narrow v2
  match (Pattern (GMetaVar v1 mt1)) (Matchee (GMetaVar v2 mt2)) = do
      guard (varTypeMatchCompat (getVarType v1) (getVarType v2))
      guard (mt2 `prec` mt1)
      putVar v1 (GMetaVar @l v2 mt2)

  match (Pattern (GMetaVar v mt)) (Matchee t) = do guard (getVarType v == NormalVar)
                                                   guard (matchTypeForTerm t `prec` mt)
                                                   putVar v t

  match (Pattern (Node _ _))      (Matchee ValStar   ) = mzero
  match (Pattern (Node _ ts))     (Matchee (GStar mt)) = forM_ ts $ \ t -> match (Pattern t) (Matchee Star)
  match (Pattern (Val _ _))       (Matchee NonvalStar) = mzero
  match (Pattern (Val _ ts))      (Matchee (GStar mt)) = forM_ ts $ \ t -> match (Pattern t) (Matchee Star)
  match (Pattern (IntNode _ _))   (Matchee (GStar _))  = return ()
  match (Pattern (StrNode _ _))   (Matchee (GStar _))  = return ()

  -- I thought I had convinced myself that Star's couldn't appear in a pattern, but...
  match (Pattern (GStar mt))      (Matchee t)          = guard (matchTypeForTerm t `prec` mt)

  match _                         _                    = mzero


  mapVarsM f = mapVarsMTerm (\v mt -> GMetaVar <$> f v <*> pure mt)

  fillMatch = mapVarsMTerm (\v mt -> getVarMaybe v (guardValMatches mt) (return $ GMetaVar v mt))
    where
      guardValMatches :: (MonadMatchable m, Typeable l) => MatchType -> Term l -> m (Term l)
      guardValMatches mt t = guard (matchTypeForTerm t `prec` mt) >> return t


instance {-# OVERLAPPABLE #-} (Typeable (GConfiguration s l), Meetable (GConfiguration s l)) => Matchable (GConfiguration s l) where
  getVars (Conf t s) = getVars t `Set.union` getVars s

  match (Pattern (Conf t1 s1)) (Matchee (Conf t2 s2)) = do
      match (Pattern t1) (Matchee t2)
      match (Pattern s1) (Matchee s2)

  mapVarsM f (Conf t s) = Conf <$> mapVarsM f t <*> mapVarsM f s
  fillMatch  (Conf t s) = Conf <$> fillMatch  t <*> fillMatch  s

-- Hack to prevent over-eagerly expanding (Matchable (Configuration l)) constraints
data UnusedLanguage
instance {-# OVERLAPPING #-} (Matchable s, Meetable (GConfiguration s UnusedLanguage)) => Matchable (GConfiguration s UnusedLanguage) where
  getVars = error "Matching UnusedLanguage"
  match = error "Matching UnusedLanguage"
  mapVarsM = error "Matching UnusedLanguage"
  fillMatch = error "Matching UnusedLanguage"

instance Matchable EmptyState where
  getVars EmptyState = Set.empty
  match _ _ = return ()
  mapVarsM _ EmptyState = return EmptyState
  fillMatch EmptyState = return EmptyState

instance Matchable () where
  getVars () = Set.empty
  match _ _ = return ()
  mapVarsM _ () = return ()
  fillMatch () = return ()


data ConstMatchable a where
  ConstMatchable :: (Typeable a, Eq a, Show a) => a -> ConstMatchable a

deriving instance Typeable (ConstMatchable a)
deriving instance Eq (ConstMatchable a)
deriving instance Show (ConstMatchable a)

instance Meetable (ConstMatchable a) where
  meet x y = guard (x == y) >> return x
  isMinimal _ = True

instance (Typeable (ConstMatchable a)) => Matchable (ConstMatchable a) where
  getVars _ = Set.empty
  match (Pattern x) (Matchee y) = guard (x == y)
  mapVarsM _ x = return x
  fillMatch x = return x


-------------------------------------- Matching on SimpEnv ------------------------------------------

-- | Description: We implemented a restricted version of ACI
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


-- |
-- Important notes on matching maps:
--
-- If you fail to close all keys before matching, then you'll get weird behavior

forMap :: Map k a -> (k -> a -> b) -> Map k b
forMap = flip Map.mapWithKey

instance (Matchable a, Matchable b, UpperBound a, UpperBound b) => Matchable (SimpEnvMap a b) where
  getVars (SimpEnvMap m) = fold (map getVars keys) `Set.union` fold (map getVars vals)
    where
      (keys, vals) = unzip (Map.toList m)

  -- Two possible implementations of abstract matching, both correct:
  -- 1) What we do now: For key k1 in pattern and k2 in matchee, if k2 <= k1 or k1 <= k2,
  --    then join all values mapping to the k2's, match against value of k1
  -- 2) For each k1/k2 that may match, branch
  --
  -- Something something I think these produce principal unifiers in different orderings
  match (Pattern (SimpEnvMap m1)) (Matchee (SimpEnvMap m2)) = do
    m1' <- mapKeysM fillMatch m1
    sequence_ $ forMap m1' $ \k1 v1 ->
      let matching = Map.filterWithKey (\k2 _ -> (k1 `meet` k2) /= Nothing) m2 in
      case Map.assocs matching of
        []          -> mzero
        ((k,v):kvs) -> do debugM $ "Matching kvs (matched " ++ show k1 ++ "): " ++ show matching
                          match (Pattern k1) (Matchee $ foldl upperBound k (map fst kvs))
                          match (Pattern v1) (Matchee $ foldl upperBound v (map snd kvs))

  mapVarsM f (SimpEnvMap m) =                     SimpEnvMap <$> (mapKeysM (mapVarsM f) =<< mapM (mapVarsM f) m)
  fillMatch  (SimpEnvMap m) = normalizeEnvMap <$> SimpEnvMap <$> (mapKeysM fillMatch    =<< mapM fillMatch    m)


-- | Used as a hint to type inference
declareTypesEq :: (Monad m) => a -> a -> m ()
declareTypesEq _ _ = return ()

instance (Matchable a, Matchable b, Typeable a, UpperBound a, UpperBound b) => Matchable (SimpEnv a b) where
  getVars (SimpEnvRest v m) = Set.insert v (getVars m)
  getVars (JustSimpMap m) = getVars m

  match (Pattern p@(SimpEnvRest v m1)) (Matchee m@(JustSimpMap m2)) = do
    vFilled <- getVarMaybe v (return.Just) (return Nothing)
    case vFilled of
      Just (SimpEnvMap vm) -> do declareTypesEq (SimpEnvMap vm) m1
                                 p' <- fillMatch p
                                 match (Pattern p') (Matchee m)
      _ -> do
        m1' <- fillMatch m1
        let (mp1, mp2) = (getSimpEnvMap m1', getSimpEnvMap m2)
        let (restMap, innerMap) = partitionAbstractMap mp1 mp2
        debugM $ "Matching maps " ++ show mp1 ++ " and " ++ show mp2
        debugM $ "Binding var " ++ show v ++ " to " ++ show restMap
        putVar v (JustSimpMap $ SimpEnvMap restMap)
        match (Pattern m1) (Matchee (SimpEnvMap innerMap))

  match (Pattern (JustSimpMap m1)) (Matchee (JustSimpMap m2)) = match (Pattern m1) (Matchee m2)

  -- TODO: Handling cases where v1 is already assigned
  match (Pattern (SimpEnvRest v1 m1)) (Matchee (SimpEnvRest v2 m2)) = do
    let (mp1, mp2) = (getSimpEnvMap m1, getSimpEnvMap m2)
    let (restMap, innerMap) = partitionAbstractMap mp1 mp2
    putVar v1 (SimpEnvRest @a @b v2 (SimpEnvMap restMap))
    match (Pattern m1) (Matchee (SimpEnvMap innerMap))

  match (Pattern x@(JustSimpMap _)) (Matchee y@(SimpEnvRest _ _)) = mzero

  -- TODO: Do we need a case for matching SimpMap with EnvRest? (Beware infinite recursion if so)


  mapVarsM f (JustSimpMap m)   = JustSimpMap <$>         mapVarsM f m
  mapVarsM f (SimpEnvRest v m) = SimpEnvRest <$> f v <*> mapVarsM f m

  fillMatch (SimpEnvRest v m1) = do
    filledVar <- getVarMaybe v (return.Just) (return Nothing)
    m1' <- fillMatch m1

    -- In the below:
    -- Order of Map.union is important.
    -- When there is conflict, will prefer the explicitly given keys
    case filledVar of
      (Just (SimpEnvRest v' m2)) -> return $ SimpEnvRest v' $ SimpEnvMap (Map.union (getSimpEnvMap m1') (getSimpEnvMap m2))
      (Just (JustSimpMap m2))    -> return $ JustSimpMap $ normalizeEnvMap $
                                                   SimpEnvMap (Map.unionWithKey
                                                                   -- Weak updates for * nodes; strong updates for concrete
                                                                   (\k x y -> if isMinimal k then x else upperBound x y)
                                                                   (getSimpEnvMap m1')
                                                                   (getSimpEnvMap m2))
      Nothing -> return $ SimpEnvRest v m1'

  fillMatch (JustSimpMap m) = JustSimpMap <$> fillMatch m


