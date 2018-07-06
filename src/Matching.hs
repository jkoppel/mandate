{-# LANGUAGE DataKinds, EmptyDataDecls, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Matching (
    MonadMatchable(..)
  , getVars
  , Match
  , runMatch

  , Matchable(..)

  , EmptyState(..)
  ) where

import Control.Monad ( MonadPlus(..) )
import Control.Monad.State ( MonadState(..), StateT, evalStateT, modify )
import Control.Monad.Trans ( lift )

import Data.Dynamic ( Dynamic, toDyn, fromDynamic)
import Data.Map ( Map )
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
  putVar :: Typeable a => MetaVar -> a -> m ()
  getVar :: Typeable a => MetaVar -> m a
  withFreshCtx :: m a -> m a

instance (MonadState MatchState m, MonadPlus m) => MonadMatchable m where
  putVar var val = modify (modMatchState $ Map.insert var (toDyn val))
  getVar var     = do maybeDyn <- Map.lookup var <$> getMatchState <$> get
                      case (maybeDyn :: Maybe Dynamic) of
                        Nothing -> mzero
                        Just x  -> maybe mzero return (fromDynamic x)

  withFreshCtx m = do old <- get
                      put $ MatchState Map.empty
                      res <- m
                      put old
                      return res


getVars :: (MonadMatchable m, Typeable (Term l)) => [MetaVar] -> m [Term l Closed]
getVars = mapM getVar

class Matchable f where
  match :: (MonadMatchable m) => f Open -> f Closed -> m ()
  fillMatch :: (MonadMatchable m)  => f Open -> m (f Closed)

instance Matchable EmptyState where
  match _ _ = return ()
  fillMatch EmptyState = return EmptyState

instance (Typeable (Term l)) => Matchable (Term l) where
  match (Node s1 ts1)   (Node s2 ts2)
    | (s1 == s2)                        = sequence_ $ zipWith match ts1 ts2
  match (IntNode s1 i1) (IntNode s2 i2)
    | (s1 == s2) && (i1 == i2)          = return ()
  match (MetaVar v)     t               = putVar v t
  match _               _               = mzero

  fillMatch (Node s ts)   = Node s <$> (mapM fillMatch ts)
  fillMatch (IntNode s i) = return (IntNode s i) -- This could just be unsafeCoerce....or hopefully just coerce
  fillMatch (MetaVar v)   = getVar v


instance {-# OVERLAPPABLE #-} (Matchable (Term l), Matchable s) => Matchable (GConfiguration l s) where
  match (Conf t1 s1) (Conf t2 s2) = match t1 t2 >> match s1 s2
  fillMatch (Conf t s) = Conf <$> fillMatch t <*> fillMatch s

-- Hack to prevent over-eagerly expanding (Matchable (Configuration l)) constraints
data UnusedLanguage
instance {-# OVERLAPPING #-} Matchable (GConfiguration UnusedLanguage s) where
  match = error "Matching UnusedLanguage"
  fillMatch = error "Matching UnusedLanguage"

-- Matchable instances for Term, Context, Map