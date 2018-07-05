{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, KindSignatures, UndecidableInstances #-}

module Matching (
    OpenClosed(..)

  , MonadMatchable -- opaque
  , Matchable

  , EmptyState(..)
  ) where

import Control.Monad.State ( MonadState(..), modify )

import Data.Dynamic ( Dynamic, toDyn, fromDynamic)
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Typeable ( Typeable )

import Var

data OpenClosed = Closed | Open

newtype MatchState = MatchState { getMatchState :: Map MetaVar Dynamic }

modMatchState :: (Map MetaVar Dynamic -> Map MetaVar Dynamic) -> MatchState -> MatchState
modMatchState f = MatchState . f . getMatchState

class (Monad m) => MonadMatchable m where
  putVar :: Typeable a => MetaVar -> a -> m ()
  getVar :: Typeable a => MetaVar -> m (Maybe a)
  withFreshCtx :: m a -> m a

instance (MonadState MatchState m, Monad m) => MonadMatchable m where
  putVar var val = modify (modMatchState $ Map.insert var (toDyn val))
  getVar var     = maybe Nothing fromDynamic <$> Map.lookup var <$> getMatchState <$> get

  withFreshCtx m = do old <- get
                      put $ MatchState Map.empty
                      res <- m
                      put old
                      return res


class Matchable f where
  match :: (MonadMatchable m) => f Open -> f Closed -> m ()
  fillMatch :: (MonadMatchable m)  => f Open -> m (Maybe (f Closed))


-- TODO: EmptyState is in a bad place
data EmptyState (v :: OpenClosed) = EmptyState

instance Matchable EmptyState where
  match _ _ = return ()
  fillMatch EmptyState = return (Just EmptyState)

-- Matchable instances for Term, Context, Map