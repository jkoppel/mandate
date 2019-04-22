{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Var (
    MetaVar
  , nextVar

  , VarAllocator(..)
  , mkPositiveVarAllocator
  , mkNegativeVarAllocator

  , MonadVarAllocator(..)
  ) where

import Control.Monad.State ( MonadState, state )

import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )

import Data.Hashable ( Hashable )

-------------------------------------------------------------------

newtype MetaVar = MetaVar Int
  deriving ( Eq, Ord, Hashable )

instance Show MetaVar where
  show (MetaVar n) = show n


globalVarCounter :: IORef Int
globalVarCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE globalVarCounter #-}

nextVar :: IO MetaVar
nextVar = MetaVar <$> atomicModifyIORef globalVarCounter (\x -> (x+1, x))


data VarAllocator = VarAllocator { allocVar :: (MetaVar, VarAllocator) }


mkIncVarAllocator :: Int -> Int -> VarAllocator
mkIncVarAllocator start inc = genVarAllocator start
  where
    genVarAllocator :: Int -> VarAllocator
    genVarAllocator x = VarAllocator { allocVar = (MetaVar x, genVarAllocator (x + inc)) }

mkPositiveVarAllocator :: VarAllocator
mkPositiveVarAllocator = mkIncVarAllocator 1 1

mkNegativeVarAllocator :: VarAllocator
mkNegativeVarAllocator = mkIncVarAllocator (-1) (-1)


class MonadVarAllocator m where
  allocVarM :: m MetaVar

-- Matches too eagerly; removing this instance
-- instance (MonadState VarAllocator m) => MonadVarAllocator m where
--  allocVarM = state allocVar
