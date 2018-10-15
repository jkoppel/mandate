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

-- TODO: There is a flaw in this design
-- Initially designed (after not very much thought) that this would just be for terms,
-- which may be open or closed. Should have realized immediately (though it didn't take long)
-- that also configurations need pattern variables or to be closed.
--
-- Then, the question: when do I need open vs. closed, and is there anything else?
--
-- I'm still not sure I would have realized till implementation-time the idea that there may be
-- terms which must be filled in prior to being matched. The only example currently is
-- SimpEnvMaps (e.g.: to fill in var("y"), must specialize the pattern "Gamma, x=v" to "Gamma, y=v"),
-- and these could totally be implemented (at much greater expense) to not need that.
--
-- So, do I keep this as is (and let matching have a secret extra precondition), or do I add a HalfOpen state?

-- Also, another problem: Things with binders can be closed, and yet still contain mvs

--data OpenClosed = Closed | Open

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
