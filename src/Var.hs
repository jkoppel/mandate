{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Var (
    VarType(..)

  , MetaVar
  , nextVar

  , changeVarType
  , getVarType

  , varToSymbol
  , varToSymbolLax
  , symbolToVar
  , symbolToVarLax
  , boundifyVar
  , unboundifyVar

  , VarAllocator(..)
  , mkNormalVarAllocator
  , mkBoundVarAllocator

  , MonadVarAllocator(..)
  ) where

import Control.Monad.State ( MonadState, state )
import Data.IORef
import GHC.Generics ( Generic )
import System.IO.Unsafe ( unsafePerformIO )

import Data.Hashable ( Hashable )

-------------------------------------------------------------------


-- A flaw with this design is that map variables (associative-commutative variables)
-- are given the type "normal var." Context variables and term variables are also treated identically.
--
-- Perhaps variables should really be objects, but I don't know what the interface would be.

-- NormalVar's are existential. SymbolVar's are universal.  Two NormalVar's unify
-- if there is a common substitution that makes both equal. A NormalVar x unifies with
-- SymbolVar y only if x can be set to y, so that they will be equal under all substitutions
-- to y.
data VarType = NormalVar | SymbolVar | BoundVar
  deriving ( Eq, Ord, Show, Generic )

instance Hashable VarType

newtype VarId = VarId Int
  deriving ( Eq, Ord, Hashable )


-- | There's a distinction not captured by the type-system: Things containing variables
-- should either have only Normal/Bound vars, or only symbols/Bound vars
--
-- There is still some question of whether this is better done by adding a new constructor to Term.
-- Arguments both ways; both variants are isomorphic, and doing it this way weakens the abstraction barrier around
-- MetaVar. However, treatment of vars/symbols should be identical outside of pattern matching (right?), and
-- there already was stronger hiding around MetaVar than Term, making it easier to do things this way
data MetaVar = MetaVar VarType VarId
  deriving ( Eq, Ord, Generic )

instance Show MetaVar where
  show (MetaVar NormalVar (VarId n)) = show n
  show (MetaVar BoundVar  (VarId n)) = "'" ++ show n
  show (MetaVar SymbolVar (VarId n)) = "sym(" ++ show n ++ ")"

instance Hashable MetaVar

changeVarType :: (VarType -> VarType) -> (MetaVar -> MetaVar)
changeVarType f (MetaVar t x) = MetaVar (f t) x

getVarType :: MetaVar -> VarType
getVarType (MetaVar t _) = t

-- Errors when symbolizing a symbol
varToSymbol :: MetaVar -> MetaVar
varToSymbol (MetaVar NormalVar x) = MetaVar SymbolVar  x
varToSymbol (MetaVar BoundVar x)  = MetaVar BoundVar x
varToSymbol (MetaVar SymbolVar x) = error "Coercing symbol to symbol"

-- Doesn't error when symbolizing a symbol
varToSymbolLax :: MetaVar -> MetaVar
varToSymbolLax (MetaVar NormalVar x) = MetaVar SymbolVar  x
varToSymbolLax (MetaVar BoundVar x)  = MetaVar BoundVar x
varToSymbolLax x                     = x

symbolToVar :: MetaVar -> MetaVar
symbolToVar (MetaVar NormalVar x) = error "Coercing normal var to normal var"
symbolToVar (MetaVar BoundVar x)  = MetaVar BoundVar x
symbolToVar (MetaVar SymbolVar x) = MetaVar NormalVar x

symbolToVarLax :: MetaVar -> MetaVar
symbolToVarLax (MetaVar BoundVar x)  = MetaVar BoundVar x
symbolToVarLax (MetaVar SymbolVar x) = MetaVar NormalVar x
symbolToVarLax x                     = x

boundifyVar :: MetaVar -> MetaVar
boundifyVar (MetaVar NormalVar (VarId n)) = MetaVar BoundVar (VarId n)
boundifyVar x                             = error ("Attempting to boundify non-normal var " ++ show x)

unboundifyVar :: MetaVar -> MetaVar
unboundifyVar (MetaVar BoundVar (VarId n)) = MetaVar NormalVar (VarId n)
unboundifyVar x                            = error ("Attempting to unboundify non-bound var " ++ show x)




globalVarCounter :: IORef Int
globalVarCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE globalVarCounter #-}

nextVar :: IO MetaVar
nextVar = MetaVar NormalVar <$> atomicModifyIORef globalVarCounter (\x -> (x+1, VarId x))


data VarAllocator = VarAllocator { allocVar :: (MetaVar, VarAllocator) }

mkIncVarAllocator :: VarType -> Int -> Int -> VarAllocator
mkIncVarAllocator tp start inc = genVarAllocator start
  where
    genVarAllocator :: Int -> VarAllocator
    genVarAllocator x = VarAllocator { allocVar = ( MetaVar tp (VarId x)
                                                  , genVarAllocator (x + inc)
                                                  )

                                     }

mkNormalVarAllocator :: VarAllocator
mkNormalVarAllocator = mkIncVarAllocator NormalVar 1 1

mkBoundVarAllocator :: VarAllocator
mkBoundVarAllocator = mkIncVarAllocator BoundVar 1 1


class (Monad m) => MonadVarAllocator m where
  allocVarM :: m MetaVar

  allocVarOfTypeM :: MetaVar -> m MetaVar
  allocVarOfTypeM (MetaVar t _) = do
    MetaVar _ v <- allocVarM
    return (MetaVar t v)


-- Matches too eagerly; removing this instance
-- instance (MonadState VarAllocator m) => MonadVarAllocator m where
--  allocVarM = state allocVar
