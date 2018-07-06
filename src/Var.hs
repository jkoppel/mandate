{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Var (
    OpenClosed(..)
  , MetaVar
  , nextVar
  ) where

import Data.IORef

import System.IO.Unsafe ( unsafePerformIO )

import Data.Hashable ( Hashable )

-------------------------------------------------------------------

data OpenClosed = Closed | Open

newtype MetaVar = MetaVar Int
  deriving ( Eq, Ord, Hashable )

instance Show MetaVar where
  show (MetaVar n) = show n


globalVarCounter :: IORef Int
globalVarCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE globalVarCounter #-}

nextVar :: IO MetaVar
nextVar = MetaVar <$> atomicModifyIORef globalVarCounter (\x -> (x+1, x))