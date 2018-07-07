{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures, GeneralizedNewtypeDeriving #-}

module Var (
    OpenClosed(..)
  , ForallOC
  , HasVars(..)
  , assumeClosedList
  , assumeClosedKeys
  , MetaVar
  , nextVar
  ) where

import Data.IORef
import Data.Map ( Map )
import qualified Data.Map as Map

import GHC.Exts ( Constraint )

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
-- SimpEnvMaps, and these could totally be implemented (at much greater expense) to not need that.
--
-- So, do I keep this as is (and let matching have a secret extra precondition), or do I add a HalfOpen state?

data OpenClosed = Closed | Open

type ForallOC (c :: * -> Constraint) t = (c (t Open), c (t Closed))

class HasVars f where
  assumeClosed :: f v -> f Closed
  asOpen       :: f v -> f Open

assumeClosedList :: (HasVars f) => [f v] -> [f Closed]
assumeClosedList = map assumeClosed

assumeClosedKeys :: (HasVars k, ForallOC Ord k) => Map (k v) b -> Map (k Closed) b
assumeClosedKeys = Map.mapKeys assumeClosed

newtype MetaVar = MetaVar Int
  deriving ( Eq, Ord, Hashable )

instance Show MetaVar where
  show (MetaVar n) = show n


globalVarCounter :: IORef Int
globalVarCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE globalVarCounter #-}

nextVar :: IO MetaVar
nextVar = MetaVar <$> atomicModifyIORef globalVarCounter (\x -> (x+1, x))