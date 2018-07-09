{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications #-}

module Var (
    OpenClosed(..)
  , CheckOpenClosed(..)
  , testOpenClosed
  , ForallOC
  , HasVars(..)
  , assumeClosedList
  , asOpenList
  , listAsOC
  , assumeClosedKeys
  , asOpenKeys
  , keysAsOC
  , MetaVar
  , nextVar
  ) where

import Data.IORef
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Typeable (Typeable, eqT, (:~:)(..))

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

-- Also, another problem: Things with binders can be closed, and yet still contain mvs

data OpenClosed = Closed | Open

data CheckOpenClosed v where
  IsClosed :: CheckOpenClosed Closed
  IsOpen   :: CheckOpenClosed Open

testOpenClosed :: forall v. (Typeable v) => CheckOpenClosed v
testOpenClosed = case eqT @v @Closed of
                   Just Refl -> IsClosed
                   Nothing   -> case eqT @v @Open of
                                  Just Refl -> IsOpen
                                  Nothing   -> error "OpenClosed variable is not Open or Closed"

-- TODO: I can't wait for quantified constraints to come out. Currently, GHC cannot resolve "Ord v" if v is unknown,
-- but ForallOC Ord v holds.
type ForallOC (c :: * -> Constraint) t = (c (t Open), c (t Closed))


class HasVars f where
  assumeClosed :: f v -> f Closed

  -- Must be total and O(1)
  asOpen       :: f v -> f Open

assumeClosedList :: (HasVars f) => [f v] -> [f Closed]
assumeClosedList = map assumeClosed

asOpenList :: (HasVars f) => [f v] -> [f Open]
asOpenList = map asOpen

listAsOC :: forall v f v'. (HasVars f, Typeable v) => [f v'] -> [f v]
listAsOC l = case testOpenClosed @v of
               IsClosed -> assumeClosedList l
               IsOpen   -> asOpenList l

assumeClosedKeys :: (HasVars k, ForallOC Ord k) => Map (k v) b -> Map (k Closed) b
assumeClosedKeys = Map.mapKeys assumeClosed

asOpenKeys :: (HasVars k, ForallOC Ord k) => Map (k v) b -> Map (k Open) b
asOpenKeys = Map.mapKeys asOpen

keysAsOC :: forall v f v' b. (HasVars f, ForallOC Ord f, Typeable v) => Map (f v') b -> Map (f v) b
keysAsOC l = case testOpenClosed @v of
               IsClosed -> assumeClosedKeys l
               IsOpen   -> asOpenKeys l

newtype MetaVar = MetaVar Int
  deriving ( Eq, Ord, Hashable )

instance Show MetaVar where
  show (MetaVar n) = show n


globalVarCounter :: IORef Int
globalVarCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE globalVarCounter #-}

nextVar :: IO MetaVar
nextVar = MetaVar <$> atomicModifyIORef globalVarCounter (\x -> (x+1, x))