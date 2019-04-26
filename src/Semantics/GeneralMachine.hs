{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Semantics.GeneralMachine (
    GenAMRhs(..)
  , GenAMState(..)

  , GenAMState(..)
  , GenAMRule(..)
  , NamedGenAMRule(..)
  , NamedGenAMRules

  , stepGenAm
  ) where

import Control.Monad ( mzero, mplus )

import Data.Set ( Set )
import qualified Data.Set as Set

import GHC.Generics ( Generic )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Hashable ( Hashable )
import Data.Typeable ( Typeable )

import Configuration
import Debug
import Lang
import Lattice
import Matching
import Semantics.Abstraction
import Semantics.Context
import Semantics.General
import Unification


-- TODO; I think, given my generalization of everything else,
-- this "payload" approach is the wrong one, and could be simplified
data GenAMRhs payload l = GenAMLetComputation (Configuration l) (ExtComp l) (GenAMRhs payload l)
                        | GenAMRhs (payload l)
  deriving ( Eq )

instance (Lang l, Meetable (payload l)) => Meetable (GenAMRhs payload l) where
  meet (GenAMLetComputation c1 f1 r1) (GenAMLetComputation c2 f2 r2) =
    GenAMLetComputation <$> c1 `meet` c2 <*> f1 `meet` f2 <*> r1 `meet` r2
  meet (GenAMRhs p1) (GenAMRhs p2) = GenAMRhs <$> p1 `meet` p2
  meet _ _ = Nothing

instance (Lang l, Typeable payload, Matchable (payload l)) => Matchable (GenAMRhs payload l) where
  getVars (GenAMLetComputation c f r) = getVars c `Set.union` getVars f `Set.union` getVars r
  getVars (GenAMRhs p) = getVars p

  match (Pattern (GenAMLetComputation c1 f1 r1)) (Matchee (GenAMLetComputation c2 f2 r2)) =
      match (Pattern c1) (Matchee c2) >> match (Pattern f1) (Matchee f2) >> match (Pattern r1) (Matchee r2)
  match (Pattern (GenAMRhs p1)) (Matchee (GenAMRhs p2)) = match (Pattern p1) (Matchee p2)

  refreshVars (GenAMLetComputation c f p) = GenAMLetComputation <$> refreshVars c <*> refreshVars f <*> refreshVars p
  refreshVars (GenAMRhs p) = GenAMRhs <$> refreshVars p

  fillMatch (GenAMLetComputation c f p) = GenAMLetComputation <$> fillMatch c <*> fillMatch f <*> fillMatch p
  fillMatch (GenAMRhs p) = GenAMRhs <$> fillMatch p

instance (Show (Configuration l), Show (payload l), Lang l) => Show (GenAMRhs payload l) where
  showsPrec d (GenAMLetComputation conf c r) = showString "let " . showsPrec d conf .
                                               showString " = " . showsPrec d c . showString " in " .
                                               showsPrec d r
  showsPrec d (GenAMRhs x) = showsPrec d x


-------------------------------------------------------------------------

data GenAMState t l = GenAMState { genAmConf :: Configuration l
                                 , genAmK    :: Context l
                                 , genAmExtra :: t
                                 }
  deriving ( Eq, Generic )


instance (Lang l, Hashable t) => Hashable (GenAMState t l)

instance (Lang l, Meetable t) => Meetable (GenAMState t l) where
  meet (GenAMState c1 k1 e1) (GenAMState c2 k2 e2) =
    GenAMState <$> c1 `meet` c2 <*> k1 `meet` k2 <*> e1 `meet` e2

instance (Lang l, Matchable t, Show (GenAMState t l)) => Matchable (GenAMState t l) where
  getVars (GenAMState c k e) = getVars c `Set.union` getVars k `Set.union` getVars e
  match (Pattern (GenAMState c1 k1 e1)) (Matchee (GenAMState c2 k2 e2))
      = match (Pattern c1) (Matchee c2) >> match (Pattern k1) (Matchee k2) >> match (Pattern e1) (Matchee e2)
  match _ _ = mzero
  refreshVars (GenAMState c k e) = GenAMState <$> refreshVars c <*> refreshVars k <*> refreshVars e
  fillMatch   (GenAMState c k e) = GenAMState <$> fillMatch   c <*> fillMatch   k <*> fillMatch e

instance (Lang l, Show (GenAMState t l), Unifiable t) => Unifiable (GenAMState t l) where
  unify (GenAMState c1 k1 e1) (GenAMState c2 k2 e2) = unify c1 c2 >> unify k1 k2 >> unify e1 e2

instance (Show (Configuration l), Show (Context l)) => Show (GenAMState () l) where
  showsPrec d (GenAMState c k ()) = showString "<" . showsPrec d c . showString " | " .
                                    showsPrec d k . showString ">"


-------------------------------------------------------------------------

data GenAMRule t l = GenAMRule { genAmBefore :: GenAMState t l
                               , genAmAfter  :: GenAMRhs (GenAMState t) l
                               }

instance (Show (Configuration l), Lang l, Show (GenAMState t l)) => Show (GenAMRule t l) where
  showsPrec d (GenAMRule before after) = showsPrec d before . showString "  ---->  " . showsPrec d after

data NamedGenAMRule t l = NamedGenAMRule { genAmRuleName :: ByteString
                                         , getGenAmRule  :: GenAMRule t l
                                         }

type NamedGenAMRules t l = [NamedGenAMRule t l]

instance (Show (Configuration l), Lang l, Show (GenAMRule t l)) => Show (NamedGenAMRule t l) where
  showsPrec d (NamedGenAMRule nm r) = showString (BS.unpack nm) . showString ":\n" . showsPrec (d+1) r
  showList rs = showRules rs

-------------------------------------------------------------------------

instance AbstractCompFuncs (GenAMRhs p l) l where
  abstractCompFuncs abs (GenAMLetComputation c (ExtComp f args) r) = GenAMLetComputation c (ExtComp (abs f) args) r
  abstractCompFuncs _ t@(GenAMRhs _) = t


instance AbstractCompFuncs (NamedGenAMRule t l) l where
  abstractCompFuncs abs (NamedGenAMRule nm r) = NamedGenAMRule nm (abstractCompFuncs abs r)

instance AbstractCompFuncs (GenAMRule t l) l where
  abstractCompFuncs abs (GenAMRule l r) = GenAMRule l (abstractCompFuncs abs r)

instance (Irrelevance (Configuration l), Irrelevance (Context l), Irrelevance t) => Irrelevance (GenAMState t l) where
  irrelevance irr (GenAMState conf ctx t) = GenAMState (irrelevance irr conf) (irrelevance irr ctx) (irrelevance irr t)

-------------------------------------------------------------------------



runGenAmRhs :: (Lang l, Matchable (GenAMState t l)) => GenAMRhs (GenAMState t) l -> Match (GenAMState t l)
runGenAmRhs (GenAMLetComputation c f r) = do res <- runExtComp f
                                             match (Pattern c) (Matchee res)
                                             runGenAmRhs r
runGenAmRhs (GenAMRhs p) = fillMatch p


reduceFrame :: (Lang l) => GenAMState t l -> Match ()
reduceFrame (GenAMState c (KPush (KInp i _) _) phase) = do
  c' <- fillMatch c
  match (Pattern i) (Matchee c')
reduceFrame _ = return ()


useGenAmRule :: (Lang l, Show (GenAMState t l), Matchable t) => NamedGenAMRule t l -> GenAMState t l -> Match (GenAMState t l)
useGenAmRule (NamedGenAMRule nm (GenAMRule left right)) st = do
    -- NOTE: I don't really know how to do higher-order matching, but I think what I did is reasonable
    ---- The trick is the special match rule for Frame, which clears bound variables from the context.

    debugM $ "Trying rule " ++ BS.unpack nm ++ " for state " ++ show st
    match (Pattern left) (Matchee st)
    reduceFrame left
    debugM $ "LHS matched: " ++ BS.unpack nm
    ret <- runGenAmRhs right
    debugM $ "Rule succeeeded:" ++ BS.unpack nm
    return ret


stepGenAm :: (Lang l, Show (GenAMState t l), Matchable t) => NamedGenAMRules t l -> GenAMState t l -> Match (GenAMState t l)
stepGenAm allRs st = go allRs
  where
    go []     = mzero
    go (r:rs) = useGenAmRule r st `mplus` go rs