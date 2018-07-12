{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, UndecidableInstances #-}


module Semantics.AbstractMachine (

  ) where

import Control.Monad ( MonadPlus(..), liftM )
import Data.Maybe ( fromJust, listToMaybe )
import Data.Monoid ( Monoid(..), )
import Data.Set ( Set )
import qualified Data.Set as Set

import GHC.Generics ( Generic )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Hashable ( Hashable )

import Configuration
import Debug
import Graph
import Lang
import Matching
import Rose
import Semantics.Abstraction
import Semantics.Context
import Semantics.General
import Semantics.GeneralMachine
import Semantics.SOS
import Term
import TransitionSystem
import Var

-- TODO: Define PAMState by delegating to AMState....or the Haskell version of such

data AMState l = AMState { amConf  :: Configuration l
                         , amK     :: Context l
                         }
  deriving ( Eq, Generic )

instance (Lang l) => Hashable (AMState l)

instance (Lang l) => Matchable (AMState l) where
  getVars (AMState c k) = getVars c `Set.union` getVars k
  match (Pattern (AMState c1 k1)) (Matchee (AMState c2 k2)) = match (Pattern c1) (Matchee c2) >> match (Pattern k1) (Matchee k2)
  match _ _ = mzero
  refreshVars (AMState c k) = AMState <$> refreshVars c <*> refreshVars k
  fillMatch   (AMState c k) = AMState <$> fillMatch   c <*> fillMatch   k

instance (Show (Configuration l), Show (Context l)) => Show (AMState l) where
  showsPrec d (AMState c k) = showString "<" . showsPrec d c . showString " | " .
                              showsPrec d k . showString "> "

type AMRhs = GenAMRhs AMState

data AMRule l = AM { amBefore :: AMState l
                   , amAfter  :: AMRhs l
                   }

instance (Show (Configuration l), LangBase l) => Show (AMRule l) where
  showsPrec d (AM before after) = showsPrec d before . showString "  ---->  " . showsPrec d after

data NamedAMRule l = NamedAMRule { amRuleName :: ByteString
                                 , getAMRule  :: AMRule l
                                 }

instance (Show (Configuration l), LangBase l) => Show (NamedAMRule l) where
  showsPrec d (NamedAMRule nm r) = showString (BS.unpack nm) . showString ":\n" . showsPrec (d+1) r
  showList rs = showRules rs
