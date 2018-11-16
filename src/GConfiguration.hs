{-# LANGUAGE StandaloneDeriving, GADTs, FlexibleContexts #-}


module GConfiguration (
  GConfiguration(..)
  , confTerm
  , confState
  ) where

import Data.ByteString.Char8 ( ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

import Data.Hashable ( Hashable(..) )

import Term

data GConfiguration s l where
  Conf :: Typeable s => Term l -> s -> GConfiguration s l

deriving instance (Eq (Term l), Eq s) => Eq (GConfiguration s l)
deriving instance (Ord (Term l), Ord s) => Ord (GConfiguration s l)

instance (Hashable s) => Hashable (GConfiguration s l) where
  hashWithSalt s (Conf t st) = s `hashWithSalt` t `hashWithSalt` s

confTerm :: GConfiguration s l -> Term l
confTerm (Conf t _) = t

confState :: GConfiguration s l -> s
confState (Conf _ s) = s

-- | The configuration is the object of the transition system defined by
-- a language's semantics. It contains a term together with the extra information
-- (i.e.: environment) computed and transformed when running a program.