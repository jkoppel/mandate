{-# LANGUAGE DeriveGeneric, FlexibleContexts, StandaloneDeriving #-}

module Semantics.General (
    ExtComp(..)
  , runExtComp

  , showRules
  ) where

import Control.Monad ( guard )
import Data.Foldable ( fold )
import Data.List ( intersperse )

import GHC.Generics ( Generic )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Hashable ( Hashable )

import Configuration
import LangBase
import Matching
import Term


------------------------------- External / meta-level computations ---------------------------------------

data ExtComp l = ExtComp (CompFunc l) [Term l]
  deriving ( Generic )

deriving instance (LangBase l) => Eq (ExtComp l)

instance (LangBase l) => Hashable (ExtComp l)

runExtComp :: (LangBase l) => ExtComp l -> Match (Configuration l)
runExtComp (ExtComp f ts) = do ts' <- fillMatchList ts
                               runMatchEffect $ runCompFunc f ts'

instance (LangBase l) => Show (ExtComp l) where
  showsPrec d (ExtComp f ts) = showString (BS.unpack $ compFuncName f) . showsPrec (d+1) ts

instance (LangBase l) => Matchable (ExtComp l) where
  getVars (ExtComp f ts) = fold (map getVars ts)

  match (Pattern (ExtComp f1 ts1)) (Matchee (ExtComp f2 ts2)) = do
    guard (f1 == f2)
    guard (length ts1 == length ts2)
    matchList (Pattern ts1) (Matchee ts2)

  fillMatch (ExtComp f ts) = ExtComp f <$> fillMatchList ts

  refreshVars (ExtComp f ts) = ExtComp f <$> refreshVarsList ts



---------------------------------------------------------------------

showRules :: (Show a) => [a] -> ShowS
showRules rs = showString "Begin Rules:\n\n" .
               foldr (.) id (intersperse (showString "\n\n") $ map (showsPrec 0) rs) .
               showString "\nEnd Rules"