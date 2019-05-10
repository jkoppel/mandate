{-# LANGUAGE DeriveGeneric, FlexibleContexts, StandaloneDeriving #-}

module Semantics.General (
    ExtComp(..)
  , runExtComp
  , extComp
  , showRules
  ) where

import Control.Monad ( guard )
import Data.Foldable ( fold )
import Data.List ( intersperse )
import Data.Set ( union )

import GHC.Generics ( Generic )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Hashable ( Hashable )

import Configuration
import Lang
import Lattice
import Matching
import Term
import Unification


------------------------------- External / meta-level computations ---------------------------------------

data ExtComp l = ExtComp (CompFunc l) [Configuration l]
  deriving ( Generic )

deriving instance (Lang l) => Eq (ExtComp l)

instance (Lang l) => Hashable (ExtComp l)

runExtComp :: (Lang l) => ExtComp l -> Match (Configuration l)
runExtComp (ExtComp f ts) = do ts' <- fillMatchList ts
                               runMatchEffect $ runCompFunc f ts'

extComp :: (Lang l, Hashable (RedState l), Unifiable (RedState l)) => CompFunc l -> RedState l -> [Term l] -> ExtComp l
extComp func state terms = ExtComp func $ map (\term -> Conf term state) terms

instance (Lang l) => Show (ExtComp l) where
  showsPrec d (ExtComp f ts) = showString (BS.unpack $ compFuncName f) . showsPrec (d+1) ts

instance (Lang l) => Meetable (ExtComp l) where
  meet (ExtComp f1 ts1) (ExtComp f2 ts2) =
    ExtComp <$> (guard (f1 == f2) >> return f1) <*> mapM (uncurry meet) (zip ts1 ts2)

instance (Lang l) => Matchable (ExtComp l) where
  getVars (ExtComp f ts) = fold (map getVars ts)

  match (Pattern (ExtComp f1 ts1)) (Matchee (ExtComp f2 ts2)) = do
    guard (f1 == f2)
    guard (length ts1 == length ts2)
    matchList (Pattern ts1) (Matchee ts2)

  fillMatch (ExtComp f ts) = ExtComp f <$> fillMatchList ts

  refreshVars (ExtComp f ts) = ExtComp f <$> refreshVarsList ts

instance (Matchable a, Matchable b) => Matchable (a, b) where
  getVars (a,b) = getVars a `union` getVars b

  match (Pattern (a,b)) (Matchee (a',b')) = do
    match (Pattern a) (Matchee a')
    match (Pattern b) (Matchee b')

  fillMatch (a, b) = (,) <$> fillMatch a <*> fillMatch b

  refreshVars (a, b) = (,) <$> refreshVars a <*> refreshVars b


---------------------------------------------------------------------

showRules :: (Show a) => [a] -> ShowS
showRules rs = showString "Begin Rules:\n\n" .
               foldr (.) id (intersperse (showString "\n\n") $ map (showsPrec 0) rs) .
               showString "\nEnd Rules"