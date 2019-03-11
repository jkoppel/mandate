{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}

module Languages.Translation (
    ToGeneric(..)
  , FromGeneric(..)

  , ibsToString

  , checkRoundTrip
  , checkRoundTrip'
  ) where

import Data.Maybe ( fromJust )
import Data.String ( fromString )

import qualified Data.ByteString.Char8 as BS
import Data.Interned ( unintern )
import Data.Interned.ByteString ( InternedByteString )

import Term

---------------------------------------------------------------------------------------------------------

class ToGeneric l a where
  toGeneric :: a -> Term l

class FromGeneric l a where
  fromGeneric :: Term l -> Maybe a

ibsToString :: InternedByteString -> String
ibsToString = BS.unpack . unintern


--------------------------------

-- TODO: Make an actual tests dir, and set this up with QuickCheck. And don't forget to run checkTerm on the output
checkRoundTrip :: forall l a. (ToGeneric l a, FromGeneric l a, Eq a, Show a) => a -> ()
checkRoundTrip b = if fromJust (fromGeneric (toGeneric @l b)) == b then () else error ("Failed checkRoundTrip: " ++ show b)

checkRoundTrip' :: forall l a. (ToGeneric l a, FromGeneric l a, Eq a, Show a) => a -> ()
checkRoundTrip' b = if toGeneric (fromJust (fromGeneric @l @a (toGeneric @l b))) == (toGeneric @l b) then () else error ("Failed checkRoundTrip': " ++ show b)