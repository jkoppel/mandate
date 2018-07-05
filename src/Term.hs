{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, DeriveGeneric, EmptyDataDecls, GADTs, PatternSynonyms, StandaloneDeriving, TypeFamilies #-}

module Term (
  Sort(..)
, Symbol(..)
, SigNode(..)
, Signature(..)

, Term
, pattern Node
, pattern IntNode
, pattern MetaVar

) where

import Data.Function ( on )
import Data.List ( intersperse )
import Data.String ( IsString(..) )

import GHC.Generics ( Generic )

import Unsafe.Coerce ( unsafeCoerce )

import qualified Data.ByteString.Char8 as BS
import Data.Hashable ( Hashable(..) )
import Data.Interned ( Interned(..), intern, unintern, Id, Cache, mkCache )
import Data.Interned.ByteString ( InternedByteString(..) )

import Matching
import Var

-----------------------------------------------------------------------------------------------------------

-- Beware: Lists are not strict

data Sort = Sort {-# UNPACK #-} !InternedByteString
  deriving ( Eq, Ord, Generic )

instance IsString Sort where
  fromString = Sort . fromString

instance Show Sort where
  showsPrec d (Sort s) = showString (BS.unpack $ unintern s)

-- I think this does have hidden coupling with Sort; they're likely to keep
-- mirror implementations. But, they're small
data Symbol = Symbol {-# UNPACK #-} !InternedByteString
  deriving ( Eq, Ord, Generic )

instance IsString Symbol where
  fromString = Symbol . fromString

instance Show Symbol where
  showsPrec d (Symbol s) = showString (BS.unpack $ unintern s)

instance Hashable Symbol where
  hashWithSalt s (Symbol ibs) = s `hashWithSalt` (internedByteStringId ibs)

data SigNode = NodeSig !Symbol [Sort] Sort
             | IntSig !Symbol Sort
  deriving ( Eq, Ord, Show, Generic )

data Signature a = Signature [SigNode]
  deriving ( Eq, Ord, Show, Generic )

-- Need a single Interned instance for all terms. This implies
-- that they must share a cache.
-- This is a special private token used to make things share a cache
-- Has the side effect that can have cache collisions
-- between the same term in different languages
data AnyLanguage

-- TODO: Consider hiding ids/constructors, using pattern syns
data Term a v where
    TNode    :: !Id -> !Symbol  -> [Term a v] -> Term a v
    TIntNode :: !Id -> !Symbol -> Integer -> Term a v
    TMetaVar :: !Id -> !MetaVar -> Term a Open

instance Show (Term a v) where
  showsPrec d (TNode _ s ts) = showsPrec (d+1) s . showList ts
  showsPrec d (TIntNode _ s n) = showsPrec (d+1) s . showString "(" . showsPrec (d+1) n . showString ")"
  showsPrec d (TMetaVar _ v) = showsPrec d v

  showList ts = showString "(" . foldr (.) id (intersperse (showString ", ") (map (showsPrec 0) ts)) . showString ")"

getId :: Term a v -> Id
getId (TNode    i _ _) = i
getId (TIntNode i _ _) = i
getId (TMetaVar i _)   = i

type GenericTerm = Term AnyLanguage Open

-- I tried to get safe coercions working, but couldn't
-- Did not find good tutorials. Maybe it only works with newtypes ATM?
toGeneric :: Term a v -> GenericTerm
toGeneric = unsafeCoerce

fromGeneric :: GenericTerm -> Term a v
fromGeneric = unsafeCoerce


data UninternedTerm v a where
 BNode :: Symbol -> [Term a v] -> UninternedTerm a v
 BIntNode :: Symbol -> Integer -> UninternedTerm a v
 BMetaVar :: MetaVar -> UninternedTerm a Open

type GenericUninternedTerm = UninternedTerm AnyLanguage Open


toUGeneric :: UninternedTerm a v -> GenericUninternedTerm
toUGeneric = unsafeCoerce

fromUGeneric :: GenericUninternedTerm -> UninternedTerm a v
fromUGeneric = unsafeCoerce

instance Interned GenericTerm where
  type Uninterned GenericTerm = GenericUninternedTerm
  data Description GenericTerm = DNode Symbol [Id]
                               | DIntNode Symbol Integer
                               | DMetaVar MetaVar
    deriving ( Eq, Ord, Generic )

  describe (BNode s ts)    = DNode s (map getId ts)
  describe (BIntNode s n)  = DIntNode s n
  describe (BMetaVar m)    = DMetaVar m

  identify i = go where
    go (BNode s ts)   = TNode i s ts
    go (BIntNode s n) = TIntNode i s n
    go (BMetaVar m)   = TMetaVar i m

  cache = termCache

instance Hashable (Description GenericTerm)


instance Eq (Term a v) where
  (==) = (==) `on` getId

instance Ord (Term a v) where
  compare = compare `on` getId

termCache :: Cache GenericTerm
termCache = mkCache
{-# NOINLINE termCache #-}


pattern Node :: Symbol -> [Term a v] -> Term a v
pattern Node s ts <- (TNode _ s ts) where
  Node s ts = fromGeneric $ intern $ toUGeneric (BNode s ts)

pattern IntNode :: Symbol -> Integer -> Term a v
pattern IntNode s n <- (TIntNode _ s n) where
  IntNode s n = fromGeneric $ intern $ toUGeneric (BIntNode s n)

pattern MetaVar :: MetaVar -> Term a Open
pattern MetaVar v <- (TMetaVar _ v) where
  MetaVar v = fromGeneric $ intern $ toUGeneric (BMetaVar v)