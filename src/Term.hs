{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, DeriveGeneric, EmptyDataDecls, GADTs, StandaloneDeriving, TypeFamilies #-}

module Term (
  Sort(..)
, Symbol(..)
, SigNode(..)
, Signature(..)
, Term(..)
, node
, metaVar
, MetaVar
,
, Lang(..)
) where

import Data.Function ( on )
import Data.Hashable ( Hashable(..) )
import Data.Interned ( Interned(..), intern, Id, Cache, mkCache )
import Data.Interned.ByteString ( InternedByteString(..) )

import GHC.Generics ( Generic )

import Unsafe.Coerce ( unsafeCoerce )

-- TODO: Add hash-consing

-- Beware: Lists are not strict

data Sort = Sort {-# UNPACK #-} !InternedByteString
  deriving ( Eq, Ord, Show, Generic )

data Symbol = Symbol {-# UNPACK #-} !InternedByteString
  deriving ( Eq, Ord, Show, Generic )

instance Hashable Symbol where
  hashWithSalt s (Symbol ibs) = s `hashWithSalt` (internedByteStringId ibs)

data SigNode = SigNode !Symbol [Sort]
  deriving ( Eq, Ord, Show, Generic )

-- Probably want to make Signature internable, but not its constituents
data Signature a = Signature [SigNode]
  deriving ( Eq, Ord, Show, Generic )

type MetaVar = Int

-- Need a single Interned instance for all terms. This implies
-- that they must share a cache.
-- This is a special private token used to make things share a cache
-- Has the side effect that can have cache collisions
-- between the same term in different languages
data AnyLanguage

data VarSet = Closed | Open

-- TODO: Consider hiding ids/constructors, using pattern syns
data Term v a where
    Node    :: !Id -> !Symbol  -> [Term v a] -> Term v a
    MetaVar :: !Id -> !MetaVar -> Term Open a

deriving instance Show (Term v a)

getId :: Term v a -> Id
getId (Node    i _ _)  = i
getId (MetaVar i _)    = i

type GenericTerm = Term Open AnyLanguage

toGeneric :: Term v a -> GenericTerm
toGeneric = unsafeCoerce

fromGeneric :: GenericTerm -> Term v a
fromGeneric = unsafeCoerce


data UninternedTerm v a where
 BNode :: Symbol -> [Term v a] -> UninternedTerm v a
 BMetaVar :: MetaVar -> UninternedTerm Open a

type GenericUninternedTerm = UninternedTerm Open AnyLanguage

toUGeneric :: UninternedTerm v a -> GenericUninternedTerm
toUGeneric = unsafeCoerce

fromUGeneric :: GenericUninternedTerm -> UninternedTerm v a
fromUGeneric = unsafeCoerce

instance Interned GenericTerm where
  type Uninterned GenericTerm = GenericUninternedTerm
  data Description GenericTerm = DNode Symbol [Id]
                               | DMetaVar MetaVar
    deriving ( Eq, Ord, Generic )

  describe (BNode s ts) = DNode s (map getId ts)
  describe (BMetaVar m) = DMetaVar m

  identify i = go where
    go (BNode s ts) = Node i s ts
    go (BMetaVar m) = MetaVar i m

  cache = termCache

instance Hashable (Description GenericTerm)


instance Eq (Term v a) where
  (==) = (==) `on` getId

instance Ord (Term v a) where
  compare = compare `on` getId


termCache :: Cache GenericTerm
termCache = mkCache
{-# NOINLINE termCache #-}

node :: Symbol -> [Term v a] -> Term v a
node s ts = fromGeneric $ intern $ toUGeneric (BNode s ts)

metaVar :: MetaVar -> Term Open a
metaVar v = fromGeneric $ intern $ toUGeneric (BMetaVar v)


-- Rules

class Lang l where
  type RedState l
  -- LEFT OFF HERE

-- Context types
-- Context values
