{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE DataKinds, DeriveGeneric, EmptyDataDecls, GADTs, PatternSynonyms, StandaloneDeriving, TypeFamilies #-}

module Term (
  Sort(..)
, Symbol(..)
, SigNode(..)
, sigNodeSymbol
, sigNodeSort
, Signature(..)

, Term
, pattern Node
, pattern IntNode
, pattern StrNode
, pattern MetaVar

, checkTerm
) where

import Control.DeepSeq ( deepseq )

import Data.Function ( on )
import Data.List ( intersperse, find )
import Data.String ( IsString(..) )

import GHC.Generics ( Generic )

import Unsafe.Coerce ( unsafeCoerce )

import qualified Data.ByteString.Char8 as BS
import Data.Hashable ( Hashable(..) )
import Data.Interned ( Interned(..), intern, unintern, Id, Cache, mkCache )
import Data.Interned.ByteString ( InternedByteString(..) )

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
             | StrSig !Symbol Sort
  deriving ( Eq, Ord, Show, Generic )

sigNodeSymbol :: SigNode -> Symbol
sigNodeSymbol (NodeSig s _ _) = s
sigNodeSymbol (IntSig s _)    = s
sigNodeSymbol (StrSig s _)    = s

sigNodeSort :: SigNode -> Sort
sigNodeSort (NodeSig _ _ s) = s
sigNodeSort (IntSig _ s)    = s
sigNodeSort (StrSig _ s)    = s

data Signature a = Signature [SigNode]
  deriving ( Eq, Ord, Show, Generic )

-- Need a single Interned instance for all terms. This implies
-- that they must share a cache.
-- This is a special private token used to make things share a cache
-- Has the side effect that can have cache collisions
-- between the same term in different languages
data AnyLanguage

data Term a v where
    TNode    :: !Id -> !Symbol  -> [Term a v] -> Term a v
    TIntNode :: !Id -> !Symbol -> !Integer -> Term a v
    TStrNode :: !Id -> !Symbol -> !InternedByteString -> Term a v
    TMetaVar :: !Id -> !MetaVar -> Term a Open

instance Show (Term a v) where
  showsPrec d (TNode _ s ts) = showsPrec (d+1) s . showList ts
  showsPrec d (TIntNode _ s n) = showsPrec (d+1) s . showString "(" . showsPrec (d+1) n . showString ")"
  showsPrec d (TStrNode _ s str) = showsPrec (d+1) s . showString "(" . showsPrec (d+1) str . showString ")"
  showsPrec d (TMetaVar _ v) = showsPrec d v

  showList ts = showString "(" . foldr (.) id (intersperse (showString ", ") (map (showsPrec 0) ts)) . showString ")"

getId :: Term a v -> Id
getId (TNode    i _ _) = i
getId (TIntNode i _ _) = i
getId (TStrNode i _ _) = i
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
 BStrNode :: Symbol -> InternedByteString -> UninternedTerm a v
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
                               | DStrNode Symbol Id
                               | DMetaVar MetaVar
    deriving ( Eq, Ord, Generic )

  describe (BNode s ts)     = DNode s (map getId ts)
  describe (BIntNode s n)   = DIntNode s n
  describe (BStrNode s str) = DStrNode s (internedByteStringId str)
  describe (BMetaVar m)     = DMetaVar m

  identify i = go where
    go (BNode s ts)     = TNode i s ts
    go (BIntNode s n)   = TIntNode i s n
    go (BStrNode s str) = TStrNode i s str
    go (BMetaVar m)    = TMetaVar i m

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

pattern StrNode :: Symbol -> InternedByteString -> Term a v
pattern StrNode s str <- (TStrNode _ s str) where
  StrNode s str = fromGeneric $ intern $ toUGeneric (BStrNode s str)

pattern MetaVar :: () => (v ~ Open) => MetaVar -> Term a v
pattern MetaVar v <- (TMetaVar _ v) where
  MetaVar v = fromGeneric $ intern $ toUGeneric (BMetaVar v)


instance HasVars (Term l) where
  assumeClosed (Node s ts)     = Node s (map assumeClosed ts)
  assumeClosed (IntNode s i)   = IntNode s i -- Could save epsilon time using unsafeCoerce
  assumeClosed (StrNode s str) = StrNode s str
  assumeClosed (MetaVar v)     = error ("Assuming term closed, but has var " ++ show v)

------------------------------------------------------------------------------------

-- If want the language in error messages, can add Typeable constraints and show the TypeRep
getInSig :: Signature l -> Symbol -> SigNode
getInSig (Signature sig) s = case find (\n -> sigNodeSymbol n == s) sig of
                           Just n -> n
                           Nothing -> error ("Cannot find symbol " ++ show s ++ " in signature " ++ show sig)

sortForSym :: Signature l -> Symbol -> Sort
sortForSym sig s = sigNodeSort $ getInSig sig s

-- Metavars are currently unsorted / can have any sort
sortOfTerm :: Signature l -> Term l v -> Maybe Sort
sortOfTerm sig (Node    sym _) = Just $ sortForSym sig sym
sortOfTerm sig (IntNode sym _) = Just $ sortForSym sig sym
sortOfTerm sig (StrNode sym _) = Just $ sortForSym sig sym
sortOfTerm sig (MetaVar _)     = Nothing

sortCheckTerm :: Signature l -> Term l v -> Sort -> ()
sortCheckTerm sig t sort =
  case sortOfTerm sig t of
    Just sort' -> if sort' == sort then
                    ()
                  else
                    error ("Expected term " ++ show t ++ " to have sort " ++ show sort ++ " but was sort " ++ show sort')
    Nothing -> ()

-- This version is for debugging only, and will halt execution on failure
-- Bad terms should never be created
checkTerm :: Signature l -> Term l v -> ()
checkTerm sig t@(Node s ts)   = case getInSig sig s of
                                  NodeSig _ ss _ -> if length ss /= length ts then
                                                      error ("Invalid number of arguments in " ++ show t)
                                                    else
                                                      zipWith (sortCheckTerm sig) ts ss `deepseq`
                                                      map (checkTerm sig) ts `deepseq`
                                                      ()
                                  IntSig _ _     -> error ("In Term " ++ show t ++ ", IntNode symbol used as node: " ++ show s)
                                  StrSig _ _     -> error ("In Term " ++ show t ++ ", StrNode symbol used as node: " ++ show s)
checkTerm sig t@(IntNode s i) = case getInSig sig s of
                                  NodeSig _ _ _ -> error ("In Term " ++ show t ++ ", node symbol used as IntNode: " ++ show s)
                                  IntSig _ _    -> ()
                                  StrSig _ _    -> error ("In Term " ++ show t ++ ", StrNode symbol used as IntNode: " ++ show s)
checkTerm sig t@(StrNode s i) = case getInSig sig s of
                                  NodeSig _ _ _ -> error ("In Term " ++ show t ++ ", node symbol used as IntNode: " ++ show s)
                                  IntSig _ _    -> error ("In Term " ++ show t ++ ", IntNode symbol used as StrNode: " ++ show s)
                                  StrSig _ _    -> ()
checkTerm _   (MetaVar _)   = (())