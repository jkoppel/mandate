{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric, EmptyDataDecls, PatternSynonyms, TypeFamilies #-}

module Term (
  Sort(..)
, Symbol(..)
, SigNode(..)
, sigNodeSymbol
, sigNodeSort
, Signature(..)

, Term
, pattern Node
, pattern Val
, pattern IntNode
, pattern StrNode
, pattern MetaVar

, checkSig
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
             | ValSig  !Symbol [Sort] Sort
             | IntSig  !Symbol Sort
             | StrSig  !Symbol Sort
  deriving ( Eq, Ord, Show, Generic )

sigNodeSymbol :: SigNode -> Symbol
sigNodeSymbol (NodeSig s _ _) = s
sigNodeSymbol (ValSig  s _ _) = s
sigNodeSymbol (IntSig s _)    = s
sigNodeSymbol (StrSig s _)    = s

sigNodeSort :: SigNode -> Sort
sigNodeSort (NodeSig _ _ s) = s
sigNodeSort (ValSig  _ _ s) = s
sigNodeSort (IntSig _ s)    = s
sigNodeSort (StrSig _ s)    = s

data Signature l = Signature [SigNode]
  deriving ( Eq, Ord, Show, Generic )

-- Need a single Interned instance for all terms. This implies
-- that they must share a cache.
-- This is a special private token used to make things share a cache
-- Has the side effect that can have cache collisions
-- between the same term in different languages. (This is a feature, not a bug.)
data AnyLanguage

-- Rules of Val nodes:
-- * May never be reduced
-- * The Abstract Machine Generator will never create a rule that has a non-root
--   Val on the LHS.

data Term l = TNode    !Id !Symbol [Term l]
            | TVal     !Id !Symbol [Term l]
            | TIntNode !Id !Symbol !Integer
            | TStrNode !Id !Symbol !InternedByteString
            | TMetaVar !Id !MetaVar

instance Show (Term l) where
  showsPrec d (TNode _ s ts) = showsPrec (d+1) s . showList ts
  showsPrec d (TVal  _ s ts) = showsPrec (d+1) s . showList ts
  showsPrec d (TIntNode _ s n) = showsPrec (d+1) s . showString "(" . showsPrec (d+1) n . showString ")"
  showsPrec d (TStrNode _ s str) = showsPrec (d+1) s . showString "(" . showsPrec (d+1) str . showString ")"
  showsPrec d (TMetaVar _ v) = showsPrec d v

  showList ts = showString "(" . foldr (.) id (intersperse (showString ", ") (map (showsPrec 0) ts)) . showString ")"

getId :: Term l -> Id
getId (TNode    i _ _) = i
getId (TVal     i _ _) = i
getId (TIntNode i _ _) = i
getId (TStrNode i _ _) = i
getId (TMetaVar i _)   = i

type GenericTerm = Term AnyLanguage

-- I tried to get safe coercions working, but couldn't
-- Did not find good tutorials. Maybe it only works with newtypes ATM?
toGeneric :: Term l -> GenericTerm
toGeneric = unsafeCoerce

fromGeneric :: GenericTerm -> Term l
fromGeneric = unsafeCoerce


data UninternedTerm l = BNode Symbol [Term l]
                      | BVal  Symbol [Term l]
                      | BIntNode Symbol Integer
                      | BStrNode Symbol InternedByteString
                      | BMetaVar MetaVar

type GenericUninternedTerm = UninternedTerm AnyLanguage


toUGeneric :: UninternedTerm l -> GenericUninternedTerm
toUGeneric = unsafeCoerce

fromUGeneric :: GenericUninternedTerm -> UninternedTerm l
fromUGeneric = unsafeCoerce

instance Interned GenericTerm where
  type Uninterned GenericTerm = GenericUninternedTerm
  data Description GenericTerm = DNode Symbol [Id]
                               | DVal  Symbol [Id]
                               | DIntNode Symbol Integer
                               | DStrNode Symbol Id
                               | DMetaVar MetaVar
    deriving ( Eq, Ord, Generic )

  describe (BNode s ts)     = DNode s (map getId ts)
  describe (BVal  s ts)     = DVal  s (map getId ts)
  describe (BIntNode s n)   = DIntNode s n
  describe (BStrNode s str) = DStrNode s (internedByteStringId str)
  describe (BMetaVar m)     = DMetaVar m

  identify i = go where
    go (BNode s ts)     = TNode i s ts
    go (BVal  s ts)     = TVal  i s ts
    go (BIntNode s n)   = TIntNode i s n
    go (BStrNode s str) = TStrNode i s str
    go (BMetaVar m)    = TMetaVar i m

  cache = termCache

instance Hashable (Description GenericTerm)


instance Eq (Term l) where
  (==) = (==) `on` getId

instance Ord (Term l) where
  compare = compare `on` getId

termCache :: Cache GenericTerm
termCache = mkCache
{-# NOINLINE termCache #-}


pattern Node :: Symbol -> [Term l] -> Term l
pattern Node s ts <- (TNode _ s ts) where
  Node s ts = fromGeneric $ intern $ toUGeneric (BNode s ts)

pattern Val :: Symbol -> [Term l] -> Term l
pattern Val s ts <- (TVal _ s ts) where
  Val s ts = fromGeneric $ intern $ toUGeneric (BVal s ts)

pattern IntNode :: Symbol -> Integer -> Term l
pattern IntNode s n <- (TIntNode _ s n) where
  IntNode s n = fromGeneric $ intern $ toUGeneric (BIntNode s n)

pattern StrNode :: Symbol -> InternedByteString -> Term l
pattern StrNode s str <- (TStrNode _ s str) where
  StrNode s str = fromGeneric $ intern $ toUGeneric (BStrNode s str)

pattern MetaVar :: MetaVar -> Term l
pattern MetaVar v <- (TMetaVar _ v) where
  MetaVar v = fromGeneric $ intern $ toUGeneric (BMetaVar v)

------------------------------------------------------------------------------------

-- I feel like a hypocrite using string constants in signature definitions.
-- The reason is that I really don't want to give a non-capitalized name
-- to any sort constant....but Haskell won't let me use capitalized names
-- (unless I make them all pattern synonyms, with all the syntax that entails).
--
-- So....next best thing: Programmatic consistency check
checkSig :: [Sort] -> Signature l -> ()
checkSig sorts (Signature sigs) = map checkNodeSig sigs `deepseq` ()
  where
    checkSort s = if elem s sorts then () else error $ "Signature contains illegal sort " ++ show s

    checkNodeSig (NodeSig _ ss s) = map checkSort ss `deepseq` checkSort s `deepseq` ()
    checkNodeSig (ValSig  _ ss s) = map checkSort ss `deepseq` checkSort s `deepseq` ()
    checkNodeSig (IntSig  _    s) = checkSort s `deepseq` ()
    checkNodeSig (StrSig  _    s) = checkSort s `deepseq` ()


-- If want the language in error messages, can add Typeable constraints and show the TypeRep
getInSig :: Signature l -> Symbol -> SigNode
getInSig (Signature sig) s = case find (\n -> sigNodeSymbol n == s) sig of
                           Just n -> n
                           Nothing -> error ("Cannot find symbol " ++ show s ++ " in signature " ++ show sig)

sortForSym :: Signature l -> Symbol -> Sort
sortForSym sig s = sigNodeSort $ getInSig sig s

-- Metavars are currently unsorted / can have any sort
sortOfTerm :: Signature l -> Term l -> Maybe Sort
sortOfTerm sig (Node    sym _) = Just $ sortForSym sig sym
sortOfTerm sig (Val     sym _) = Just $ sortForSym sig sym
sortOfTerm sig (IntNode sym _) = Just $ sortForSym sig sym
sortOfTerm sig (StrNode sym _) = Just $ sortForSym sig sym
sortOfTerm sig (MetaVar _)     = Nothing

sortCheckTerm :: Signature l -> Term l -> Sort -> ()
sortCheckTerm sig t sort =
  case sortOfTerm sig t of
    Just sort' -> if sort' == sort then
                    ()
                  else
                    error ("Expected term " ++ show t ++ " to have sort " ++ show sort ++ " but was sort " ++ show sort')
    Nothing -> ()


-- This here is boxing at its finest.
checkInternalNode :: Signature l -> Term l -> [Sort] -> [Term l] -> ()
checkInternalNode sig t ss ts = if length ss /= length ts then
                                  error ("Invalid number of arguments in " ++ show t)
                                else
                                  zipWith (sortCheckTerm sig) ts ss `deepseq`
                                  map (checkTerm sig) ts `deepseq`
                                  ()

-- This version is for debugging only, and will halt execution on failure
-- Bad terms should never be created
checkTerm :: Signature l -> Term l -> ()
checkTerm sig t@(Node s ts)   = case getInSig sig s of
                                  NodeSig _ ss _ -> checkInternalNode sig t ss ts
                                  sym            -> error ("In Term " ++ show t ++ ", symbol " ++ show sym ++ " used as node: " ++ show s)
checkTerm sig t@(Val  s ts)   = case getInSig sig s of
                                  ValSig  _ ss _ -> checkInternalNode sig t ss ts
                                  sym            -> error ("In Term " ++ show t ++ ", symbol " ++ show sym ++ " used as ValNode: " ++ show s)
checkTerm sig t@(IntNode s i) = case getInSig sig s of
                                  IntSig _ _    -> ()
                                  sym            -> error ("In Term " ++ show t ++ ", symbol " ++ show sym ++ " used as IntNode: " ++ show s)
checkTerm sig t@(StrNode s i) = case getInSig sig s of
                                  StrSig _ _    -> ()
                                  sym            -> error ("In Term " ++ show t ++ ", symbol " ++ show sym ++ " used as StrNode: " ++ show s)
checkTerm _   (MetaVar _)   = (())