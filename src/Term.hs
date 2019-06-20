{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, EmptyDataDecls, PatternSynonyms, TypeFamilies #-}

module Term (
    Sort(..)
  , Symbol(..)
  , SigNode(..)
  , sigNodeSymbol
  , sigNodeSort
  , sigNodeArity
  , Signature(..)

  , MatchType(..)
  , matchTypeForTerm

  , Term
  , pattern Node
  , pattern Val
  , pattern IntNode
  , pattern StrNode

  , pattern ValVar
  , pattern NonvalVar
  , pattern MetaVar
  , pattern GMetaVar

  , pattern ValStar
  , pattern NonvalStar
  , pattern Star
  , pattern GStar

  , mapTerm
  , traverseTerm

  , getSigNode
  , checkSig
  , checkTerm
  , sortOfTerm
  ) where

import Control.DeepSeq ( deepseq )
import Control.Monad ( (=<<) )
import Control.Monad.Identity ( Identity(..) )

import Data.Function ( on )
import Data.List ( intersperse, find )
import Data.String ( IsString(..) )

import GHC.Generics ( Generic )

import Unsafe.Coerce ( unsafeCoerce )

import qualified Data.ByteString.Char8 as BS
import Data.Hashable ( Hashable(..) )
import Data.Interned ( Interned(..), intern, unintern, Id, Cache, mkCache )
import Data.Interned.ByteString ( InternedByteString(..) )

import Lattice
import Var

-----------------------------------------------------------------------------------------------------------

----------------------------- Signatures -------------------------------------

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

sigNodeArity :: SigNode -> Int
sigNodeArity (NodeSig _ ts _) = length ts
sigNodeArity (ValSig  _ ts _) = length ts
sigNodeArity (IntSig  _ _   ) = 0
sigNodeArity (StrSig  _ _   ) = 0

data Signature l = Signature [SigNode]
  deriving ( Eq, Ord, Show, Generic )


----------------------------- Terms: Match Types -------------------------------------

-- Rules of Val nodes:
-- * May never be reduced
-- * The Abstract Machine Generator will never create a rule that has a non-root
--   Val on the LHS.
--
-- Note that StrNode/IntNode's exist outside the Val/Nonval hierarchy.
-- If you must match them with a Valvar / NonvalVar, then wrap them in a Val or Node

data MatchType = ValueOnly | NonvalOnly | TermOrValue
  deriving ( Eq, Ord, Show, Generic )

instance Hashable MatchType

matchTypeForTerm :: Term l -> MatchType
matchTypeForTerm (Node _ _)      = NonvalOnly
matchTypeForTerm (Val _ _)       = ValueOnly
matchTypeForTerm (IntNode _ _)   = TermOrValue
matchTypeForTerm (StrNode _ _)   = TermOrValue
matchTypeForTerm (GMetaVar _ mt) = mt
matchTypeForTerm (GStar      mt) = mt

instance Meetable MatchType where
  meet a b = if a `prec` b then Just a
             else if b `prec` a then Just b
             else Nothing

  prec _          TermOrValue = True
  prec ValueOnly  ValueOnly   = True
  prec NonvalOnly  NonvalOnly = True
  prec _          _           = False

  isMinimal ValueOnly   = True
  isMinimal NonvalOnly  = True
  isMinimal TermOrValue = False

instance UpperBound MatchType where
  top = TermOrValue

  upperBound x y | x == y = x
  upperBound _ _ = TermOrValue

instance Meetable (Term l) where
  -- I'm not sure if the var cases are correct
  meet x y
    | x == y = Just x
  meet (GMetaVar v1 mt1) (GMetaVar v2 mt2) = GMetaVar (min v1 v2) <$> meet mt1 mt2
  meet (GMetaVar v mt1)  (GStar mt2)       = GMetaVar v <$> meet mt1 mt2
  meet (GStar mt1)       (GMetaVar v mt2)  = GMetaVar v <$> meet mt1 mt2
  meet (GStar mt1) (GStar mt2) = GStar <$> meet mt1 mt2
  meet t s@(GStar _)           = meet s t
  meet (GStar mt) x            = if matchTypeForTerm x `prec` mt then Just x else Nothing
  meet Star       x            = Just x -- FIXME: Is this case redundant
  meet x                 (GMetaVar v  mt) = GMetaVar v <$> meet (matchTypeForTerm x) mt
  meet (GMetaVar v mt)   x                = GMetaVar v <$> meet (matchTypeForTerm x) mt
  meet _                 _                = Nothing


  isMinimal (GStar _) = False
  isMinimal (Val  _ ts) = all isMinimal ts
  isMinimal (Node _ ts) = all isMinimal ts
  isMinimal _           = True

instance UpperBound (Term l) where
  top = Star

  -- TODO: What to do if there's a var?
  upperBound x y
           | x == y = x
  upperBound (Val s1 ts1)  (Val s2 ts2)  = if s1 == s2 then
                                             Val s1 (map (uncurry upperBound) (zip ts1 ts2))
                                           else
                                             ValStar
  upperBound (Node s1 ts1) (Node s2 ts2) = if s1 == s2 then
                                             Val s1 (map (uncurry upperBound) (zip ts1 ts2))
                                           else
                                             ValStar
  -- IntNode, StrNode handled by equality case
  upperBound (GStar mt)    t             = if matchTypeForTerm t `prec` mt then GStar mt else Star
  upperBound t             (GStar mt)    = if matchTypeForTerm t `prec` mt then GStar mt else Star
  upperBound _ _ = Star


----------------------------- Terms -------------------------------------

-- | Terms in language `l`. These should be syntactically valid according to the signature for language `l`
-- of which only one should exist.
--
-- The `Id` field is used internally by the hash table; do not touch. Use the
-- smart constructors defined below, which hide the Id.
data Term l = TNode    !Id !Symbol [Term l]             -- A node which is not a normal form

            | TVal     !Id !Symbol [Term l]             -- A value node, i.e.: does not contain any reducible expressions
                                                        -- Corresponds to variables written "v" in blackboard presentations
                                                        -- of semantics

            | TIntNode !Id !Symbol !Integer             -- An integer. Ideally, would be a special case of `TVal`,
                                                        -- but must be a distinct node to simplify matching

            | TStrNode !Id !Symbol !InternedByteString  -- Like `TIntNode`, but for strings

            | TMetaVar !Id !MetaVar !MatchType          -- A meta-syntactic variable. Closed terms do not have these.
                                                        -- However, some closed syntactic objects (i.e.: continuations)
                                                        -- do bind these and can thereby contain non-closed terms

            | TStar    !Id !MatchType                   -- An abstract node.

instance Show (Term l) where
  showsPrec d (TNode _ s ts) = showsPrec (d+1) s . showList ts
  showsPrec d (TVal  _ s ts) = showsPrec (d+1) s . showList ts
  showsPrec d (TIntNode _ s n) = showsPrec (d+1) s . showString "(" . showsPrec (d+1) n . showString ")"
  showsPrec d (TStrNode _ s str) = showsPrec (d+1) s . showString "(" . showsPrec (d+1) str . showString ")"

  showsPrec d (TMetaVar _ v ValueOnly) = showsPrec d v . showString "v"
  showsPrec d (TMetaVar _ v NonvalOnly) = showsPrec d v . showString "t"
  showsPrec d (TMetaVar _ v TermOrValue) = showsPrec d v

  showsPrec d (TStar _ ValueOnly)   = showString "*v"
  showsPrec d (TStar _ NonvalOnly)  = showString "*t"
  showsPrec d (TStar _ TermOrValue) = showString "*"

  showList ts = showString "(" . foldr (.) id (intersperse (showString ", ") (map (showsPrec 0) ts)) . showString ")"

getId :: Term l -> Id
getId (TNode    i _ _) = i
getId (TVal     i _ _) = i
getId (TIntNode i _ _) = i
getId (TStrNode i _ _) = i
getId (TMetaVar i _ _) = i
getId (TStar    i _)   = i


---------------------------- Generic terms -----------------------------------------

-- Internal use only.
-- A single unifying monotype for all terms. Used for compatibility with the hash-consing machinery
-- (the "intern" library).


-- We need a single Interned instance for all terms. This implies
-- that they must share a cache.
-- This is a special private token used to make things share a cache
-- Has the side effect that can have cache collisions
-- between the same term in different languages. (This is a feature, not a bug.)
data AnyLanguage

type GenericTerm = Term AnyLanguage

-- I tried to get safe coercions working, but couldn't
-- Did not find good tutorials. Maybe it only works with newtypes ATM?
toGeneric :: Term l -> GenericTerm
toGeneric = unsafeCoerce

fromGeneric :: GenericTerm -> Term l
fromGeneric = unsafeCoerce


------------------------------- Interning terms ---------------------------------

-- This is boilerplate for hash-consing terms. See documentation and examples from Edward Kmett's
-- "intern" library

data UninternedTerm l = BNode Symbol [Term l]
                      | BVal  Symbol [Term l]
                      | BIntNode Symbol Integer
                      | BStrNode Symbol InternedByteString
                      | BMetaVar MetaVar MatchType
                      | BStar MatchType


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
                               | DMetaVar MetaVar MatchType
                               | DStar MatchType
    deriving ( Eq, Ord, Generic )

  describe (BNode s ts)     = DNode s (map getId ts)
  describe (BVal  s ts)     = DVal  s (map getId ts)
  describe (BIntNode s n)   = DIntNode s n
  describe (BStrNode s str) = DStrNode s (internedByteStringId str)
  describe (BMetaVar m mt)  = DMetaVar m mt
  describe (BStar mt)       = DStar mt

  identify i = go where
    go (BNode s ts)     = TNode i s ts
    go (BVal  s ts)     = TVal  i s ts
    go (BIntNode s n)   = TIntNode i s n
    go (BStrNode s str) = TStrNode i s str
    go (BMetaVar m mt)  = TMetaVar i m mt
    go (BStar mt)       = TStar i mt

  cache = termCache

instance Hashable (Description GenericTerm)

instance Hashable (Term l) where
  hashWithSalt s t = s `hashWithSalt` getId t


instance Eq (Term l) where
  (==) = (==) `on` getId

instance Ord (Term l) where
  compare = compare `on` getId

termCache :: Cache GenericTerm
termCache = mkCache
{-# NOINLINE termCache #-}

----------------------------- Smart constructors -------------------------------------


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

pattern ValVar :: MetaVar -> Term l
pattern ValVar v <- (TMetaVar _ v ValueOnly) where
  ValVar v = fromGeneric $ intern $ toUGeneric (BMetaVar v ValueOnly)

pattern NonvalVar :: MetaVar -> Term l
pattern NonvalVar v <- (TMetaVar _ v NonvalOnly) where
  NonvalVar v = fromGeneric $ intern $ toUGeneric (BMetaVar v NonvalOnly)

pattern MetaVar :: MetaVar -> Term l
pattern MetaVar v <- (TMetaVar _ v TermOrValue) where
  MetaVar v = fromGeneric $ intern $ toUGeneric (BMetaVar v TermOrValue)

pattern GMetaVar :: MetaVar -> MatchType -> Term l
pattern GMetaVar v mt <- (TMetaVar _ v mt) where
  GMetaVar v mt = fromGeneric $ intern $ toUGeneric (BMetaVar v mt)

pattern ValStar :: Term l
pattern ValStar <- (TStar _ ValueOnly) where
  ValStar = fromGeneric $ intern $ toUGeneric (BStar ValueOnly)

pattern NonvalStar :: Term l
pattern NonvalStar <- (TStar _ NonvalOnly) where
  NonvalStar = fromGeneric $ intern $ toUGeneric (BStar NonvalOnly)

pattern Star :: Term l
pattern Star <- (TStar _ TermOrValue) where
  Star = fromGeneric $ intern $ toUGeneric (BStar TermOrValue)


pattern GStar :: MatchType -> Term l
pattern GStar mt <- (TStar _ mt) where
  GStar mt  = fromGeneric $ intern $ toUGeneric (BStar mt)


---------------------------------- Traversals --------------------------------------------------

-- | Similar to `traverse` from the `Traversable` class, except that `Term` does not satisfy `Traversable`
traverseTerm :: Monad m => (Term l -> m (Term l)) -> Term l -> m (Term l)
traverseTerm f (Node s ts)      = f =<< (Node s <$> mapM (traverseTerm f) ts)
traverseTerm f (Val  s ts)      = f =<< (Val  s <$> mapM (traverseTerm f) ts)
traverseTerm f t@(IntNode _ _)  = f t
traverseTerm f t@(StrNode _ _)  = f t
traverseTerm f t@(GMetaVar _ _) = f t
traverseTerm f t@(GStar _)      = f t

-- | Similar to `fmap`, except that `Term` is not a `Functor`
mapTerm :: (Term l -> Term l) -> Term l -> Term l
mapTerm f t = runIdentity (traverseTerm (Identity . f) t)

---------------------------------------- Sort-checking  --------------------------------------------

-- | `checkSig` checks that a signature only contains sorts from the given list

-- I feel like a hypocrite using string literals (as opposed to named constants) in signature definitions.
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
getSigNode :: Signature l -> Symbol -> SigNode
getSigNode (Signature sig) s = case find (\n -> sigNodeSymbol n == s) sig of
                                 Just n -> n
                                 Nothing -> error ("Cannot find symbol " ++ show s ++ " in signature " ++ show sig)

-- private helper
sortForSym :: Signature l -> Symbol -> Sort
sortForSym sig s = sigNodeSort $ getSigNode sig s

-- Metavars/stars are currently unsorted / can have any sort
sortOfTerm :: Signature l -> Term l -> Maybe Sort
sortOfTerm sig (Node    sym _) = Just $ sortForSym sig sym
sortOfTerm sig (Val     sym _) = Just $ sortForSym sig sym
sortOfTerm sig (IntNode sym _) = Just $ sortForSym sig sym
sortOfTerm sig (StrNode sym _) = Just $ sortForSym sig sym
sortOfTerm sig (GMetaVar _ _)  = Nothing
sortOfTerm sig (GStar _)       = Nothing


-- | Checks that a term is syntactically valid according to the given signature
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
checkTerm sig t@(Node s ts)   = case getSigNode sig s of
                                  NodeSig _ ss _ -> checkInternalNode sig t ss ts
                                  sym            -> error ("In Term " ++ show t ++ ", symbol " ++ show sym ++ " used as node: " ++ show s)
checkTerm sig t@(Val  s ts)   = case getSigNode sig s of
                                  ValSig  _ ss _ -> checkInternalNode sig t ss ts
                                  sym            -> error ("In Term " ++ show t ++ ", symbol " ++ show sym ++ " used as ValNode: " ++ show s)
checkTerm sig t@(IntNode s i) = case getSigNode sig s of
                                  IntSig _ _    -> ()
                                  sym            -> error ("In Term " ++ show t ++ ", symbol " ++ show sym ++ " used as IntNode: " ++ show s)
checkTerm sig t@(StrNode s i) = case getSigNode sig s of
                                  StrSig _ _    -> ()
                                  sym            -> error ("In Term " ++ show t ++ ", symbol " ++ show sym ++ " used as StrNode: " ++ show s)
checkTerm sig (GStar _)       = ()
checkTerm _   (GMetaVar _ _)  = ()


------------------------------------------- Code generation ----------------------------------------------------------

charToString :: Char -> String
charToString = (:[])

-- I want to have a bunch of manually-specified pattern synonyms that can be changed by hand (and not a TH block),
-- but I don't want to write the initial ones by hand. Hence, this function, to be invoked from GHCI
--
-- Perhaps I'll change my mind about that
--
-- Yes, this currently generates incorrect decls for int/str nodes.
-- TODO: Move this somewhere out of the way
patSymForSigNode :: String -> SigNode -> String
patSymForSigNode lang sn = decl ++ "\n" ++ defn
  where
    decl = "pattern " ++ show (sigNodeSymbol sn) ++ " :: " ++ concat (intersperse " -> " $ replicate (1 + sigNodeArity sn) ("Term " ++ lang))
    defn = "pattern " ++ show (sigNodeSymbol sn) ++ " " ++ intersperse ' ' (varnames ++ "=") ++ " " ++ concat (intersperse " " ([renderNode sn] ++ ["\"" ++ show (sigNodeSymbol sn) ++ "\""] ++ args))

    renderNode (ValSig _ _ _) = "Val"
    renderNode (NodeSig _ _ _) = "Node"
    renderNode (IntSig _ _) = "IntNode"
    renderNode (StrSig _ _) = "StrNode"

    argList = "[" ++ concat (intersperse ", " (map charToString varnames)) ++ "]"

    args = case sn of
             (NodeSig _ _ _ ) -> [argList]
             (ValSig _ _ _) -> [argList]
             (IntSig _ _) -> []
             (StrSig _ _) -> []

    -- Using (!!) for fail-fast
    varnames = map (['a'..'z'] !!) ([0..(sigNodeArity sn - 1)])