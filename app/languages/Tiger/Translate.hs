{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}

module Languages.Tiger.Translate (
    ToGeneric(..)
  , FromGeneric(..)
  ) where

import Data.Maybe ( fromJust )
import Data.String ( fromString )

import qualified Data.ByteString.Char8 as BS
import Data.Interned ( unintern )
import Data.Interned.ByteString ( InternedByteString )

import qualified TigerAbsyn  as T
import qualified TigerLexer  as T
import qualified TigerSymbol as T

import Term

import Languages.Translation

import           Languages.Tiger.Parse ( emptyPosn, toDumbSymbol )
import           Languages.Tiger.Signature ( Tiger )
import qualified Languages.Tiger.Signature as G

---------------------------------------------------------------------------------------------------------


instance ToGeneric Tiger T.Program where
  toGeneric (T.Pexp e)   = G.PExp (toGeneric e)
  toGeneric (T.Pdecs ds) = G.PDecs (toGeneric ds)

instance ToGeneric Tiger T.Var where
  toGeneric (T.SimpleVar (s, _)) = G.SimpleVar (toGeneric s)
  toGeneric (T.FieldVar (v, s, _)) = G.FieldVar (toGeneric v) (toGeneric s)
  toGeneric (T.SubscriptVar (v, e, _)) = G.SubscriptVar (toGeneric v) (toGeneric e)

instance ToGeneric Tiger T.Exp where
  toGeneric (T.VarExp v) = G.VarExp (toGeneric v)
  toGeneric (T.NilExp _) = G.NilExp
  toGeneric (T.IntExp (n, _)) = G.IntExp (G.ConstInt $ toInteger n)
  toGeneric (T.StringExp (s, _)) = G.StringExp (G.ConstStr $ fromString s)
  toGeneric (T.SeqExp es) = G.SeqExp (toGeneric es)
  toGeneric (T.AppExp fn args _) = G.AppExp (toGeneric fn) (toGeneric args)
  toGeneric (T.OpExp l o r _) = G.OpExp (toGeneric l) (toGeneric o) (toGeneric r)
  toGeneric (T.RecordExp flds typ _) = G.RecordExp (toGeneric flds) (toGeneric typ)
  toGeneric (T.AssignExp v e _) = G.AssignExp (toGeneric v) (toGeneric e)

  toGeneric (T.IfExp e s1 (Just s2) _) = G.IfExp (toGeneric e) (toGeneric s1) (toGeneric s2)
  toGeneric (T.IfExp e s1 Nothing _) = G.IfExp (toGeneric e) (toGeneric s1) G.NilExp

  toGeneric (T.WhileExp e b _) = G.WhileExp (toGeneric e) (toGeneric b)
  toGeneric (T.ForExp v lo hi b _) = G.ForExp (toGeneric v) (toGeneric lo) (toGeneric hi) (toGeneric b)
  toGeneric (T.BreakExp _) = G.BreakExp
  toGeneric (T.LetExp ds b _) = G.LetExp (toGeneric ds) (toGeneric b)
  toGeneric (T.ArrayExp t s i _) = G.ArrayExp (toGeneric t) (toGeneric s) (toGeneric i)

instance ToGeneric Tiger [T.Exp] where
  toGeneric []     = G.NilExpList
  toGeneric (e:es) = G.ConsExpList (toGeneric e) (toGeneric es)

instance ToGeneric Tiger [(T.Exp, T.AlexPosn)] where
  toGeneric []         = G.NilExpList
  toGeneric ((e,_):es) = G.ConsExpList (toGeneric e) (toGeneric es)

instance ToGeneric Tiger T.Dec where
  toGeneric (T.FunctionDec fs) = G.FunctionDec (toGeneric fs)
  toGeneric (T.VarDec vd mt i _) = G.VarDecDec (toGeneric vd) (toGeneric mt) (toGeneric i)
  toGeneric (T.TypeDec ts) = G.TypeDecDec (toGeneric ts)

instance ToGeneric Tiger [T.Dec] where
  toGeneric []     = G.NilDecList
  toGeneric (d:ds) = G.ConsDecList (toGeneric d) (toGeneric ds)

instance ToGeneric Tiger T.Ty where
  toGeneric (T.NameTy (s, _)) = G.NameTy (toGeneric s)
  toGeneric (T.RecordTy ts) = G.RecordTy (toGeneric ts)
  toGeneric (T.ArrayTy (s, _)) = G.ArrayTy (toGeneric s)

instance ToGeneric Tiger T.Oper where
  toGeneric T.PlusOp = G.PlusOp
  toGeneric T.MinusOp = G.MinusOp
  toGeneric T.TimesOp = G.TimesOp
  toGeneric T.DivideOp = G.DivideOp
  toGeneric T.EqOp = G.EqOp
  toGeneric T.NeqOp = G.NeqOp
  toGeneric T.LtOp = G.LtOp
  toGeneric T.LeOp = G.LeOp
  toGeneric T.GtOp = G.GtOp
  toGeneric T.GeOp = G.GeOp
  toGeneric T.AndOp = G.AndOp
  toGeneric T.OrOp = G.OrOp

instance ToGeneric Tiger T.Efield where
  toGeneric (s, e, _) = G.EField (toGeneric s) (toGeneric e)

instance ToGeneric Tiger [T.Efield] where
  toGeneric []     = G.NilEField
  toGeneric (f:fs) = G.ConsEField (toGeneric f) (toGeneric fs)

instance ToGeneric Tiger T.Tfield where
 toGeneric (T.Tfield s t _) = G.TField (toGeneric s) (toGeneric t)

instance ToGeneric Tiger [T.Tfield] where
  toGeneric []     = G.NilTField
  toGeneric (f:fs) = G.ConsTField (toGeneric f) (toGeneric fs)

instance ToGeneric Tiger T.Vardec where
  toGeneric (T.Vardec s _) = G.VarDec (toGeneric s)

instance ToGeneric Tiger T.Formals where
  toGeneric (T.Formals d s _) = G.Formals (toGeneric d) (toGeneric s)

instance ToGeneric Tiger T.Typedec where
  toGeneric (T.Typedec n t _) = G.TypeDec (toGeneric n) (toGeneric t)

instance ToGeneric Tiger [T.Typedec] where
  toGeneric []     = G.NilTypeDec
  toGeneric (t:ts) = G.ConsTypeDec (toGeneric t) (toGeneric ts)

instance ToGeneric Tiger T.Fundec where
  toGeneric (T.Fundec n p r e _) = G.FunDec (toGeneric n) (toGeneric p) (toGeneric r) (toGeneric e)

instance ToGeneric Tiger [T.Fundec] where
  toGeneric []     = G.NilFunDec
  toGeneric (f:fs) = G.ConsFunDec (toGeneric f) (toGeneric fs)

instance ToGeneric Tiger T.Symbol where
  toGeneric (s, _) = G.Symbol $ fromString s

instance ToGeneric Tiger (Maybe T.Symbol) where
  toGeneric Nothing  = G.NoneSym
  toGeneric (Just s) = G.JustSym (toGeneric s)

instance ToGeneric Tiger (Maybe (T.Symbol, T.AlexPosn)) where
  toGeneric Nothing       = G.NoneSym
  toGeneric (Just (s, _)) = G.JustSym (toGeneric s)

---------------------------------------------------------------------------------------------------------

instance FromGeneric Tiger T.Program where
  fromGeneric (G.PExp e)   = T.Pexp <$> fromGeneric e
  fromGeneric (G.PDecs ds) = T.Pdecs <$> fromGeneric ds
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Var where
  fromGeneric (G.SimpleVar s) = T.SimpleVar <$> ((,) <$> fromGeneric s <*> return emptyPosn)
  fromGeneric (G.FieldVar v s) = T.FieldVar <$> ((,,) <$> fromGeneric v <*> fromGeneric s <*> return emptyPosn)
  fromGeneric (G.SubscriptVar v e) = T.SubscriptVar <$> ((,,) <$> fromGeneric v <*> fromGeneric e <*> return emptyPosn)
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Exp where
  fromGeneric (G.VarExp v) = T.VarExp <$> fromGeneric v
  fromGeneric  G.NilExp = return $ T.NilExp emptyPosn
  fromGeneric (G.IntExp (G.ConstInt n)) = return $ T.IntExp (fromInteger n, emptyPosn)
  fromGeneric (G.StringExp (G.ConstStr s)) = return $ T.StringExp (ibsToString s, emptyPosn)
  fromGeneric (G.SeqExp es) = T.SeqExp <$> fromGeneric es
  fromGeneric (G.AppExp fn args) = T.AppExp <$> fromGeneric fn <*> fromGeneric args <*> return emptyPosn
  fromGeneric (G.OpExp l o r) = T.OpExp <$> fromGeneric l <*> fromGeneric o <*> fromGeneric r <*> return emptyPosn
  fromGeneric (G.RecordExp flds typ) = T.RecordExp <$> fromGeneric flds <*> fromGeneric typ <*> return emptyPosn
  fromGeneric (G.AssignExp v e) = T.AssignExp <$> fromGeneric v <*> fromGeneric e <*> return emptyPosn

  fromGeneric (G.IfExp e s1 G.NilExp) = T.IfExp <$> fromGeneric e <*> fromGeneric s1 <*> (return $ Just (T.NilExp emptyPosn)) <*> return emptyPosn
  fromGeneric (G.IfExp e s1 s2) = T.IfExp <$> fromGeneric e <*> fromGeneric s1 <*> (Just <$> fromGeneric s2) <*> return emptyPosn

  fromGeneric (G.WhileExp e b) = T.WhileExp <$> fromGeneric e <*> fromGeneric b <*> return emptyPosn
  fromGeneric (G.ForExp v lo hi b) = T.ForExp <$> fromGeneric v <*> fromGeneric lo <*> fromGeneric hi <*> fromGeneric b <*> return emptyPosn
  fromGeneric  G.BreakExp = return $ T.BreakExp emptyPosn
  fromGeneric (G.LetExp ds b) = T.LetExp <$> fromGeneric ds <*> fromGeneric b <*> return emptyPosn
  fromGeneric (G.ArrayExp t s i) = T.ArrayExp <$> fromGeneric t <*> fromGeneric s <*> fromGeneric i <*> return emptyPosn

  fromGeneric _ = Nothing

instance FromGeneric Tiger [T.Exp] where
  fromGeneric G.NilExpList = return []
  fromGeneric (G.ConsExpList e es) = (:) <$> fromGeneric e <*> fromGeneric es
  fromGeneric _ = Nothing

instance FromGeneric Tiger [(T.Exp, T.AlexPosn)] where
  fromGeneric G.NilExpList         = return []
  fromGeneric (G.ConsExpList e es) = (:) <$> ((,) <$> fromGeneric e <*> return emptyPosn) <*> fromGeneric es
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Dec where
  fromGeneric (G.FunctionDec fs) = T.FunctionDec <$> fromGeneric fs
  fromGeneric (G.VarDecDec vd mt i) = T.VarDec <$> fromGeneric vd <*> fromGeneric mt <*> fromGeneric i <*> return emptyPosn
  fromGeneric (G.TypeDecDec ts) = T.TypeDec <$> fromGeneric ts
  fromGeneric _ = Nothing

instance FromGeneric Tiger [T.Dec] where
  fromGeneric G.NilDecList = return []
  fromGeneric (G.ConsDecList d ds) = (:) <$> fromGeneric d <*> fromGeneric ds
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Ty where
  fromGeneric (G.NameTy s) = T.NameTy <$> ((,) <$> fromGeneric s <*> return emptyPosn)
  fromGeneric (G.RecordTy ts) = T.RecordTy <$> fromGeneric ts
  fromGeneric (G.ArrayTy s) = T.ArrayTy <$> ((,) <$> fromGeneric s <*> return emptyPosn)
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Oper where
  fromGeneric G.PlusOp = return T.PlusOp
  fromGeneric G.MinusOp = return T.MinusOp
  fromGeneric G.TimesOp = return T.TimesOp
  fromGeneric G.DivideOp = return T.DivideOp
  fromGeneric G.EqOp = return T.EqOp
  fromGeneric G.NeqOp = return T.NeqOp
  fromGeneric G.LtOp = return T.LtOp
  fromGeneric G.LeOp = return T.LeOp
  fromGeneric G.GtOp = return T.GtOp
  fromGeneric G.GeOp = return T.GeOp
  fromGeneric G.AndOp = return T.AndOp
  fromGeneric G.OrOp = return T.OrOp
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Efield where
  fromGeneric (G.EField s e) = (,,) <$> fromGeneric s <*> fromGeneric e <*> return emptyPosn
  fromGeneric _ = Nothing

instance FromGeneric Tiger [T.Efield] where
  fromGeneric G.NilEField = return []
  fromGeneric (G.ConsEField f fs) = (:) <$> fromGeneric f <*> fromGeneric fs
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Tfield where
 fromGeneric (G.TField s t) = T.Tfield <$> fromGeneric s <*> fromGeneric t <*> return emptyPosn
 fromGeneric _ = Nothing

instance FromGeneric Tiger [T.Tfield] where
  fromGeneric G.NilTField = return []
  fromGeneric (G.ConsTField f fs) = (:) <$> fromGeneric f <*> fromGeneric fs
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Vardec where
  -- The "escape" attribute seems to be an artifact of later analysis; not part of syntax
  fromGeneric (G.VarDec s) = T.Vardec <$> fromGeneric s <*> return False
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Formals where
  fromGeneric (G.Formals d s) = T.Formals <$> fromGeneric d <*> fromGeneric s <*> return emptyPosn
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Typedec where
  fromGeneric (G.TypeDec n t) = T.Typedec <$> fromGeneric n <*> fromGeneric t <*> return emptyPosn
  fromGeneric _ = Nothing

instance FromGeneric Tiger [T.Typedec] where
  fromGeneric G.NilTypeDec         = return []
  fromGeneric (G.ConsTypeDec t ts) = (:) <$> fromGeneric t <*> fromGeneric ts
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Fundec where
  fromGeneric (G.FunDec n p r e) = T.Fundec <$> fromGeneric n <*> fromGeneric p <*> fromGeneric r <*> fromGeneric e <*> return emptyPosn
  fromGeneric _ = Nothing

instance FromGeneric Tiger [T.Fundec] where
  fromGeneric G.NilFunDec         = return []
  fromGeneric (G.ConsFunDec f fs) = (:) <$> fromGeneric f <*> fromGeneric fs
  fromGeneric _ = Nothing

instance FromGeneric Tiger T.Symbol where
  fromGeneric (G.Symbol s) = return (toDumbSymbol $ ibsToString s)
  fromGeneric _ = Nothing

instance FromGeneric Tiger (Maybe T.Symbol) where
  fromGeneric G.NoneSym     = return Nothing
  fromGeneric (G.JustSym s) = Just <$> fromGeneric s
  fromGeneric _ = Nothing

instance FromGeneric Tiger (Maybe (T.Symbol, T.AlexPosn)) where
  fromGeneric G.NoneSym     = return Nothing
  fromGeneric (G.JustSym s) = Just <$> ((,) <$> fromGeneric s <*> return emptyPosn)
  fromGeneric _ = Nothing