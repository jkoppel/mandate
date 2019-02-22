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
import qualified TigerSymbol as T

import Term

import Languages.Translation

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

instance ToGeneric Tiger [(T.Exp, a)] where
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

instance ToGeneric Tiger (Maybe (T.Symbol, a)) where
  toGeneric Nothing       = G.NoneSym
  toGeneric (Just (s, _)) = G.JustSym (toGeneric s)

---------------------------------------------------------------------------------------------------------

{-

instance FromGeneric MITScript M.Name where
  fromGeneric (G.Name s) = return $ M.Name (ibsToString s)
  fromGeneric _ = Nothing

instance FromGeneric MITScript [M.Name] where
  fromGeneric G.NilName = return []
  fromGeneric (G.ConsName n ns) = (:) <$> fromGeneric n <*> fromGeneric ns
  fromGeneric _ = Nothing

instance FromGeneric MITScript M.Stmt where
  fromGeneric (G.Global n) = M.Global <$> fromGeneric n
  fromGeneric (G.Assign e1 e2) = M.Assign <$> fromGeneric e1 <*> fromGeneric e2
  fromGeneric (G.ExpStmt s) = M.ExpStmt <$> fromGeneric s
  fromGeneric (G.If p t e) = M.If <$> fromGeneric p <*> fromGeneric t <*> fromGeneric e
  fromGeneric (G.While e s) = M.While <$> fromGeneric e <*> fromGeneric s
  fromGeneric (G.Return e) = M.Return <$> fromGeneric e
  fromGeneric (G.Block ss) = M.Block <$> fromGeneric ss
  fromGeneric _ = Nothing

instance FromGeneric MITScript [M.Stmt] where
  fromGeneric G.NilStmt = return []
  fromGeneric (G.ConsStmt s ss) = (:) <$> fromGeneric s <*> fromGeneric ss
  fromGeneric _ = Nothing

instance FromGeneric MITScript M.BinOp where
  fromGeneric G.PLUS = return M.PLUS
  fromGeneric G.MINUS = return M.MINUS
  fromGeneric G.TIMES = return M.TIMES
  fromGeneric G.DIV = return M.DIV
  fromGeneric G.AND = return M.AND
  fromGeneric G.OR = return M.OR
  fromGeneric G.GT = return M.GT
  fromGeneric G.GTE = return M.GTE
  fromGeneric G.EQ = return M.EQ
  fromGeneric _ = Nothing


instance FromGeneric MITScript M.UnOp where
  fromGeneric G.UMINUS = return M.UMINUS
  fromGeneric G.NOT = return M.NOT
  fromGeneric _ = Nothing

instance FromGeneric MITScript M.Expr where
  fromGeneric (G.BinExp e1 o e2) = M.BinExp <$> fromGeneric e1 <*> fromGeneric o <*> fromGeneric e2
  fromGeneric (G.UnExp u e) = M.UnExp <$> fromGeneric u <*> fromGeneric e

  -- Interesting cases
  fromGeneric (G.NumConst (G.ConstInt n)) = return $ M.NumConst (fromInteger n)
  fromGeneric (G.BConst G.True)  = return $ M.BConst True
  fromGeneric (G.BConst G.False) = return $ M.BConst False
  fromGeneric G.None = return M.None
  fromGeneric (G.Str (G.ConstStr s)) = return $ M.Str (ibsToString s)

  fromGeneric (G.Var v) = M.Var <$> fromGeneric v
  fromGeneric (G.FunCall e es) = M.FunCall <$> fromGeneric e <*> fromGeneric es
  fromGeneric (G.FunDecl nms s) = M.FunDecl <$> fromGeneric nms <*> fromGeneric s
  fromGeneric (G.Index e1 e2) = M.Index <$> fromGeneric e1 <*> fromGeneric e2
  fromGeneric (G.FieldAccess e n) = M.FieldAccess <$> fromGeneric e <*> fromGeneric n
  fromGeneric (G.Record rps) = M.Record <$> fromGeneric rps
  fromGeneric _ = Nothing

instance FromGeneric MITScript [M.Expr] where
  fromGeneric G.NilExp = return []
  fromGeneric (G.ConsExp e es) = (:) <$> fromGeneric e <*> fromGeneric es
  fromGeneric _ = Nothing

instance FromGeneric MITScript M.RecordPair where
  fromGeneric (G.RecordPair n e) = M.RecordPair <$> fromGeneric n <*> fromGeneric e
  fromGeneric _ = Nothing

instance FromGeneric MITScript [M.RecordPair] where
  fromGeneric G.NilRecordPair = return []
  fromGeneric (G.ConsRecordPair rp rps) = (:) <$> fromGeneric rp <*> fromGeneric rps
  fromGeneric _ = Nothing

-}
