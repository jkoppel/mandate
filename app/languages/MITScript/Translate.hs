{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Languages.MITScript.Translate (
    ToGeneric(..)
  , FromGeneric(..)
  ) where

import Data.Maybe ( fromJust )
import Data.String ( fromString )

import qualified Data.ByteString.Char8 as BS
import Data.Interned ( unintern )
import Data.Interned.ByteString ( InternedByteString )

import Term

import           Languages.MITScript.Signature ( MITScript )
import qualified Languages.MITScript.Signature as G
import qualified Languages.MITScript.Syntax    as M

---------------------------------------------------------------------------------------------------------

class ToGeneric l a where
  toGeneric :: a -> Term l

instance ToGeneric MITScript M.Name where
  toGeneric (M.Name s) = G.Name (fromString s)

instance ToGeneric MITScript [M.Name] where
  toGeneric []     = G.NilName
  toGeneric (n:ns) = G.ConsName (toGeneric n) (toGeneric ns)

instance ToGeneric MITScript M.Stmt where
  toGeneric (M.Global n) = G.Global (toGeneric n)
  toGeneric (M.Assign e1 e2) = G.Assign (toGeneric e1) (toGeneric e2)
  toGeneric (M.ExpStmt s) = G.ExpStmt (toGeneric s)
  toGeneric (M.If p t e) = G.If (toGeneric p) (toGeneric t) (toGeneric e)
  toGeneric (M.While e s) = G.While (toGeneric e) (toGeneric s)
  toGeneric (M.Return e) = G.Return (toGeneric e)
  toGeneric (M.Block ss) = G.Block (toGeneric ss)

instance ToGeneric MITScript [M.Stmt] where
  toGeneric []     = G.NilStmt
  toGeneric (s:ss) = G.ConsStmt (toGeneric s) (toGeneric ss)

instance ToGeneric MITScript M.BinOp where
  toGeneric M.PLUS = G.PLUS
  toGeneric M.MINUS = G.MINUS
  toGeneric M.TIMES = G.TIMES
  toGeneric M.DIV = G.DIV
  toGeneric M.AND = G.AND
  toGeneric M.OR = G.OR
  toGeneric M.GT = G.GT
  toGeneric M.GTE = G.GTE
  toGeneric M.EQ = G.EQ

instance ToGeneric MITScript M.UnOp where
  toGeneric M.UMINUS = G.UMINUS
  toGeneric M.NOT = G.NOT

instance ToGeneric MITScript M.Expr where
  toGeneric (M.BinExp e1 o e2) = G.BinExp (toGeneric e1) (toGeneric o) (toGeneric e2)
  toGeneric (M.UnExp u e) = G.UnExp (toGeneric u) (toGeneric e)

  -- Interesting cases
  toGeneric (M.NumConst n) = G.NumConst (G.ConstInt $ toInteger n)
  toGeneric (M.BConst True) = G.BConst G.True
  toGeneric (M.BConst False) = G.BConst G.False
  toGeneric M.None = G.None
  toGeneric (M.Str s) = G.Str (G.ConstStr $ fromString s)

  toGeneric (M.Var v) = G.Var (toGeneric v)
  toGeneric (M.FunCall e es) = G.FunCall (toGeneric e) (toGeneric es)
  toGeneric (M.FunDecl nms s) = G.FunDecl (toGeneric nms) (toGeneric s)
  toGeneric (M.Index e1 e2) = G.Index (toGeneric e1) (toGeneric e2)
  toGeneric (M.FieldAccess e n) = G.FieldAccess (toGeneric e) (toGeneric n)
  toGeneric (M.Record rps) = G.Record (toGeneric rps)

instance ToGeneric MITScript [M.Expr] where
  toGeneric []     = G.NilExpr
  toGeneric (e:es) = G.ConsExpr (toGeneric e) (toGeneric es)

instance ToGeneric MITScript M.RecordPair where
  toGeneric (M.RecordPair n e) = G.RecordPair (toGeneric n) (toGeneric e)

instance ToGeneric MITScript [M.RecordPair] where
  toGeneric []     = G.NilRecordPair
  toGeneric (r:rs) = G.ConsRecordPair (toGeneric r) (toGeneric rs)

---------------------------------------------------------------------------------------------------------

class FromGeneric l a where
  fromGeneric :: Term l -> Maybe a

ibsToString :: InternedByteString -> String
ibsToString = BS.unpack . unintern

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
  fromGeneric G.NilExpr = return []
  fromGeneric (G.ConsExpr e es) = (:) <$> fromGeneric e <*> fromGeneric es
  fromGeneric _ = Nothing

instance FromGeneric MITScript M.RecordPair where
  fromGeneric (G.RecordPair n e) = M.RecordPair <$> fromGeneric n <*> fromGeneric e
  fromGeneric _ = Nothing

instance FromGeneric MITScript [M.RecordPair] where
  fromGeneric G.NilRecordPair = return []
  fromGeneric (G.ConsRecordPair rp rps) = (:) <$> fromGeneric rp <*> fromGeneric rps
  fromGeneric _ = Nothing


-- TODO: Make an actual tests dir, and set this up with QuickCheck. And don't forget to run checkTerm on the output
checkRoundTrip :: M.Stmt -> ()
checkRoundTrip b = if fromJust (fromGeneric (toGeneric b :: Term MITScript)) == b then () else error ("Failed checkRoundTrip: " ++ show b)

checkRoundTrip' :: M.Stmt -> ()
checkRoundTrip' b = if toGeneric (fromJust (fromGeneric (toGeneric b :: Term MITScript)) :: M.Stmt) == (toGeneric b :: Term MITScript) then () else error ("Failed checkRoundTrip': " ++ show b)