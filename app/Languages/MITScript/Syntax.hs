module Languages.MITScript.Syntax (
    Name(..)
  , Stmt(..)
  , BinOp(..)
  , UnOp(..)
  , Expr(..)
  , RecordPair(..)
  ) where

-- See http://6.035.scripts.mit.edu/sp17/a1.html

newtype Name = Name String
  deriving ( Eq, Ord, Show, Read )

data Stmt = Global Name
          | Assign Expr Expr
          | ExpStmt Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Return Expr
          | Block [Stmt]
  deriving ( Eq, Ord, Show, Read )

data BinOp = PLUS | MINUS | TIMES | DIV | AND | OR | GT | GTE | EQ
  deriving ( Eq, Ord, Show, Read )

data UnOp = UMINUS | NOT
  deriving ( Eq, Ord, Show, Read )

data Expr = BinExp Expr BinOp Expr
          | UnExp UnOp Expr
          | NumConst Int
          | BConst Bool
          | None
          | Str String
          | Var Name
          | FunCall Expr [Expr]
          | FunDecl [Name] Stmt
          | Index Expr Expr
          | FieldAccess Expr Name
          | Record [RecordPair]
  deriving ( Eq, Ord, Show, Read )

data RecordPair = RecordPair Name Expr
  deriving ( Eq, Ord, Show, Read )

