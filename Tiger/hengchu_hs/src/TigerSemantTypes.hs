module TigerSemantTypes
       (
         Ty(..)
       , Uniq
       ) where

import TigerSymbol
import Data.IORef

type Uniq = Integer

-- These are the semantic types of tiger langauge. Used during type checking.
data Ty = Record ([(Symbol, Ty)], Uniq)
        | Nil
        | INT
        | String
        | Array (Ty, Uniq)
        | Name (Symbol, IORef (Maybe Ty))
        | Unit

instance Show Ty where
  show (Record (sts, u)) = "Record: (" ++ show sts ++ ", " ++ show u ++ ")"
  show Nil = "Nil"
  show INT = "Int"
  show String = "String"
  show (Array(t, u)) = "Array: (" ++ show t ++ ", " ++ show u ++ ")"
  show (Name(s, _)) = "Name: " ++ show s
  show Unit = "Unit"

instance Eq Ty where
  (Record _)           == Nil               = True
  Nil                  == (Record _)        = True
  Record (stypairs, u) == Record (stypairs', u') = stypairs == stypairs' && u == u'
  Nil                  == Nil               = True
  INT                  == INT               = True
  String               == String            = True
  Array (t, u)         == Array (t', u')    = t == t' && u == u'
  Name (s, ioref)      == Name (s', ioref') = s == s' && ioref == ioref'
  Unit                 == Unit              = True
  _ == _ = False
