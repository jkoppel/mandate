{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module TigerAbsyn
  (
    Var(..)
  , Exp(..)
  , Dec(..)
  , Ty(..)
  , Typedec(..)
  , Oper(..)
  , Efield
  , Tfield(..)
  , Vardec(..)
  , Fundec(..)
  , Formals(..)
  , Program(..)
  ) where

import Data.Data ( Data )

import TigerLexer
import qualified TigerSymbol as S

deriving instance Data AlexPosn

data Program = Pexp Exp
             | Pdecs [Dec]
               deriving (Data, Show, Eq)

data Var = SimpleVar (S.Symbol, AlexPosn)
         | FieldVar (Var, S.Symbol, AlexPosn)
         | SubscriptVar (Var, Exp, AlexPosn)
           deriving (Data, Show, Eq)

data Exp = VarExp Var
         | NilExp     AlexPosn
         | IntExp     (Int, AlexPosn)
         | StringExp  (String, AlexPosn)
         | SeqExp     [(Exp, AlexPosn)]
         | AppExp     { appFunc::S.Symbol, appArgs::[Exp], appPos::AlexPosn                    }
         | OpExp      { opLeft::Exp, opOper::Oper, opRight::Exp, opPos::AlexPosn               }
         | RecordExp  { recordFields::[Efield], recordTyp::S.Symbol, recordPos::AlexPosn       }
         | AssignExp  { assignVar::Var, assignExp::Exp, assignPos::AlexPosn                    }
         | IfExp      { ifTest::Exp, ifThen::Exp, ifElse::Maybe Exp, ifPos::AlexPosn           }
         | WhileExp   { whileTest::Exp, whileBody::Exp, whilePos::AlexPosn                     }
         | ForExp     { forVar::Vardec, forLo::Exp, forHi::Exp, forBody::Exp, forPos::AlexPosn }
         | BreakExp   { breakPos::AlexPosn                                                     }
         | LetExp     { letDecs::[Dec], letBody::Exp, letPos::AlexPosn                         }
         | ArrayExp   { arrayTyp::S.Symbol, arraySize::Exp, arrayInit::Exp, arrayPos::AlexPosn }
           deriving (Data, Show, Eq)

data Dec = FunctionDec [Fundec]
         | VarDec { varDecVar::Vardec, 
                    varDecTyp::Maybe (S.Symbol, AlexPosn), 
                    varDecInit::Exp,
                    varDecPos::AlexPosn }
         | TypeDec [Typedec]
           deriving (Data, Show, Eq)

data Ty = NameTy (S.Symbol, AlexPosn)
        | RecordTy [Tfield]
        | ArrayTy (S.Symbol, AlexPosn)
           deriving (Data, Show, Eq)

data Oper = PlusOp | MinusOp | TimesOp | DivideOp
          | EqOp   | NeqOp   | LtOp    | LeOp | GtOp | GeOp
          | AndOp  | OrOp
            deriving (Data, Show, Eq)

type Efield  = (S.Symbol, Exp, AlexPosn)
data Tfield  = Tfield  { tfieldName::S.Symbol, tfieldTyp::S.Symbol, tfieldPos::AlexPosn }
                 deriving (Data, Show, Eq)
data Vardec  = Vardec  { vardecName::S.Symbol, vardecEscape::Bool }
                 deriving (Data, Show, Eq)
data Formals = Formals { formalsVar::Vardec, formalsType::S.Symbol, formalsPos::AlexPosn }
                 deriving (Data, Show, Eq)
data Typedec = Typedec { typedecName::S.Symbol, typedecTy::Ty, typedecPos::AlexPosn }
                 deriving (Data, Show, Eq)
data Fundec = Fundec { 
                       fundecName::S.Symbol
                     , fundecParams::[Tfield]
                     , fundecResult::Maybe(S.Symbol, AlexPosn)
                     , fundecBody::Exp
                     , fundecPos::AlexPosn 
                     }
              deriving (Data, Show, Eq)
