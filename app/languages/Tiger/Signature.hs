{-# LANGUAGE EmptyDataDecls, OverloadedStrings, PatternSynonyms #-}

module Languages.Tiger.Signature where

import Control.Exception

import Data.Interned.ByteString ( InternedByteString )

import Term

data Tiger

tigerSorts :: [Sort]
tigerSorts = [ "Program",  "Exp", "ExpList", "ExpOpt", "Dec", "DecList"
             , "Var", "FunDec" , "FunDecList", "Ty", "TField", "TFieldList"
             , "Oper", "EField", "EFieldList", "VarDec", "Formals", "TypeDec", "TypeDecList"
             , "Symbol", "SymOpt", "ConstInt", "ConstStr"
             ]


-- TODO: Nodes for runtime values (e.g.: eval'd record, stack frame)
tigerSig :: Signature Tiger
tigerSig = Signature [ NodeSig "PExp"  ["Exp"]     "Program"
                     , NodeSig "PDecs" ["DecList"] "Program"


                     , NodeSig "SimpleVar"    ["Symbol"]        "Var"
                     , NodeSig "FieldVar"     ["Var", "Symbol"] "Var"
                     , NodeSig "SubscriptVar" ["Var", "Exp"]    "Var"

                     , NodeSig "VarExp"    ["Var"]                         "Exp"
                     , ValSig  "NilExp"    []                              "Exp"
                     , ValSig  "IntExp"    ["ConstInt"]                    "Exp"
                     , ValSig  "StringExp" ["ConstStr"]                    "Exp"
                     , NodeSig "SeqExp"    ["ExpList"]                     "Exp"
                     , NodeSig "AppExp"    ["Symbol", "ExpList"]           "Exp"
                     , NodeSig "OpExp"     ["Exp", "Oper", "Exp"]          "Exp"
                     , NodeSig "RecordExp" ["EFieldList", "Symbol"]        "Exp"
                     , NodeSig "AssignExp" ["Var", "Exp"]                  "Exp"
                     , NodeSig "IfExp"     ["Exp", "Exp", "Exp"]           "Exp"
                     , NodeSig "WhileExp"  ["Exp", "Exp"]                  "Exp"
                     , NodeSig "ForExp"    ["VarDec", "Exp", "Exp", "Exp"] "Exp"
                     , NodeSig "BreakExp"  []                              "Exp"
                     , NodeSig "LetExp"    ["DecList", "Exp"]              "Exp"
                     , NodeSig "ArrayExp"  ["Symbol", "Exp", "Exp"]        "Exp"

                     , NodeSig "FunctionDec" ["FunDecList"]              "Dec"
                     , NodeSig "VarDec"      ["VarDec", "SymOpt", "Exp"] "Dec"
                     , NodeSig "TypeDec"     ["TypeDecList"]             "Dec"

                     , NodeSig "NameTy"   ["Symbol"] "Ty"
                     , NodeSig "RecordTy" ["TField"] "Ty"
                     , NodeSig "ArrayTy"  ["Symbol"] "Ty"

                     , NodeSig "PlusOp"   [] "Oper"
                     , NodeSig "MinusOp"  [] "Oper"
                     , NodeSig "TimesOp"  [] "Oper"
                     , NodeSig "DivideOp" [] "Oper"
                     , NodeSig "EqOp"     [] "Oper"
                     , NodeSig "NeqOp"    [] "Oper"
                     , NodeSig "LtOp"     [] "Oper"
                     , NodeSig "LeOp"     [] "Oper"
                     , NodeSig "GtOp"     [] "Oper"
                     , NodeSig "AndOp"    [] "Oper"
                     , NodeSig "OrOp"     [] "Oper"

                     , NodeSig "EField" ["Symbol", "Exp"] "EField"

                     , NodeSig "TField" ["Symbol", "Symbol"] "TField"

                     -- What is that "escape" boolean in their grammar
                     , NodeSig "VarDec" ["Symbol"] "VarDec"

                     , NodeSig "Formals" ["VarDec", "Symbol"] "Formals"

                     , NodeSig "TypeDec" ["Symbol", "Ty"] "TypeDec"

                     , NodeSig "FunDec" ["Symbol", "TFieldList", "SymOpt", "Exp"] "FunDec"

                     , NodeSig "NilFunDec"  []                       "FunDecList"
                     , NodeSig "ConsFunDec" ["FunDec", "FunDecList"] "FunDecList"

                     , NodeSig "NilExpList"  []                 "ExpList"
                     , NodeSig "ConsExpList" ["Exp", "ExpList"] "ExpList"

                     , NodeSig "NilTypeDec"  []                         "TypeDecList"
                     , NodeSig "ConsTypeDec" ["TypeDec", "TypeDecList"] "TypeDecList"

                     , NodeSig "NilTField"  []                       "TFieldList"
                     , NodeSig "ConsTField" ["TField", "TFieldList"] "TFieldList"

                     , NodeSig "NoneSym" []         "SymOpt"
                     , NodeSig "JustSym" ["Symbol"] "SymOpt"


                     , StrSig "Symbol" "Symbol"
                     , StrSig "ConstStr" "ConstStr"
                     , IntSig "ConstInt" "ConstInt"
                     ]

--------------------------------------------------------------------------------------------------------------------

-- Generated using "patSymForSigNode" in Term.hs; fixed by hand

{-
pattern Name :: InternedByteString -> Term MITScript
pattern Name s = StrNode "Name" s

pattern NilName :: Term MITScript
pattern NilName = Val "NilName" []

pattern ConsName :: Term MITScript -> Term MITScript -> Term MITScript
pattern ConsName a b = Val "ConsName" [a, b]

pattern Global :: Term MITScript -> Term MITScript
pattern Global a = Node "Global" [a]

pattern Assign :: Term MITScript -> Term MITScript -> Term MITScript
pattern Assign a b = Node "Assign" [a, b]

pattern ExpStmt :: Term MITScript -> Term MITScript
pattern ExpStmt a = Node "ExpStmt" [a]

pattern If :: Term MITScript -> Term MITScript -> Term MITScript -> Term MITScript
pattern If a b c = Node "If" [a, b, c]

pattern While :: Term MITScript -> Term MITScript -> Term MITScript
pattern While a b = Node "While" [a, b]

pattern Return :: Term MITScript -> Term MITScript
pattern Return a = Val "Return" [a]

pattern Block :: Term MITScript -> Term MITScript
pattern Block a = Node "Block" [a]

pattern NilStmt :: Term MITScript
pattern NilStmt = Val "NilStmt" []

pattern ConsStmt :: Term MITScript -> Term MITScript -> Term MITScript
pattern ConsStmt a b = Node "ConsStmt" [a, b]

pattern PLUS :: Term MITScript
pattern PLUS = Node "PLUS" []

pattern MINUS :: Term MITScript
pattern MINUS = Node "MINUS" []

pattern TIMES :: Term MITScript
pattern TIMES = Node "TIMES" []

pattern DIV :: Term MITScript
pattern DIV = Node "DIV" []

pattern AND :: Term MITScript
pattern AND = Node "AND" []

pattern OR :: Term MITScript
pattern OR = Node "OR" []

pattern GT :: Term MITScript
pattern GT = Node "GT" []

pattern GTE :: Term MITScript
pattern GTE = Node "GTE" []

pattern EQ :: Term MITScript
pattern EQ = Node "EQ" []

pattern UMINUS :: Term MITScript
pattern UMINUS = Node "UMINUS" []

pattern NOT :: Term MITScript
pattern NOT = Node "NOT" []

pattern BinExp :: Term MITScript -> Term MITScript -> Term MITScript -> Term MITScript
pattern BinExp a b c = Node "BinExp" [a, b, c]

pattern UnExp :: Term MITScript -> Term MITScript -> Term MITScript
pattern UnExp a b = Node "UnExp" [a, b]

pattern NumConst :: Term MITScript -> Term MITScript
pattern NumConst a = Val "NumConst" [a]

pattern BConst :: Term MITScript -> Term MITScript
pattern BConst a = Val "BConst" [a]

pattern None :: Term MITScript
pattern None = Val "None" []

pattern Str :: Term MITScript -> Term MITScript
pattern Str a = Val "Str" [a]

pattern Var :: Term MITScript -> Term MITScript
pattern Var a = Node "Var" [a]

pattern FunCall :: Term MITScript -> Term MITScript -> Term MITScript
pattern FunCall a b = Node "FunCall" [a, b]

pattern FunDecl :: Term MITScript -> Term MITScript -> Term MITScript
pattern FunDecl a b = Node "FunDecl" [a, b]

pattern Index :: Term MITScript -> Term MITScript -> Term MITScript
pattern Index a b = Node "Index" [a, b]

pattern FieldAccess :: Term MITScript -> Term MITScript -> Term MITScript
pattern FieldAccess a b = Node "FieldAccess" [a, b]

pattern Record :: Term MITScript -> Term MITScript
pattern Record a = Node "Record" [a]

pattern NilExp :: Term MITScript
pattern NilExp = Node "NilExp" []

pattern ConsExp :: Term MITScript -> Term MITScript -> Term MITScript
pattern ConsExp a b = Node "ConsExp" [a, b]

pattern RecordPair :: Term MITScript -> Term MITScript -> Term MITScript
pattern RecordPair a b = Node "RecordPair" [a, b]

pattern NilRecordPair :: Term MITScript
pattern NilRecordPair = Node "NilRecordPair" []

pattern ConsRecordPair :: Term MITScript -> Term MITScript -> Term MITScript
pattern ConsRecordPair a b = Node "ConsRecordPair" [a, b]

pattern True :: Term MITScript
pattern True = Val "True" []

pattern False :: Term MITScript
pattern False = Val "False" []

pattern ConstInt :: Integer -> Term MITScript
pattern ConstInt n = IntNode "ConstInt" n

pattern ConstStr :: InternedByteString -> Term MITScript
pattern ConstStr s = StrNode "ConstStr" s

--------------------------------------------------------------------------------------------------------------------
--- Runtime values

pattern ReducedRecord :: Term MITScript -> Term MITScript
pattern ReducedRecord a = Val "ReducedRecord" [a]

pattern ReducedRecordPair :: Term MITScript -> Term MITScript -> Term MITScript
pattern ReducedRecordPair a b = Val "ReducedRecordPair" [a, b]

pattern ReducedRecordNil :: Term MITScript
pattern ReducedRecordNil = Val "ReducedRecordNil" []

pattern ReducedRecordCons :: Term MITScript -> Term MITScript -> Term MITScript
pattern ReducedRecordCons a b = Val "ReducedRecordCons" [a, b]

pattern ReferenceVal :: Term MITScript -> Term MITScript
pattern ReferenceVal a = Val "ReferenceVal" [a]

pattern HeapAlloc :: Term MITScript -> Term MITScript
pattern HeapAlloc a = Node "HeapAlloc" [a]

pattern NilFrame :: Term MITScript
pattern NilFrame = Val "NilFrame" []

pattern ConsFrame :: Term MITScript -> Term MITScript -> Term MITScript
pattern ConsFrame a b = Val "ConsFrame" [a, b]

pattern Parent :: Term MITScript -> Term MITScript
pattern Parent p = Val "Parent" [p]

pattern Closure :: Term MITScript -> Term MITScript -> Term MITScript -> Term MITScript
pattern Closure params body frame = Val "Closure" [params, body, frame]

pattern ReducedNilExp :: Term MITScript
pattern ReducedNilExp = Val "ReducedNilExp" []

pattern ReducedConsExp :: Term MITScript -> Term MITScript -> Term MITScript
pattern ReducedConsExp a b = Val "ReducedConsExp" [a, b]

pattern HeapAddr :: Integer -> Term MITScript
pattern HeapAddr n = IntNode "HeapAddr" n

pattern Scope :: Term MITScript -> Term MITScript -> Term MITScript -> Term MITScript
pattern Scope body params args = Node "Scope" [body, params, args]

pattern Print :: Term MITScript
pattern Print = Node "Print" []

pattern IntCast :: Term MITScript
pattern IntCast = Node "IntCast" []

pattern Read :: Term MITScript
pattern Read = Node "Read" []

pattern Builtin :: Term MITScript -> Term MITScript -> Term MITScript
pattern Builtin name arg = Node "Builtin" [name, arg]

pattern GlobalVar :: Term MITScript
pattern GlobalVar = Val "GlobalVar" []

-}