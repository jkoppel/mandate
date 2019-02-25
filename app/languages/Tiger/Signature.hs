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

             --- Runtime values
             , "Frame", "FrameList"
             , "RecordPair", "RecordPairList"
             , "ReducedRecordPair", "ReducedRecordPairList"
             , "HeapAddr"

             --- Builtins
             , "Builtin"
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
                     , NodeSig "VarDecDec"   ["VarDec", "SymOpt", "Exp"] "Dec"
                     , NodeSig "TypeDecDec"  ["TypeDecList"]             "Dec"

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
                     , NodeSig "GeOp"     [] "Oper"
                     , NodeSig "AndOp"    [] "Oper"
                     , NodeSig "OrOp"     [] "Oper"

                     , NodeSig "EField" ["Symbol", "Exp"] "EField"

                     , NodeSig "TField" ["Symbol", "Symbol"] "TField"

                     -- What is that "escape" boolean in their grammar
                     , NodeSig "VarDec" ["Symbol"] "VarDec"

                     , NodeSig "Formals" ["VarDec", "Symbol"] "Formals"

                     , NodeSig "TypeDec" ["Symbol", "Ty"] "TypeDec"

                     , NodeSig "FunDec" ["Symbol", "TFieldList", "SymOpt", "Exp"] "FunDec"

                     , NodeSig "NilDecList"  []                 "DecList"
                     , NodeSig "ConsDecList" ["Dec", "DecList"] "DecList"

                     , NodeSig "NilFunDec"  []                       "FunDecList"
                     , NodeSig "ConsFunDec" ["FunDec", "FunDecList"] "FunDecList"

                     , NodeSig "NilExpList"  []                 "ExpList"
                     , NodeSig "ConsExpList" ["Exp", "ExpList"] "ExpList"

                     , NodeSig "NilTypeDec"  []                         "TypeDecList"
                     , NodeSig "ConsTypeDec" ["TypeDec", "TypeDecList"] "TypeDecList"

                     , NodeSig "NilEField"  []                       "EFieldList"
                     , NodeSig "ConsEField" ["EField", "EFieldList"] "EFieldList"

                     , NodeSig "NilTField"  []                       "TFieldList"
                     , NodeSig "ConsTField" ["TField", "TFieldList"] "TFieldList"

                     , NodeSig "NoneSym" []         "SymOpt"
                     , NodeSig "JustSym" ["Symbol"] "SymOpt"

                     , StrSig "Symbol" "Symbol"
                     , StrSig "ConstStr" "ConstStr"
                     , IntSig "ConstInt" "ConstInt"


                     ----- Runtime values


                     , ValSig "NilFrame"  []                        "FrameList"
                     , ValSig "ConsFrame" ["HeapAddr", "FrameList"] "FrameList"

                     , ValSig "Parent"            ["Exp"]
                     , ValSig "ReducedRecordNil"  []                                             "ReducedRecordPairList"
                     , ValSig "ReducedRecordCons" ["ReducedRecordPair", "ReducedRecordPairList"] "ReducedRecordPairList"

                     , IntSig "HeapAddr" "HeapAddr"
                     , NodeSig "RecordPair"        ["Name", "Exp"] "RecordPair"
                     , ValSig  "ReducedRecordPair" ["Name", "Exp"] "ReducedRecordPair"

                     , ValSig "DoExit" ["ConstInt"] "Exp"

                     ----- Builtins
                     , NodeSig "Print"     [] "Builtin"
                     , NodeSig "Flush"     [] "Builtin"
                     , NodeSig "GetChar"   [] "Builtin"
                     , NodeSig "Ord"       [] "Builtin"
                     , NodeSig "Chr"       [] "Builtin"
                     , NodeSig "Size"      [] "Builtin"
                     , NodeSig "Substring" [] "Builtin"
                     , NodeSig "Concat"    [] "Builtin"
                     , NodeSig "Not"       [] "Builtin"
                     , NodeSig "Exit"      [] "Builtin"
                     ]

--------------------------------------------------------------------------------------------------------------------

-- Generated using "patSymForSigNode" in Term.hs; fixed by hand

pattern PExp :: Term Tiger -> Term Tiger
pattern PExp a = Node "PExp" [a]

pattern PDecs :: Term Tiger -> Term Tiger
pattern PDecs a = Node "PDecs" [a]

pattern SimpleVar :: Term Tiger -> Term Tiger
pattern SimpleVar a = Node "SimpleVar" [a]

pattern FieldVar :: Term Tiger -> Term Tiger -> Term Tiger
pattern FieldVar a b = Node "FieldVar" [a, b]

pattern SubscriptVar :: Term Tiger -> Term Tiger -> Term Tiger
pattern SubscriptVar a b = Node "SubscriptVar" [a, b]

pattern VarExp :: Term Tiger -> Term Tiger
pattern VarExp a = Node "VarExp" [a]

pattern NilExp :: Term Tiger
pattern NilExp = Val "NilExp" []

pattern IntExp :: Term Tiger -> Term Tiger
pattern IntExp a = Val "IntExp" [a]

pattern StringExp :: Term Tiger -> Term Tiger
pattern StringExp a = Val "StringExp" [a]

pattern SeqExp :: Term Tiger -> Term Tiger
pattern SeqExp a = Node "SeqExp" [a]

pattern AppExp :: Term Tiger -> Term Tiger -> Term Tiger
pattern AppExp a b = Node "AppExp" [a, b]

pattern OpExp :: Term Tiger -> Term Tiger -> Term Tiger -> Term Tiger
pattern OpExp a b c = Node "OpExp" [a, b, c]

pattern RecordExp :: Term Tiger -> Term Tiger -> Term Tiger
pattern RecordExp a b = Node "RecordExp" [a, b]

pattern AssignExp :: Term Tiger -> Term Tiger -> Term Tiger
pattern AssignExp a b = Node "AssignExp" [a, b]

pattern IfExp :: Term Tiger -> Term Tiger -> Term Tiger -> Term Tiger
pattern IfExp a b c = Node "IfExp" [a, b, c]

pattern WhileExp :: Term Tiger -> Term Tiger -> Term Tiger
pattern WhileExp a b = Node "WhileExp" [a, b]

pattern ForExp :: Term Tiger -> Term Tiger -> Term Tiger -> Term Tiger -> Term Tiger
pattern ForExp a b c d = Node "ForExp" [a, b, c, d]

pattern BreakExp :: Term Tiger
pattern BreakExp = Node "BreakExp" []

pattern LetExp :: Term Tiger -> Term Tiger -> Term Tiger
pattern LetExp a b = Node "LetExp" [a, b]

pattern ArrayExp :: Term Tiger -> Term Tiger -> Term Tiger -> Term Tiger
pattern ArrayExp a b c = Node "ArrayExp" [a, b, c]

pattern FunctionDec :: Term Tiger -> Term Tiger
pattern FunctionDec a = Node "FunctionDec" [a]

pattern VarDecDec :: Term Tiger -> Term Tiger -> Term Tiger -> Term Tiger
pattern VarDecDec a b c = Node "VarDecDec" [a, b, c]

pattern TypeDecDec :: Term Tiger -> Term Tiger
pattern TypeDecDec a = Node "TypeDecDec" [a]

pattern NameTy :: Term Tiger -> Term Tiger
pattern NameTy a = Node "NameTy" [a]

pattern RecordTy :: Term Tiger -> Term Tiger
pattern RecordTy a = Node "RecordTy" [a]

pattern ArrayTy :: Term Tiger -> Term Tiger
pattern ArrayTy a = Node "ArrayTy" [a]

pattern PlusOp :: Term Tiger
pattern PlusOp = Node "PlusOp" []

pattern MinusOp :: Term Tiger
pattern MinusOp = Node "MinusOp" []

pattern TimesOp :: Term Tiger
pattern TimesOp = Node "TimesOp" []

pattern DivideOp :: Term Tiger
pattern DivideOp = Node "DivideOp" []

pattern EqOp :: Term Tiger
pattern EqOp = Node "EqOp" []

pattern NeqOp :: Term Tiger
pattern NeqOp = Node "NeqOp" []

pattern LtOp :: Term Tiger
pattern LtOp = Node "LtOp" []

pattern LeOp :: Term Tiger
pattern LeOp = Node "LeOp" []

pattern GtOp :: Term Tiger
pattern GtOp = Node "GtOp" []

pattern GeOp :: Term Tiger
pattern GeOp = Node "GeOp" []

pattern AndOp :: Term Tiger
pattern AndOp = Node "AndOp" []

pattern OrOp :: Term Tiger
pattern OrOp = Node "OrOp" []

pattern EField :: Term Tiger -> Term Tiger -> Term Tiger
pattern EField a b = Node "EField" [a, b]

pattern TField :: Term Tiger -> Term Tiger -> Term Tiger
pattern TField a b = Node "TField" [a, b]

pattern VarDec :: Term Tiger -> Term Tiger
pattern VarDec a = Node "VarDec" [a]

pattern Formals :: Term Tiger -> Term Tiger -> Term Tiger
pattern Formals a b = Node "Formals" [a, b]

pattern TypeDec :: Term Tiger -> Term Tiger -> Term Tiger
pattern TypeDec a b = Node "TypeDec" [a, b]

pattern FunDec :: Term Tiger -> Term Tiger -> Term Tiger -> Term Tiger -> Term Tiger
pattern FunDec a b c d = Node "FunDec" [a, b, c, d]

pattern NilDecList :: Term Tiger
pattern NilDecList = Node "NilDecList" []

pattern ConsDecList :: Term Tiger -> Term Tiger -> Term Tiger
pattern ConsDecList a b = Node "ConsDecList" [a, b]

pattern NilFunDec :: Term Tiger
pattern NilFunDec = Node "NilFunDec" []

pattern ConsFunDec :: Term Tiger -> Term Tiger -> Term Tiger
pattern ConsFunDec a b = Node "ConsFunDec" [a, b]

pattern NilExpList :: Term Tiger
pattern NilExpList = Node "NilExpList" []

pattern ConsExpList :: Term Tiger -> Term Tiger -> Term Tiger
pattern ConsExpList a b = Node "ConsExpList" [a, b]

pattern NilTypeDec :: Term Tiger
pattern NilTypeDec = Node "NilTypeDec" []

pattern ConsTypeDec :: Term Tiger -> Term Tiger -> Term Tiger
pattern ConsTypeDec a b = Node "ConsTypeDec" [a, b]

pattern NilEField :: Term Tiger
pattern NilEField = Node "NilEField" []

pattern ConsEField :: Term Tiger -> Term Tiger -> Term Tiger
pattern ConsEField a b = Node "ConsEField" [a, b]

pattern NilTField :: Term Tiger
pattern NilTField = Node "NilTField" []

pattern ConsTField :: Term Tiger -> Term Tiger -> Term Tiger
pattern ConsTField a b = Node "ConsTField" [a, b]

pattern NoneSym :: Term Tiger
pattern NoneSym = Node "NoneSym" []

pattern JustSym :: Term Tiger -> Term Tiger
pattern JustSym a = Node "JustSym" [a]

pattern Symbol :: InternedByteString -> Term Tiger
pattern Symbol s = StrNode "Symbol" s

pattern ConstStr :: InternedByteString -> Term Tiger
pattern ConstStr s = StrNode "ConstStr" s

pattern ConstInt :: Integer -> Term Tiger
pattern ConstInt n = IntNode "ConstInt" n
