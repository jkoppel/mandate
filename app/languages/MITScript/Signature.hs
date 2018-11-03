{-# LANGUAGE EmptyDataDecls, OverloadedStrings, PatternSynonyms #-}

module Languages.MITScript.Signature where

import Control.Exception

import Data.Interned.ByteString ( InternedByteString )

import Term

data MITScript

mitScriptSorts :: [Sort]
mitScriptSorts = [ "Name",  "NameList", "Stmt", "StmtList"
                , "BinOp", "UnOp", "Expr", "ExprList"
                , "RecordPair", "RecordPairList"
                , "ReducedRecordPair", "ReducedRecordPairList"
                , "Bool", "ConstInt", "ConstStr"
                ]


-- TODO: More nodes needed to implement runtime values
mitScriptSig :: Signature MITScript
mitScriptSig = Signature [ StrSig "Name" "Name"

                         , ValSig "NilName" [] "NameList"
                         , ValSig "ConsName" ["Name", "NameList"] "NameList"

                         , NodeSig "Global"  ["Name"]                 "Stmt"
                         , NodeSig "Assign"  ["Expr", "Expr"]         "Stmt"
                         , NodeSig "ExpStmt" ["Expr"]                 "Stmt"
                         , NodeSig "If"      ["Expr", "Stmt", "Stmt"] "Stmt"
                         , NodeSig "While"   ["Expr", "Stmt"]         "Stmt"
                         , NodeSig "Return"  ["Expr"]                 "Stmt"
                         , NodeSig "Block"   ["StmtList"]             "Stmt"

                         , ValSig "NilStmt" [] "StmtList"
                         , NodeSig "ConsStmt" ["Stmt", "StmtList"] "StmtList"

                         , NodeSig "PLUS"  [] "BinOp"
                         , NodeSig "MINUS" [] "BinOp"
                         , NodeSig "TIMES" [] "BinOp"
                         , NodeSig "DIV"   [] "BinOp"
                         , NodeSig "AND"   [] "BinOp"
                         , NodeSig "OR"    [] "BinOp"
                         , NodeSig "GT"    [] "BinOp"
                         , NodeSig "GTE"   [] "BinOp"
                         , NodeSig "EQ"    [] "BinOp"

                         , NodeSig "UMINUS" [] "UnOp"
                         , NodeSig "NOT"    [] "UnOp"

                         , NodeSig "BinExp"      ["Expr", "BinOp", "Expr"] "Expr"
                         , NodeSig "UnExp"       ["UnOp", "Expr"]          "Expr"
                         , ValSig  "NumConst"    ["ConstInt"]              "Expr"
                         , ValSig  "BConst"      ["Bool"]                  "Expr"
                         , ValSig  "None"        []                        "Expr"
                         , ValSig  "Str"         ["ConstStr"]              "Expr"
                         , NodeSig "Var"         ["Name"]                  "Expr"
                         , NodeSig "FunCall"     ["Expr", "ExprList"]      "Expr"
                         , NodeSig "FunDecl"     ["NameList", "Stmt"]      "Expr"
                         , NodeSig "Index"       ["Expr", "Expr"]          "Expr"
                         , NodeSig "FieldAccess" ["Expr", "Name"]          "Expr"
                         , NodeSig "Record"      ["RecordPairList"]        "Expr"

                         , NodeSig "NilExpr" [] "ExprList"
                         , NodeSig "ConsExpr" ["Expr", "ExprList"] "ExprList"

                         , NodeSig "RecordPair" ["Name", "Expr"] "RecordPair"

                         , NodeSig "NilRecordPair" [] "RecordPairList"
                         , NodeSig "ConsRecordPair" ["RecordPair", "RecordPairList"] "RecordPairList"

                         , ValSig "True" [] "Bool"
                         , ValSig "False" [] "Bool"
                         , IntSig "ConstInt" "ConstInt"
                         , StrSig "ConstStr" "ConstStr"

                         , ValSig "ReducedRecord" ["ReducedRecordPairList"] "Expr"
                         , ValSig "ReducedRecordPair" ["Name", "Expr"] "ReducedRecordPair"

                         , ValSig "ReducedRecordNil" [] "ReducedRecordPairList"
                         , ValSig "ReducedRecordCons" ["ReducedRecordPair", "ReducedRecordPairList"] "ReducedRecordPairList"
                         , NodeSig "HeapAlloc"   ["Expr"] "Expr"
                         , NodeSig "ReferenceVal"  ["Expr"] "Expr"
             ]

--------------------------------------------------------------------------------------------------------------------

-- Generated using "patSymForSigNode" in Term.hs; fixed by hand


pattern Name :: InternedByteString ->Term MITScript
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
pattern Return a = Node "Return" [a]

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

pattern NilExpr :: Term MITScript
pattern NilExpr = Node "NilExpr" []

pattern ConsExpr :: Term MITScript -> Term MITScript -> Term MITScript
pattern ConsExpr a b = Node "ConsExpr" [a, b]

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

-- Runtime records: like record literals, but values must be fully evaluated
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