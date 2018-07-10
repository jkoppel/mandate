{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}

module Languages.MITScript.Signature (
    mitScriptSig
  ) where

import Control.Exception

import Term

data MITScript

sorts :: [Sort]
sorts = [ "Name",  "NameList", "Stmt", "StmtList"
        , "BinOp", "UnOp", "Expr", "ExprList"
        , "RecordPair", "RecordPairList"
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
             ]
