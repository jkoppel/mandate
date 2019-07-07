{-# LANGUAGE EmptyDataDecls, OverloadedStrings, PatternSynonyms #-}

module Languages.MITScript.Signature where

import Control.Exception

import Data.Interned.ByteString ( InternedByteString )

import Term

data MITScript

mitScriptSorts :: [Sort]
mitScriptSorts = [ "Name",  "NameList", "Stmt", "StmtList"
                , "BinOp", "UnOp" , "Builtin", "LVal", "Exp", "ExpList" , "ReducedExpList"
                , "RecordPair", "RecordPairList"
                , "ReducedRecordPair", "ReducedRecordPairList"
                , "Bool", "ConstInt", "ConstStr", "HeapAddr"
                , "Frame", "FrameList"
                ]


mitScriptSig :: Signature MITScript
mitScriptSig = Signature [ StrSig "Name" "Name"

                         , ValSig "NilName"  []                   "NameList"
                         , ValSig "ConsName" ["Name", "NameList"] "NameList"

                         , ValSig "NilFrame"  []                        "FrameList"
                         , ValSig "ConsFrame" ["HeapAddr", "FrameList"] "FrameList"

                         , ValSig  "NilStmt"  []                   "StmtList"
                         , NodeSig "ConsStmt" ["Stmt", "StmtList"] "StmtList"

                         , NodeSig "NilExp"  []                  "ExpList"
                         , NodeSig "ConsExp" ["Exp", "ExpList"]  "ExpList"

                         , ValSig "ReducedNilExp"  []                  "ReducedExpList"
                         , ValSig "ReducedConsExp" ["Exp", "ExpList"]  "ReducedExpList"

                         , NodeSig "NilRecordPair"  []                               "RecordPairList"
                         , NodeSig "ConsRecordPair" ["RecordPair", "RecordPairList"] "RecordPairList"

                         , ValSig "Parent"            ["Exp"]                                        "ReducedRecordPairList"
                         , ValSig "ReducedRecordNil"  []                                             "ReducedRecordPairList"
                         , ValSig "ReducedRecordCons" ["ReducedRecordPair", "ReducedRecordPairList"] "ReducedRecordPairList"

                         , NodeSig "Global"   ["Name"]                "Stmt"
                         , NodeSig "Assign"   ["LVal", "Exp"]         "Stmt"
                         , NodeSig "ExpStmt"  ["Exp"]                 "Stmt"
                         , NodeSig "If"       ["Exp", "Stmt", "Stmt"] "Stmt"
                         , NodeSig "While"    ["Exp", "Stmt"]         "Stmt"
                         , NodeSig "Block"    ["StmtList"]            "Stmt"
                         , NodeSig "MkReturn" ["Exp"]                 "Stmt"
                         , ValSig  "Return"   ["Exp"]                 "Stmt"

                         , NodeSig "PLUS"  [] "qfBinOp"
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

                         , NodeSig "Print"   [] "Builtin"
                         , NodeSig "IntCast" [] "Builtin"
                         , NodeSig "Read"    [] "Builtin"

                        -- The Expression Irrelevance abstraction level treats everyhting with type "Exp" as a star.
                        -- Sometimes, we dont want that. So we change the type to "Exp*"
                         , NodeSig "BinExp"         ["Exp", "BinOp", "Exp"]              "Exp"
                         , NodeSig "UnExp"          ["UnOp", "Exp"]                      "Exp"
                         , ValSig  "NumConst"       ["ConstInt"]                         "Exp"
                         , ValSig  "ReferenceVal"   ["HeapAddr"]                         "Exp*"
                         , ValSig  "BConst"         ["Bool"]                             "Exp"
                         , ValSig  "None"           []                                   "Exp"
                         , ValSig  "GlobalVar"      []                                   "Exp*"
                         , ValSig  "Str"            ["ConstStr"]                         "Exp"
                         , NodeSig "Var"            ["Name"]                             "Exp*"
                         , NodeSig "LVar"           ["Name"]                             "LVal"
                         , NodeSig "FunCall"        ["Exp", "ExpList"]                   "Exp"
                         , NodeSig "FunDecl"        ["NameList", "Stmt"]                 "Exp"
                         , NodeSig "Scope"          ["StmtList", "NameList", "ExpList"]  "Exp*"
                         , ValSig  "Closure"        ["NameList", "Stmt", "HeapAddr"]     "Exp*"
                         , NodeSig "Index"          ["Exp", "Exp"]                       "Exp*"
                         , ValSig  "LIndex"         ["Exp", "Exp"]                       "LVal"
                         , NodeSig "MkLIndex"       ["Exp", "Exp"]                       "LVal"
                         , NodeSig "FieldAccess"    ["Exp", "Name"]                      "Exp*"
                         , NodeSig "MkLFieldAccess" ["Exp", "Name"]                      "LVal"
                         , ValSig  "LFieldAccess"   ["Exp", "Name"]                      "LVal"
                         , NodeSig "HeapAlloc"      ["Exp"]                              "Exp*"
                         , NodeSig "Record"         ["RecordPairList"]                   "Exp*"
                         , ValSig  "ReducedRecord"  ["ReducedRecordPairList"]            "Exp"
                         , NodeSig "Builtin"        ["Builtin", "Exp"]                   "Exp*"

                         , ValSig  "ShouldGlobalAssign" ["LVal", "Exp"] "Stmt*"
                         , NodeSig "GlobalAssign"       ["LVal", "Exp"] "Stmt*"

                         , NodeSig "RecordPair"        ["Name", "Exp"] "RecordPair"
                         , ValSig  "ReducedRecordPair" ["Name", "Exp"] "ReducedRecordPair"

                         , ValSig "True"  [] "Bool"
                         , ValSig "False" [] "Bool"
                         , IntSig "ConstInt" "Exp"
                         , IntSig "HeapAddr" "HeapAddr"
                         , StrSig "ConstStr" "Exp"
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

pattern MkReturn :: Term MITScript -> Term MITScript
pattern MkReturn a = Node "MkReturn" [a]

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

pattern LVar :: Term MITScript -> Term MITScript
pattern LVar a = Node "LVar" [a]

pattern FunCall :: Term MITScript -> Term MITScript -> Term MITScript
pattern FunCall a b = Node "FunCall" [a, b]

pattern FunDecl :: Term MITScript -> Term MITScript -> Term MITScript
pattern FunDecl a b = Node "FunDecl" [a, b]

pattern Index :: Term MITScript -> Term MITScript -> Term MITScript
pattern Index a b = Node "Index" [a, b]

pattern LIndex :: Term MITScript -> Term MITScript -> Term MITScript
pattern LIndex a b = Val "LIndex" [a, b]

pattern MkLIndex :: Term MITScript -> Term MITScript -> Term MITScript
pattern MkLIndex a b = Node "MkLIndex" [a, b]

pattern FieldAccess :: Term MITScript -> Term MITScript -> Term MITScript
pattern FieldAccess a b = Node "FieldAccess" [a, b]

pattern LFieldAccess :: Term MITScript -> Term MITScript -> Term MITScript
pattern LFieldAccess a b = Val "LFieldAccess" [a, b]

pattern MkLFieldAccess :: Term MITScript -> Term MITScript -> Term MITScript
pattern MkLFieldAccess a b = Node "MkLFieldAccess" [a, b]

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

pattern ShouldGlobalAssign :: Term MITScript -> Term MITScript -> Term MITScript
pattern ShouldGlobalAssign a b = Val "ShouldGlobalAssign" [a, b]

pattern GlobalAssign :: Term MITScript -> Term MITScript -> Term MITScript
pattern GlobalAssign a b = Node "GlobalAssign" [a, b]

pattern GlobalVar :: Term MITScript
pattern GlobalVar = Val "GlobalVar" []