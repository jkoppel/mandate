{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Languages.Tiger.CfgGenHandwritten (
  makeExpCfg
) where


import CfgGenRuntime
import Graph
import Term

import Languages.Tiger.Signature


-----------------------------------------

{-
Differences between generated and handwritten:

Total: 38 differences vs. 81 connect statements

Treating all "disconnects graph" errors as missing cases:

4 deeper node difference, 10 extra node, 7 substantive, 16 missing cases, 1 cosmetic

Also, one difference which may be a bug in the generated (AssnFnArgs)

One difference from odd node identity in the generated (OpExp)

VarDecDec: Handwritten incorrectly evaluates var name
While: Handwritten can connect end of body to statement after loop in case of break. Handwritten incorrectly always enters loop. These may both be caused by a typo confusing exit0 and exit1.
PExp: Handwritten has extra node at end
VarDec: Handwritten incorrectly disconnects graph
Formals: Handwritten incorrectly disconnects graph
SimpleVar: Deeper node difference
TField: Handwritten incorrectly disconnects graph
ConsTypeDec: Handwritten improperly evaluates it (2x difference)
ForExp: Handwritten improperly lets loop exit after evaluating lower bound. Improperly forces loop to enter after upper bound. Improperly reevaluates bounds.
LoopBody: Identical
IfExp: Identical
MkLFieldVar: Handwritten improperly executes field name
Builtin: Missing case
Seq: Identical
NilEField: Missing case
AssnFnArgs: Generated improperly does nothing!!!!!!
ConsExpList: Identical
HeapAlloc: Generated oddly collapses this into one node, as if no computation. Makes sense though.
TypeDec: Handwritten improperly disconnects
ConsTField: Handwritten improperly disconnects
AppExp: Identical
VarExp: Handwritten creates extra node at end
Scope: Handwritten creates extra node at end
FieldVar: Identical
JustSym: Handwritten improperly disconnects
ConsDecList: Handwritten creates extra node at end
MkLSubscriptVar: Missing case
FunDec: Handwritten improperly evaluates body
TypeDecDec: Handwritten improperly disconnects
ConsFunDec: Handwritten creates extra node at end
NameTy: Handwritten improperly disconnects
PDecs: Identical
NilFunDec: Missing case
LSimpleVar: Deeper node problem
RecordTy: Handwritten improperly disconnects
ArrayExp: Identical
RecordExp: Handwritten improperly evaluates symbol
OpExp: Generated contains an extra cOut->aOut edge, possibly as a result of the and-0 rule.
EField: Handwritten improperly disconnects
FunctionDec: Identical
ConsEField: Handwritten improperly disconnects
SubscriptVar: Identical
LetExp: Handwritten improperly evaluates RHS before LHS, contains extra node at end
ArrayTy: Handwritten improperly disconnects
DoLet: Handwritten contains extra node at end

-}


makeExpCfg :: Term Tiger -> Graph (GraphNode Tiger)
makeExpCfg t = withoutIsolatedNodes $ runGraphGen $ genCfg t

----- Verbatim output of the following commands:
--
-- > :script ghci-scripts/run-tiger
-- > gs <- makeGraphPatterns (irrelevance ValueIrr) (irrSkippingFunScope ValueIrr) amRules signature
-- > graphPatternsToCode gs

-- We have performed the following systematic renames:
-- ** n1     -> tIn
-- ** n2     -> tOut
-- ** entry0 -> aIn
-- ** exit0  -> aOut
-- ** entry1 -> bIn
-- ** exit1  -> bOut
-- ** entry2 -> cIn
-- ** exit2  -> cOut
-- ** entry3 -> dIn
-- ** exit3  -> dOut
--
-- After performing these find-and-replace renames, the programmer's variable names
-- almost match those of the auto-generated coder

genCfg :: Term Tiger -> GraphGen Tiger (GraphNodes Tiger, GraphNodes Tiger)
genCfg term = do
  case term of
    Node sym terms -> do
      (tIn, tOut) <- makeInOut term
      case sym of
        "PExp" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          connect tIn bIn
          connect bOut tOut
          return (tIn, tOut)

        "PDecs" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          connect tIn bIn
          connect bOut tOut
          return (tIn, tOut)

        "IfExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          (cIn, cOut) <- genCfg (terms !! 2)
          connect tIn aIn
          connect aOut bIn
          connect aOut cIn
          return (tIn, bOut ++ cOut)

        "ForExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          (cIn, cOut) <- genCfg (terms !! 2)
          (dIn, dOut) <- genCfg (terms !! 3)
          connect tIn bIn
          connect bOut cIn
          connect cOut dIn
          connect dOut bIn
          connect dOut tOut
          return (tIn, bOut ++ tOut)

        "WhileExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut aIn
          connect bOut tOut
          return (tIn, bOut ++ tOut)

        "LoopBody" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect aOut tOut
          return (tIn, bOut ++ tOut)

        "SimpleVar" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "FieldVar" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "SubscriptVar" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "LSimpleVar" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "MkLFieldVar" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "VarExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "Seq" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect aOut tOut
          return (tIn, bOut ++ tOut)

        "AppExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn bIn
          connect bOut aIn
          connect aOut tOut
          return (tIn, tOut)

        "OpExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          (cIn, cOut) <- genCfg (terms !! 2)
          connect tIn aIn
          connect aOut tOut
          connect aOut cIn
          connect cOut tOut
          -- connect cOut aOut possible? (for some operators)
          return (tIn, aOut ++ tOut ++ cOut)

        "RecordExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aIn bIn
          connect bOut tOut
          return (tIn, tOut)

        "AssignExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn bIn
          connect bOut aIn
          connect aOut tOut
          return (tIn, tOut)

        "LetExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn bIn
          connect bOut aIn
          connect aOut tOut
          return (tIn, tOut)

        "ArrayExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          (cIn, cOut) <- genCfg (terms !! 2)
          connect tIn bIn
          connect bOut cIn
          connect cOut tOut
          return (tIn, tOut)

        "FunctionDec" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "VarDecDec" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          (cIn, cOut) <- genCfg (terms !! 2)
          connect tIn cIn
          connect cOut aIn
          connect aOut tOut
          return (tIn, tOut)

        "TypeDecDec" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          return (tIn, tOut)

        "NameTy" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          return (tIn, tOut)

        "RecordTy" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          return (tIn, tOut)

        "ArrayTy" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          return (tIn, tOut)

        "EField" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          return (tIn, tOut)

        "TField" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          return (tIn, tOut)

        "VarDec" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          return (tIn, tOut)

        "Formals" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          return (tIn, tOut)

        "TypeDec" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          return (tIn, tOut)

        "FunDec" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          (cIn, cOut) <- genCfg (terms !! 2)
          (dIn, dOut) <- genCfg (terms !! 3)
          connect tIn dIn
          connect dOut tOut
          return (tIn, tOut)

        "ConsDecList" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "ConsFunDec" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "ConsExpList" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "ConsTypeDec" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "ConsEField" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          return (tIn, tOut)

        "ConsTField" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          return (tIn, tOut)

        "JustSym" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          return (tIn, tOut)

        "HeapAlloc" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "Scope" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "DoLet" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "AssnFnArgs" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn bIn
          connect bOut aIn
          connect aOut tOut
          return (tIn, tOut)

        _ -> return (tIn, tOut)

    Val sym terms -> do
          (tIn, tOut) <- makeInOut term
          connect tIn tOut
          return (tIn, tOut)

    _ -> do
      (tIn, tOut) <- makeInOut term
      connect tIn tOut
      return (tIn, tOut)
