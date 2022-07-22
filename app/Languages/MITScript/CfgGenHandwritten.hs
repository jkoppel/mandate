{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Languages.MITScript.CfgGenHandwritten (
  makeExpCfg
) where


import CfgGenRuntime
import Graph
import Term

import Languages.MITScript.Signature


-----------------------------------------


{--


MITScript:

Total: 15/59

4 substantive, 8 extra node,  2 deeper computation, 1 cosmetic

Generated one includes many dummy cases for nodes that don’t have computation, which will add nodes of degree 0 to the graph (or never get called). Handwritten does not

ConsRecordPair: Identical
MkLindex: Identical
BinExp: Handwritten incorrectly allows short-circuiting
FunCall: Identical
If: Handwritten one has extra join node at end of if
Var/LVar: Deeper computation difference
RecordPair: Incorrectly evaluates key
Assign: Generated one outputs “aOut->tOut” edge twice
MkLFieldAccess: Handwritten improperly evaluates field name
Builtin: Handwritten improperly evaluates builtin name
HeapAlloc: Identical
UnExp: Identical
Global: Handwritten improperly executes variable name
While: Handwritten does not allow for break, and connects end of body to beginning of condition instead of beginning of while
ConsStmt: Handwritten does not allow for break, creates extra node for end of cons
MkReturn: Identical
ConsExp: Identical
Record: Identical
Block: Handwritten one creates extra node for end of block
FieldAccess: Handwritten one incorrectly evaluates the field name
Index: In “a[b]”, handwritten one incorrectly evaluates a before b
FunDecl: Handwritten one incorrectly creates no edges, disconnecting everything after the fun decl

 -}

makeExpCfg :: Term MITScript -> Graph (GraphNode MITScript)
makeExpCfg t = withoutIsolatedNodes $ runGraphGen $ genCfg t

-- We have performed the following systematic renames:
-- ** n1     -> tIn
-- ** n2     -> tOut
-- ** entry0 -> aIn
-- ** exit0  -> aOut
-- ** entry1 -> bIn
-- ** exit1  -> bOut
-- ** entry2 -> cIn
-- ** exit2  -> cOut
--
-- After performing these find-and-replace renames, the programmer's variable names
-- almost match those of the auto-generated coder

genCfg :: Term MITScript -> GraphGen MITScript (GraphNodes MITScript, GraphNodes MITScript)
genCfg term =
  case term of
    Node sym terms -> do
      (tIn, tOut) <- makeInOut term
      case sym of
        "ConsStmt" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "ConsExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "ConsRecordPair" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "Global" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "Assign" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn bIn
          connect bOut aIn
          connect aOut tOut
          return (tIn, tOut)

        "ExpStmt" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "If" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          (cIn, cOut) <- genCfg (terms !! 2)
          connect tIn aIn
          connect aOut bIn
          connect aOut cIn
          connect bOut tOut
          connect cOut tOut
          return (tIn, tOut)

        "While" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut aIn
          connect bOut tOut
          return (tIn, tOut)

        "Block" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "MkReturn" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "BinExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          (cIn, cOut) <- genCfg (terms !! 2)
          connect tIn aIn
          connect aOut tOut
          connect aOut cIn
          connect cOut tOut
          -- connect cOut aOut possible? (for some operators)
          return (tIn, aOut ++ tOut ++ cOut)

        "UnExp" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn bIn
          connect bOut tOut
          return (tIn, tOut)

        "Var" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "LVar" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "FunCall" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "FunDecl" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          -- TODO: Required?
          --connect tIn aIn
          --connect aOut bIn
          --connect bOut tOut
          return (tIn, tOut)

        "Index" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "LIndex" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "MkLIndex" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "FieldAccess" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "LFieldAccess" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "MkLFieldAccess" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "HeapAlloc" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "Record" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          connect tIn aIn
          connect aOut tOut
          return (tIn, tOut)

        "Builtin" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "GlobalAssign" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "RecordPair" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        _ -> return (tIn, tOut)

    Val     sym terms -> do
       (tIn, tOut) <- makeInOut term
       connect tIn tOut
       return (tIn, tOut)

    _ -> do
       (tIn, tOut) <- makeInOut term
       connect tIn tOut
       return (tIn, tOut)
