{-# LANGUAGE FlexibleContexts, OverloadedStrings, PatternSynonyms #-}

module Languages.Imp.CfgGenHandwritten (
  makeExpCfg
) where


import CfgGenRuntime
import Graph
import Term

import           Languages.Imp.Imp hiding ( True, False )
import qualified Languages.Imp.Imp as Imp

-----------------------------------------

----------------------------------------------------------------
----------------------- Exp CFG --------------------------------
----------------------------------------------------------------

{--
 Difference between generated and handwritten:


Total differences: 3/24, not counting missing cases. Make that 4, assuming he’d implement Seq like he implemented ConsStmt, but he’d do ReadInt correctly.

1 extra node, 2 substantive

If: identical
While: Handwritten one incorrectly connects entry of condition to entry of body
(:=): Identical
+: Identical
<: Incorrectly connects input of second arg to final output, leaving output of final arg disconnected
VarExp: Handwritten one assigns node to the actual var
Write/WriteInt: Identical
ReadInt: Missing in handwritten
Seq: Mising in handwritten

Handwritten one also creates unreachable junk nodes

Handwritten one puts the base edges in const nodes, not val nodes.
 -}

makeExpCfg :: Term ImpLang -> Graph (GraphNode ImpLang)
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
genCfg :: Term ImpLang -> GraphGen ImpLang (GraphNodes ImpLang, GraphNodes ImpLang)
genCfg term = do
  case term of
    Node sym terms -> do
      (tIn, tOut) <- makeInOut term
      case sym of
        -- "Seq" case added by author, but based on ConsStmt for another language
        "Seq" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          connect tIn aIn
          connect aOut bIn
          connect bOut tOut
          return (tIn, tOut)

        "If" -> do
          (aIn, aOut) <- genCfg (terms !! 0)
          (bIn, bOut) <- genCfg (terms !! 1)
          (cIn, cOut) <- genCfg (terms !! 2)
          connect tIn aIn
          connect aOut bIn
          connect aOut cIn
          return (tIn, concat [bOut, cOut])

        "While" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          (cIn, cOut) <- genCfg (terms !! 1)
          connect tIn bIn
          connect bOut cIn
          connect bOut tOut
          connect cIn tIn
          return (tIn, tOut)

        ":=" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          (cIn, cOut) <- genCfg (terms !! 1)
          connect tIn cIn
          connect cIn tOut
          return (tIn, tOut)

        "+" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          (cIn, cOut) <- genCfg (terms !! 1)
          connect tIn bIn
          connect bOut cIn
          connect cIn tOut
          return (tIn, tOut)

        "<" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          (cIn, cOut) <- genCfg (terms !! 1)
          connect tIn bIn
          connect bOut cIn
          connect cIn tOut
          return (tIn, tOut)

        "VarExp" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          connect tIn bIn
          connect bOut tOut
          return (tIn, tOut)

        "Var" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          connect tIn bIn
          connect bOut tOut
          return (tIn, tOut)

        "WriteInt" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          connect tIn bIn
          connect bOut tOut
          return (tIn, tOut)

        "Write" -> do
          (bIn, bOut) <- genCfg (terms !! 0)
          connect tIn bIn
          connect bOut tOut
          return (tIn, tOut)
        _ -> return (tIn, tOut)

    Val     sym terms -> do
       (tIn, tOut) <- makeInOut term
       case sym of
         "EVal" -> do
           (bIn, bOut) <- genCfg (terms !! 0)
           connect tIn bIn
           connect bOut tOut
           return (tIn, tOut)
         _ -> return (tIn, tOut)

    _ -> do
       (tIn, tOut) <- makeInOut term
       connect tIn tOut
       return (tIn, tOut)
