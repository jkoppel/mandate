{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Languages.MITScript.CfgGen (
  makeExpCfg
) where


import CfgGenRuntime
import Graph
import Term

import Languages.MITScript.Signature


-----------------------------------------


makeExpCfg :: Term MITScript -> Graph (GraphNode MITScript)
makeExpCfg t = withoutIsolatedNodes $ runGraphGen $ genCfg t

----- Verbatim output of the following commands:
--
-- > :script ghci-scripts/run-mitscript
-- > gs <- makeGraphPatterns (irrelevance ValueIrr) (irrSkippingScope ValueIrr) amRules signature
-- > graphPatternsToCode gs

genCfg t@(Node "IntCast" []) = do (tIn, tOut) <- makeInOut t
                                  return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ConsRecordPair" [a, b]) = do (tIn, tOut) <- makeInOut t
                                             (bIn, bOut) <- genCfg b
                                             (aIn, aOut) <- genCfg a
                                             connect tIn aIn
                                             connect aOut bIn
                                             connect bOut tOut
                                             return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "GlobalAssign" [a, b]) = do (tIn, tOut) <- makeInOut t
                                           (aIn, aOut) <- genCfg a
                                           (bIn, bOut) <- genCfg b
                                           return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "MkLIndex" [a, b]) = do (tIn, tOut) <- makeInOut t
                                       (aIn, aOut) <- genCfg a
                                       (bIn, bOut) <- genCfg b
                                       connect bOut tOut
                                       connect aOut bIn
                                       connect tIn aIn
                                       return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "UMINUS" []) = do (tIn, tOut) <- makeInOut t
                                 return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "GTE" []) = do (tIn, tOut) <- makeInOut t
                              return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "BinExp" [a, b, c]) = do (tIn, tOut) <- makeInOut t
                                        (aIn, aOut) <- genCfg a
                                        (bIn, bOut) <- genCfg b
                                        (cIn, cOut) <- genCfg c
                                        connect cOut tOut
                                        connect aOut cIn
                                        connect tIn aIn
                                        return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "FunCall" [a, b]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      (bIn, bOut) <- genCfg b
                                      connect bOut tOut
                                      connect aOut bIn
                                      connect tIn aIn
                                      return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "If" [a, b, c]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    (cIn, cOut) <- genCfg c
                                    connect tIn aIn
                                    connect aOut cIn
                                    connect aOut bIn
                                    return (inNodes [tIn], outNodes [bOut,cOut])
genCfg t@(Node "Read" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Var" [a]) = do (tIn, tOut) <- makeInOut t
                               (aIn, aOut) <- genCfg a
                               connect tIn tOut
                               return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "AND" []) = do (tIn, tOut) <- makeInOut t
                              return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "LVar" [a]) = do (tIn, tOut) <- makeInOut t
                                (aIn, aOut) <- genCfg a
                                connect tIn tOut
                                return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "RecordPair" [a, b]) = do (tIn, tOut) <- makeInOut t
                                         (aIn, aOut) <- genCfg a
                                         (bIn, bOut) <- genCfg b
                                         connect tIn bIn
                                         connect bOut tOut
                                         return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Assign" [a, b]) = do (tIn, tOut) <- makeInOut t
                                     (aIn, aOut) <- genCfg a
                                     (bIn, bOut) <- genCfg b
                                     connect bOut aIn
                                     connect tIn bIn
                                     connect aOut tOut
                                     connect aOut tOut
                                     return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "MkLFieldAccess" [a, b]) = do (tIn, tOut) <- makeInOut t
                                             (bIn, bOut) <- genCfg b
                                             (aIn, aOut) <- genCfg a
                                             connect tIn aIn
                                             connect aOut tOut
                                             return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Builtin" [a, b]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      (bIn, bOut) <- genCfg b
                                      connect bOut tOut
                                      connect tIn bIn
                                      return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "TIMES" []) = do (tIn, tOut) <- makeInOut t
                                return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "OR" []) = do (tIn, tOut) <- makeInOut t
                             return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "HeapAlloc" [a]) = do (tIn, tOut) <- makeInOut t
                                     (aIn, aOut) <- genCfg a
                                     connect aOut tOut
                                     connect tIn aIn
                                     return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "NilExp" []) = do (tIn, tOut) <- makeInOut t
                                 connect tIn tOut
                                 return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "UnExp" [a, b]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    connect tIn bIn
                                    connect bOut tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Global" [a]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  connect tIn tOut
                                  return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "While" [a, b]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    connect bOut tIn
                                    connect bOut tOut
                                    connect tIn aIn
                                    connect aOut tOut
                                    connect aOut bIn
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "ExpStmt" [a]) = do (tIn, tOut) <- makeInOut t
                                   (aIn, aOut) <- genCfg a
                                   connect tIn aIn
                                   connect aOut tOut
                                   return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "GT" []) = do (tIn, tOut) <- makeInOut t
                             return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "DIV" []) = do (tIn, tOut) <- makeInOut t
                              return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ConsStmt" [a, b]) = do (tIn, tOut) <- makeInOut t
                                       (aIn, aOut) <- genCfg a
                                       (bIn, bOut) <- genCfg b
                                       connect tIn aIn
                                       connect aOut tOut
                                       connect aOut bIn
                                       return (inNodes [tIn], outNodes [tOut,bOut])
genCfg t@(Node "MkReturn" [a]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    connect aOut tOut
                                    connect tIn aIn
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "MINUS" []) = do (tIn, tOut) <- makeInOut t
                                return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ConsExp" [a, b]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      (bIn, bOut) <- genCfg b
                                      connect tIn aIn
                                      connect aOut bIn
                                      connect bOut tOut
                                      return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "NOT" []) = do (tIn, tOut) <- makeInOut t
                              return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Record" [a]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  connect aOut tOut
                                  connect tIn aIn
                                  return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "PLUS" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "NilRecordPair" []) = do (tIn, tOut) <- makeInOut t
                                        connect tIn tOut
                                        return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "EQ" []) = do (tIn, tOut) <- makeInOut t
                             return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Block" [a]) = do (tIn, tOut) <- makeInOut t
                                 (aIn, aOut) <- genCfg a
                                 connect tIn aIn
                                 return (inNodes [tIn], outNodes [aOut])
genCfg t@(Node "FieldAccess" [a, b]) = do (tIn, tOut) <- makeInOut t
                                          (aIn, aOut) <- genCfg a
                                          (bIn, bOut) <- genCfg b
                                          connect tIn aIn
                                          connect aOut tOut
                                          return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Index" [a, b]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    connect aOut tOut
                                    connect bOut aIn
                                    connect tIn bIn
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Print" []) = do (tIn, tOut) <- makeInOut t
                                return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "FunDecl" [a, b]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      (bIn, bOut) <- genCfg b
                                      connect tIn tOut
                                      return (inNodes [tIn], outNodes [tOut])
genCfg t@(Val _ _) = do (a, b) <- makeInOut t
                        connect a b
                        return (a, b)
genCfg t@(StrNode _ _) = makeInOut t
genCfg t@(IntNode _ _) = makeInOut t