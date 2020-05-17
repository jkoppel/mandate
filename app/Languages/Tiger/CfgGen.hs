{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Languages.Tiger.CfgGen (
  makeExpCfg
) where


import CfgGenRuntime
import Graph
import Term

import Languages.Tiger.Signature


-----------------------------------------


makeExpCfg :: Term Tiger -> Graph (GraphNode Tiger)
makeExpCfg t = withoutIsolatedNodes $ runGraphGen $ genCfg t

----- Verbatim output of the following commands:
--
-- > :script ghci-scripts/run-tiger
-- > gs <- makeGraphPatterns (irrelevance ValueIrr) (irrSkippingFunScope ValueIrr) amRules signature
-- > graphPatternsToCode gs


genCfg t@(Node "LeOp" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "VarDecDec" [a, b, c]) = do (tIn, tOut) <- makeInOut t
                                           (aIn, aOut) <- genCfg a
                                           (bIn, bOut) <- genCfg b
                                           (cIn, cOut) <- genCfg c
                                           connect cOut tOut
                                           connect tIn cIn
                                           return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Ord" []) = do (tIn, tOut) <- makeInOut t
                              return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "WhileExp" [a, b]) = do (tIn, tOut) <- makeInOut t
                                       (aIn, aOut) <- genCfg a
                                       (bIn, bOut) <- genCfg b
                                       connect tIn aIn
                                       connect aOut bIn
                                       connect aOut tOut
                                       connect bOut tIn
                                       connect bOut tOut
                                       return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "PExp" [a]) = do (tIn, tOut) <- makeInOut t
                                (aIn, aOut) <- genCfg a
                                connect tIn aIn
                                return (inNodes [tIn], outNodes [aOut])
genCfg t@(Node "VarDec" [a]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "NilExpList" []) = do (tIn, tOut) <- makeInOut t
                                     connect tIn tOut
                                     return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "NeqOp" []) = do (tIn, tOut) <- makeInOut t
                                return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "MinusOp" []) = do (tIn, tOut) <- makeInOut t
                                  return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Formals" [a, b]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      (bIn, bOut) <- genCfg b
                                      return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "SimpleVar" [a]) = do (tIn, tOut) <- makeInOut t
                                     (aIn, aOut) <- genCfg a
                                     connect tIn tOut
                                     return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "TField" [a, b]) = do (tIn, tOut) <- makeInOut t
                                     (aIn, aOut) <- genCfg a
                                     (bIn, bOut) <- genCfg b
                                     return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ConsTypeDec" [a, b]) = do (tIn, tOut) <- makeInOut t
                                          (aIn, aOut) <- genCfg a
                                          (bIn, bOut) <- genCfg b
                                          return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Size" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "AssignExp" [a, b]) = do (tIn, tOut) <- makeInOut t
                                        (aIn, aOut) <- genCfg a
                                        (bIn, bOut) <- genCfg b
                                        connect aOut tOut
                                        connect tIn bIn
                                        connect bOut aIn
                                        return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Not" []) = do (tIn, tOut) <- makeInOut t
                              return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ForExp" [a, b, c, d]) = do (tIn, tOut) <- makeInOut t
                                           (bIn, bOut) <- genCfg b
                                           (cIn, cOut) <- genCfg c
                                           (dIn, dOut) <- genCfg d
                                           (aIn, aOut) <- genCfg a
                                           connect tIn bIn
                                           connect bOut cIn
                                           connect cOut tOut
                                           connect cOut dIn
                                           connect dOut tOut
                                           connect dOut cOut
                                           return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "LoopBody" [a, b]) = do (tIn, tOut) <- makeInOut t
                                       (aIn, aOut) <- genCfg a
                                       (bIn, bOut) <- genCfg b
                                       connect tIn aIn
                                       connect aOut tOut
                                       connect aOut bIn
                                       return (inNodes [tIn], outNodes [tOut,bOut])
genCfg t@(Node "Exit" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "IfExp" [a, b, c]) = do (tIn, tOut) <- makeInOut t
                                       (aIn, aOut) <- genCfg a
                                       (bIn, bOut) <- genCfg b
                                       (cIn, cOut) <- genCfg c
                                       connect aOut cIn
                                       connect aOut bIn
                                       connect tIn aIn
                                       return (inNodes [tIn], outNodes [cOut,bOut])
genCfg t@(Node "MkLFieldVar" [a, b]) = do (tIn, tOut) <- makeInOut t
                                          (aIn, aOut) <- genCfg a
                                          (bIn, bOut) <- genCfg b
                                          connect tIn aIn
                                          connect aOut tOut
                                          return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Builtin" [a, b]) = do (tIn, tOut) <- makeInOut t
                                      (bIn, bOut) <- genCfg b
                                      (aIn, aOut) <- genCfg a
                                      connect tIn bIn
                                      connect bOut tOut
                                      return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Seq" [a, b]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  (bIn, bOut) <- genCfg b
                                  connect tIn aIn
                                  connect aOut tOut
                                  connect aOut bIn
                                  return (inNodes [tIn], outNodes [bOut,tOut])
genCfg t@(Node "NilEField" []) = do (tIn, tOut) <- makeInOut t
                                    connect tIn tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "AssnFnArgs" [a, b]) = do (tIn, tOut) <- makeInOut t
                                         (aIn, aOut) <- genCfg a
                                         (bIn, bOut) <- genCfg b
                                         return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ConsExpList" [a, b]) = do (tIn, tOut) <- makeInOut t
                                          (aIn, aOut) <- genCfg a
                                          (bIn, bOut) <- genCfg b
                                          connect bOut tOut
                                          connect tIn aIn
                                          connect aOut bIn
                                          return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "HeapAlloc" [a]) = do (tIn, tOut) <- makeInOut t
                                     (aIn, aOut) <- genCfg a
                                     return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "TypeDec" [a, b]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      (bIn, bOut) <- genCfg b
                                      return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ConsTField" [a, b]) = do (tIn, tOut) <- makeInOut t
                                         (aIn, aOut) <- genCfg a
                                         (bIn, bOut) <- genCfg b
                                         return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "GetChar" []) = do (tIn, tOut) <- makeInOut t
                                  return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "GtOp" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "AppExp" [a, b]) = do (tIn, tOut) <- makeInOut t
                                     (aIn, aOut) <- genCfg a
                                     (bIn, bOut) <- genCfg b
                                     connect aOut tOut
                                     connect bOut aIn
                                     connect tIn bIn
                                     return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "VarExp" [a]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  connect tIn aIn
                                  return (inNodes [tIn], outNodes [aOut])
genCfg t@(Node "EqOp" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "NilTField" []) = do (tIn, tOut) <- makeInOut t
                                    return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Scope" [a]) = do (tIn, tOut) <- makeInOut t
                                 (aIn, aOut) <- genCfg a
                                 connect tIn aIn
                                 return (inNodes [tIn], outNodes [aOut])
genCfg t@(Node "FieldVar" [a, b]) = do (tIn, tOut) <- makeInOut t
                                       (aIn, aOut) <- genCfg a
                                       (bIn, bOut) <- genCfg b
                                       connect tIn aIn
                                       connect aOut tOut
                                       return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "JustSym" [a]) = do (tIn, tOut) <- makeInOut t
                                   (aIn, aOut) <- genCfg a
                                   return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "DivideOp" []) = do (tIn, tOut) <- makeInOut t
                                   return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ConsDecList" [a, b]) = do (tIn, tOut) <- makeInOut t
                                          (aIn, aOut) <- genCfg a
                                          (bIn, bOut) <- genCfg b
                                          connect tIn aIn
                                          connect aOut bIn
                                          return (inNodes [tIn], outNodes [bOut])
genCfg t@(Node "OrOp" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "MkLSubscriptVar" [a, b]) = do (tIn, tOut) <- makeInOut t
                                              (aIn, aOut) <- genCfg a
                                              (bIn, bOut) <- genCfg b
                                              connect tIn aIn
                                              connect bOut tOut
                                              connect aOut bIn
                                              return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "FunDec" [a, b, c, d]) = do (tIn, tOut) <- makeInOut t
                                           (aIn, aOut) <- genCfg a
                                           (bIn, bOut) <- genCfg b
                                           (cIn, cOut) <- genCfg c
                                           (dIn, dOut) <- genCfg d
                                           connect tIn tOut
                                           return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "TypeDecDec" [a]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      connect tIn tOut
                                      return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "ConsFunDec" [a, b]) = do (tIn, tOut) <- makeInOut t
                                         (aIn, aOut) <- genCfg a
                                         (bIn, bOut) <- genCfg b
                                         connect aOut bIn
                                         connect tIn aIn
                                         return (inNodes [tIn], outNodes [bOut])
genCfg t@(Node "LtOp" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "NameTy" [a]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "PDecs" [a]) = do (tIn, tOut) <- makeInOut t
                                 (aIn, aOut) <- genCfg a
                                 connect tIn aIn
                                 return (inNodes [tIn], outNodes [aOut])
genCfg t@(Node "NoneSym" []) = do (tIn, tOut) <- makeInOut t
                                  return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "NilFunDec" []) = do (tIn, tOut) <- makeInOut t
                                    connect tIn tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "LSimpleVar" [a]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      connect tIn tOut
                                      return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "RecordTy" [a]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Flush" []) = do (tIn, tOut) <- makeInOut t
                                return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "NilTypeDec" []) = do (tIn, tOut) <- makeInOut t
                                     return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Concat" []) = do (tIn, tOut) <- makeInOut t
                                 return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "PlusOp" []) = do (tIn, tOut) <- makeInOut t
                                 return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ArrayExp" [a, b, c]) = do (tIn, tOut) <- makeInOut t
                                          (aIn, aOut) <- genCfg a
                                          (bIn, bOut) <- genCfg b
                                          (cIn, cOut) <- genCfg c
                                          connect bOut cIn
                                          connect cOut tOut
                                          connect tIn bIn
                                          return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "GeOp" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Chr" []) = do (tIn, tOut) <- makeInOut t
                              return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "RecordExp" [a, b]) = do (tIn, tOut) <- makeInOut t
                                        (aIn, aOut) <- genCfg a
                                        (bIn, bOut) <- genCfg b
                                        connect tIn aIn
                                        connect aOut tOut
                                        return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "OpExp" [a, b, c]) = do (tIn, tOut) <- makeInOut t
                                       (aIn, aOut) <- genCfg a
                                       (bIn, bOut) <- genCfg b
                                       (cIn, cOut) <- genCfg c
                                       connect aOut tOut
                                       connect aOut cIn
                                       connect aOut cIn
                                       connect cOut aOut
                                       connect cOut tOut
                                       connect tIn aIn
                                       return (inNodes [tIn], outNodes [aOut,cOut,tOut])
genCfg t@(Node "AndOp" []) = do (tIn, tOut) <- makeInOut t
                                return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "EField" [a, b]) = do (tIn, tOut) <- makeInOut t
                                     (aIn, aOut) <- genCfg a
                                     (bIn, bOut) <- genCfg b
                                     connect bOut tOut
                                     connect tIn bIn
                                     return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "FunctionDec" [a]) = do (tIn, tOut) <- makeInOut t
                                       (aIn, aOut) <- genCfg a
                                       connect aOut tOut
                                       connect tIn aIn
                                       return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "ConsEField" [a, b]) = do (tIn, tOut) <- makeInOut t
                                         (aIn, aOut) <- genCfg a
                                         (bIn, bOut) <- genCfg b
                                         connect bOut tOut
                                         connect aOut bIn
                                         connect tIn aIn
                                         return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Print" []) = do (tIn, tOut) <- makeInOut t
                                return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "SubscriptVar" [a, b]) = do (tIn, tOut) <- makeInOut t
                                           (bIn, bOut) <- genCfg b
                                           (aIn, aOut) <- genCfg a
                                           connect bOut tOut
                                           connect tIn aIn
                                           connect aOut bIn
                                           return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "TimesOp" []) = do (tIn, tOut) <- makeInOut t
                                  return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "LetExp" [a, b]) = do (tIn, tOut) <- makeInOut t
                                     (aIn, aOut) <- genCfg a
                                     (bIn, bOut) <- genCfg b
                                     connect tIn aIn
                                     connect aOut bIn
                                     return (inNodes [tIn], outNodes [bOut])
genCfg t@(Node "ArrayTy" [a]) = do (tIn, tOut) <- makeInOut t
                                   (aIn, aOut) <- genCfg a
                                   return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "DoLet" [a, b]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    connect tIn aIn
                                    connect aOut bIn
                                    return (inNodes [tIn], outNodes [bOut])
genCfg t@(Node "Substring" []) = do (tIn, tOut) <- makeInOut t
                                    return (inNodes [tIn], outNodes [tIn])
genCfg t@(Val _ _) = do (a, b) <- makeInOut t
                        connect a b
                        return (a, b)
genCfg t@(StrNode _ _) = makeInOut t
genCfg t@(IntNode _ _) = makeInOut t