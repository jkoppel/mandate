genCfg t@(Node "IntCast" []) = do (tIn, tOut) <- makeInOut t
                                  return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ConsRecordPair" [a, b]) = do (tIn, tOut) <- makeInOut t
                                             (aIn, aOut) <- genCfg a
                                             (bIn, bOut) <- genCfg b
                                             connect aOut bIn
                                             connect tIn aIn
                                             connect bOut tOut
                                             return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "GlobalAssign" [a, _]) = do (tIn, tOut) <- makeInOut t
                                           (aIn, aOut) <- genCfg a
                                           return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "MkLIndex" [_, _]) = do (tIn, tOut) <- makeInOut t
                                       connect tIn tOut
                                       return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "UMINUS" []) = do (tIn, tOut) <- makeInOut t
                                 return (inNodes [tIn], outNodes [tIn])

genCfg t@(Node "If" [_, a, b]) = do (tIn, tOut) <- makeInOut t
                                    (bIn, bOut) <- genCfg b
                                    (aIn, aOut) <- genCfg a
                                    connect tIn bIn
                                    connect tIn aIn
                                    return (inNodes [tIn], outNodes [bOut,aOut])
genCfg t@(Node "Read" []) = do (tIn, tOut) <- makeInOut t
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Var" [a]) = do (tIn, tOut) <- makeInOut t
                               (aIn, aOut) <- genCfg a
                               connect tIn tOut
                               return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "RecordPair" [a, _]) = do (tIn, tOut) <- makeInOut t
                                         (aIn, aOut) <- genCfg a
                                         connect tIn tOut
                                         return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Assign" [a, _]) = do (tIn, tOut) <- makeInOut t
                                     (aIn, aOut) <- genCfg a
                                     connect aOut tOut
                                     connect aOut tOut
                                     connect tIn aIn
                                     return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "MkLFieldAccess" [_, a]) = do (tIn, tOut) <- makeInOut t
                                             (aIn, aOut) <- genCfg a
                                             connect tIn tOut
                                             return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Builtin" [a, _]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      connect tIn tOut
                                      return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "HeapAlloc" [_]) = do (tIn, tOut) <- makeInOut t
                                     connect tIn tOut
                                     return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "NilExp" []) = do (tIn, tOut) <- makeInOut t
                                 connect tIn tOut
                                 return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Global" [a]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  connect tIn tOut
                                  return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "While" [_, a]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    connect tIn aIn
                                    connect tIn tOut
                                    connect aOut tOut
                                    connect aOut tIn
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "ExpStmt" [_]) = do (tIn, tOut) <- makeInOut t
                                   connect tIn tOut
                                   return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "ConsStmt" [a, b]) = do (tIn, tOut) <- makeInOut t
                                       (aIn, aOut) <- genCfg a
                                       (bIn, bOut) <- genCfg b
                                       connect tIn aIn
                                       connect aOut tOut
                                       connect aOut bIn
                                       return (inNodes [tIn], outNodes [tOut,bOut])
genCfg t@(Node "MkReturn" [_]) = do (tIn, tOut) <- makeInOut t
                                    connect tIn tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "ConsExp" [_, a]) = do (tIn, tOut) <- makeInOut t
                                      (aIn, aOut) <- genCfg a
                                      connect aOut tOut
                                      connect tIn aIn
                                      return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Record" [a]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  connect aOut tOut
                                  connect tIn aIn
                                  return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "NilRecordPair" []) = do (tIn, tOut) <- makeInOut t
                                        connect tIn tOut
                                        return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Block" [a]) = do (tIn, tOut) <- makeInOut t
                                 (aIn, aOut) <- genCfg a
                                 connect tIn aIn
                                 return (inNodes [tIn], outNodes [aOut])
genCfg t@(Node "FieldAccess" [_, a]) = do (tIn, tOut) <- makeInOut t
                                          (aIn, aOut) <- genCfg a
                                          connect tIn tOut
                                          return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Index" [_, _]) = do (tIn, tOut) <- makeInOut t
                                    connect tIn tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Print" []) = do (tIn, tOut) <- makeInOut t
                                return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "LVar" [a]) = do (tIn, tOut) <- makeInOut t
                                (aIn, aOut) <- genCfg a
                                connect tIn tOut
                                return (inNodes [tIn], outNodes [tOut])
genCfg t@(Val _ _) = do (a, b) <- makeInOut t
                        connect a b
                        return (a, b)
genCfg t@(StrNode _ _) = makeInOut t
genCfg t@(IntNode _ _) = makeInOut t