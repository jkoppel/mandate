genCfg t@(Node "Write" [a]) = do (tIn, tOut) <- makeInOut t
                                 (aIn, aOut) <- genCfg a
                                 connect tIn aIn
                                 connect aOut tOut
                                 return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "WriteInt" [a]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    connect tIn aIn
                                    connect aOut tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "If" [a, b, c]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    (cIn, cOut) <- genCfg c
                                    connect tIn aIn
                                    connect aOut cIn
                                    connect aOut bIn
                                    return (inNodes [tIn], outNodes [cOut,bOut])
genCfg t@(Node ":=" [a, b]) = do (tIn, tOut) <- makeInOut t
                                 (aIn, aOut) <- genCfg a
                                 (bIn, bOut) <- genCfg b
                                 connect bOut tOut
                                 connect tIn bIn
                                 return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Var" [a]) = do (tIn, tOut) <- makeInOut t
                               (aIn, aOut) <- genCfg a
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ReadInt" []) = do (tIn, tOut) <- makeInOut t
                                  connect tIn tOut
                                  return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Seq" [a, b]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  (bIn, bOut) <- genCfg b
                                  connect tIn aIn
                                  connect aOut bIn
                                  return (inNodes [tIn], outNodes [bOut])
genCfg t@(Node "VarExp" [a]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  connect tIn tOut
                                  return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "+" [a, b]) = do (tIn, tOut) <- makeInOut t
                                (aIn, aOut) <- genCfg a
                                (bIn, bOut) <- genCfg b
                                connect aOut bIn
                                connect bOut tOut
                                connect tIn aIn
                                return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "<" [a, b]) = do (tIn, tOut) <- makeInOut t
                                (aIn, aOut) <- genCfg a
                                (bIn, bOut) <- genCfg b
                                connect aOut bIn
                                connect tIn aIn
                                connect bOut tOut
                                return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "While" [a, b]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    connect bOut tIn
                                    connect tIn aIn
                                    connect aOut bIn
                                    connect aOut tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Val _ _) = do (a, b) <- makeInOut t
                        connect a b
                        return (a, b)
genCfg t@(StrNode _ _) = makeInOut t
genCfg t@(IntNode _ _) = makeInOut t