genCfg t@(Node "Write" [_]) = do (tIn, tOut) <- makeInOut t
                                 connect tIn tOut
                                 return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "WriteInt" [_]) = do (tIn, tOut) <- makeInOut t
                                    connect tIn tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "If" [_, a, b]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    connect tIn bIn
                                    connect tIn aIn
                                    return (inNodes [tIn], outNodes [bOut,aOut])
genCfg t@(Node ":=" [a, _]) = do (tIn, tOut) <- makeInOut t
                                 (aIn, aOut) <- genCfg a
                                 connect tIn tOut
                                 return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Var" [a]) = do (tIn, tOut) <- makeInOut t
                               (aIn, aOut) <- genCfg a
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "Seq" [a, b]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  (bIn, bOut) <- genCfg b
                                  connect tIn aIn
                                  connect aOut bIn
                                  return (inNodes [tIn], outNodes [bOut])
genCfg t@(Node "While" [_, a]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    connect aOut tIn
                                    connect tIn aIn
                                    connect tIn tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Val _ _) = do (a, b) <- makeInOut t
                        connect a b
                        return (a, b)
genCfg t@(StrNode _ _) = makeInOut t
genCfg t@(IntNode _ _) = makeInOut t                        