{-# LANGUAGE EmptyDataDecls, OverloadedStrings, PatternSynonyms #-}

module Add (
  ) where

import Semantics
import Term
import Var

data AddLang

addLangSig :: Signature AddLang
addLangSig = Signature [ NodeSig "+" ["Exp", "Exp"] "Exp"
                       , NodeSig "Val" ["Const"] "Exp"
                       , IntSig "Const" "Const"]

pattern Plus :: Node AddLang v -> Node AddLang v -> Node AddLang v
pattern Plus x y = Node "+" [x, y]

pattern Val :: Node AddLang v -> Node AddLang v
pattern Val n = Node "Val" [n]

pattern Const :: Int -> Node AddLang v
pattern Const n = IntNode "Const" n

mv :: MetaVar -> Term AddLang Open
mv = MetaVar

addLangRules :: Rules AddLang
addLangRules = [ let (e1, e2, e1') = (mv nextVar, mv nextVar, mv nextVar) in
                 StepTo (Plus e1 e2)
                          (LetStepTo e1' e1
                          (Build $ Plus e1' e2))

               , let (v1, e2, x) = (mv nextVar, mv nextVar, mv nextVar) in
                 StepTo (Plus (Val v1) e2)
                          (LetStepTo e2' e2
                          (Build $ Plus v1 e2'))

               , let (v1, v2, v') = (mv nextVar, mv nextVar, mv nextVar) in
                 StepTo (Plus (Val v1) (Val v2))
                          (LetComputation v' ([v1, v2], \[Const n1, Const n2] -> Const (n1+n2))
                          (Build $ Val (Const v')))


               ]