{-# LANGUAGE DataKinds, EmptyDataDecls, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.Imp (
    impLangSig
  , impLangRules
  ) where

import Data.Interned.ByteString ( InternedByteString(..) )

import Configuration
import Matching
import Semantics
import Term
import Var

data ImpLang

type instance RedState ImpLang = SimpEnv MetaVar (Term ImpLang)

impLangSig :: Signature ImpLang
impLangSig = Signature [ NodeSig ":=" ["Var", "Exp"] "Stmt"
                       , NodeSig "Seq" ["Stmt", "Stmt"] "Stmt"
                       , NodeSig "If" ["Exp", "Stmt", "Stmt"] "Stmt"
                       , NodeSig "While" ["Exp", "Stmt"] "Stmt"

                       , NodeSig "Var" ["VarName"] "Var"
                       , StrSig "VarName" "VarName"
                       , NodeSig "VarExp" ["Var"] "Exp"

                       , NodeSig "true" [] "Exp"
                       , NodeSig "false" [] "Exp"
                       , NodeSig "Val" ["Const"] "Exp"
                       , IntSig "Const" "Const"

                       , NodeSig "+" ["Exp", "Exp"] "Exp"
                       , NodeSig "<" ["Exp", "Exp"] "Exp"
                       ]

pattern (:=) :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern (:=) x y = Node ":=" [x, y]

pattern Seq :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern Seq x y = Node "Seq" [x, y]

pattern If :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern If x y z = Node "If" [x,y,z]

pattern While :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern While x y = Node "While" [x, y]

pattern Var :: Term ImpLang v -> Term ImpLang v
pattern Var x = Node "Var" [x]

pattern VarName :: InternedByteString -> Term ImpLang v
pattern VarName v = StrNode "VarName" v

pattern VarExp :: Term ImpLang v -> Term ImpLang v
pattern VarExp v = Node "VarExp" [v]

pattern True :: Term ImpLang v
pattern True = Node "true" []

pattern False :: Term ImpLang v
pattern False = Node "false" []

pattern Val :: Term ImpLang v -> Term ImpLang v
pattern Val n = Node "Val" [n]

pattern Const :: Integer -> Term ImpLang v
pattern Const n = IntNode "Const" n

pattern Plus :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern Plus x y = Node "+" [x, y]

pattern LT :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern LT x y = Node "<" [x, y]

conf :: Term ImpLang Open -> MetaVar -> Configuration ImpLang Open
conf t v = Conf t (WholeSimpEnv v)

mv :: MetaVar -> Term ImpLang Open
mv = MetaVar

impLangRules :: IO (Rules ImpLang)
impLangRules = sequence [
                   mkRule4 $ \e1 e2 e1' g ->
                             let (me1, me2, me1') = (mv e1, mv e2, mv e1') in
                             StepTo (conf (Plus me1 me2) g)
                               (LetStepTo (conf me1' g) (conf me1 g)
                               (Build $ conf (Plus me1' me2) g))

                 , mkRule4 $ \v1 e2 e2' g ->
                             let (mv1, me2, me2') = (mv v1, mv e2, mv e2') in
                             StepTo (conf (Plus (Val mv1) me2) g)
                               (LetStepTo (conf me2' g) (conf me2 g)
                               (Build $ conf (Plus (Val mv1) me2') g))

                 , mkRule4 $ \v1 v2 v' g ->
                             let (mv1, mv2, mv') = (mv v1, mv v2, mv v') in
                             StepTo (conf (Plus (Val mv1) (Val mv2)) g)
                               (LetComputation v' ([v1, v2], \[Const n1, Const n2] -> Const (n1+n2))
                               (Build $ conf (Val mv') g))

                ]