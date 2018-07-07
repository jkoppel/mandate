{-# LANGUAGE DataKinds, EmptyDataDecls, OverloadedStrings, PatternSynonyms, TypeFamilies #-}

module Languages.Add (
    AddLang
  ) where

import Configuration
import Lang
import Matching
import Semantics
import Term
import Var

data AddLang

type instance RedState AddLang = EmptyState

addLangSig :: Signature AddLang
addLangSig = Signature [ NodeSig "+" ["Exp", "Exp"] "Exp"
                       , ValSig "EVal" ["Const"] "Exp"
                       , IntSig "Const" "Const"]

pattern Plus :: Term AddLang v -> Term AddLang v -> Term AddLang v
pattern Plus x y = Node "+" [x, y]

pattern EVal :: Term AddLang v -> Term AddLang v
pattern EVal n = Val "Val" [n]

pattern Const :: Integer -> Term AddLang v
pattern Const n = IntNode "Const" n

mv :: MetaVar -> Term AddLang Open
mv = MetaVar

conf :: Term AddLang v -> Configuration AddLang v
conf t = Conf t EmptyState

addLangRules :: IO (NamedRules AddLang)
addLangRules = sequence [

                   name "plus-cong-1" $
                   mkRule3 $ \e1 e2 e1' ->
                             let (me1, me2, me1') = (mv e1, mv e2, mv e1') in
                             StepTo (conf $ Plus me1 me2)
                               (LetStepTo (conf me1') (conf me1)
                               (Build $ conf $ Plus me1' me2))

                 , name "plus-cong-2" $
                   mkRule3 $ \v1 e2 e2' ->
                             let (mv1, me2, me2') = (mv v1, mv e2, mv e2') in
                             StepTo (conf $ Plus (EVal mv1) me2)
                               (LetStepTo (conf me2') (conf me2)
                               (Build $ conf $ Plus (EVal mv1) me2'))

                 , name "plus-eval" $
                   mkRule3 $ \v1 v2 v' ->
                             let (mv1, mv2, mv') = (mv v1, mv v2, mv v') in
                             StepTo (conf $ Plus (EVal mv1) (EVal mv2))
                               (LetComputation v' ([v1, v2], \[Const n1, Const n2] -> Const (n1+n2))
                               (Build $ conf $ EVal mv'))
               ]


instance Lang AddLang where
  signature = addLangSig
  rules = addLangRules

-----------

term1 :: Term AddLang Closed
term1 = Plus (Plus (EVal $ Const 1) (EVal $ Const 2)) (Plus (EVal $ Const 3) (EVal $ Const 4))