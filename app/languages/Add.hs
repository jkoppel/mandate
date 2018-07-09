{-# LANGUAGE DataKinds, EmptyCase, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}

module Languages.Add (
    AddLang
  ) where

import Configuration
import Lang
import Matching
import Semantics.PAM
import Semantics.SOS
import Term
import Var



data AddLang

instance LangBase AddLang where
  type RedState AddLang = EmptyState

  data CompFunc AddLang = RunAdd
    deriving ( Eq )

  compFuncName RunAdd = "runAdd"
  runCompFunc RunAdd [Const n1, Const n2] = return $ initConf $ Const (n1+n2)

instance Lang AddLang where
  signature = addLangSig
  rules = addLangRules

  initConf t = Conf t EmptyState

addLangSig :: Signature AddLang
addLangSig = Signature [ NodeSig "+" ["Exp", "Exp"] "Exp"
                       , ValSig "EVal" ["Const"] "Exp"
                       , IntSig "Const" "Const"]

pattern Plus :: Term AddLang -> Term AddLang -> Term AddLang
pattern Plus x y = Node "+" [x, y]

pattern EVal :: Term AddLang -> Term AddLang
pattern EVal n = Val "Val" [n]

pattern Const :: Integer -> Term AddLang
pattern Const n = IntNode "Const" n

mv :: MetaVar -> Term AddLang
mv = MetaVar

conf :: Term AddLang -> Configuration AddLang
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
                               (LetComputation (conf $ MetaVar v') (RunAdd, [mv1, mv2])
                               (Build $ conf $ EVal mv'))
               ]

-----------

term1 :: Term AddLang
term1 = Plus (Plus (EVal $ Const 1) (EVal $ Const 2)) (Plus (EVal $ Const 3) (EVal $ Const 4))