{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}

module Languages.Add (
    AddLang
  ) where

import GHC.Generics ( Generic )

import Data.Hashable ( Hashable )

import Configuration
import Lang
import Matching
import Semantics.Abstraction
import Semantics.General
import Semantics.PAM
import Semantics.SOS
import Term
import Var



data AddLang

instance LangBase AddLang where
  type RedState AddLang = EmptyState

  data CompFunc AddLang = RunAdd | AbsRunAdd
    deriving ( Eq, Generic )

  compFuncName RunAdd = "runAdd"

  runCompFunc RunAdd [EVal (Const n1), EVal (Const n2)] = return $ initConf $ EVal (Const (n1+n2))

  runCompFunc AbsRunAdd [GStar _, _] = return $ initConf ValStar
  runCompFunc AbsRunAdd [_, GStar _] = return $ initConf ValStar

instance ValueIrrelevance (CompFunc AddLang) where
  valueIrrelevance RunAdd    = AbsRunAdd
  valueIrrelevance AbsRunAdd = AbsRunAdd

instance Hashable (CompFunc AddLang)

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
pattern EVal n = Val "EVal" [n]

pattern Const :: Integer -> Term AddLang
pattern Const n = IntNode "Const" n

vv :: MetaVar -> Term AddLang
vv = ValVar

tv :: MetaVar -> Term AddLang
tv = NonvalVar

mv :: MetaVar -> Term AddLang
mv = MetaVar

conf :: Term AddLang -> Configuration AddLang
conf t = Conf t EmptyState

addLangRules :: IO (NamedRules AddLang)
addLangRules = sequence [

                   name "plus-cong-1" $
                   mkRule3 $ \e1 e2 e1' ->
                             let (te1, me2, me1') = (tv e1, mv e2, mv e1') in
                             StepTo (conf $ Plus te1 me2)
                               (LetStepTo (conf me1') (conf te1)
                               (Build $ conf $ Plus me1' me2))

                 , name "plus-cong-2" $
                   mkRule3 $ \v1 e2 e2' ->
                             let (vv1, te2, me2') = (vv v1, tv e2, mv e2') in
                             StepTo (conf $ Plus vv1 te2)
                               (LetStepTo (conf me2') (conf te2)
                               (Build $ conf $ Plus vv1 me2'))

                 , name "plus-eval" $
                   mkRule3 $ \v1 v2 v' ->
                             let (vv1, vv2, vv') = (vv v1, vv v2, vv v') in
                             StepTo (conf $ Plus vv1 vv2)
                               (LetComputation (conf vv') (ExtComp RunAdd [vv1, vv2])
                               (Build $ conf vv'))
               ]

-----------

term1 :: Term AddLang
term1 = Plus (Plus (EVal $ Const 1) (EVal $ Const 2)) (Plus (EVal $ Const 3) (EVal $ Const 4))