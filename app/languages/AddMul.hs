{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}

module Languages.AddMul (
    AddMulLang
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



data AddMulLang

instance LangBase AddMulLang where
  type RedState AddMulLang = EmptyState

  data CompFunc AddMulLang = RunAdd | AbsRunAdd | RunMul | AbsRunMul
    deriving ( Eq, Generic )

  compFuncName RunMul = "runMul"
  compFuncName RunAdd = "runAdd"

  runCompFunc RunAdd [EVal (Const n1), EVal (Const n2)] = return $ initConf $ EVal (Const (n1+n2))
  runCompFunc RunMul [EVal (Const n1), EVal (Const n2)] = return $ initConf $ EVal (Const (n1*n2))

  runCompFunc AbsRunAdd [GStar _, _] = return $ initConf ValStar
  runCompFunc AbsRunAdd [_, GStar _] = return $ initConf ValStar
  runCompFunc AbsRunMul [GStar _, _] = return $ initConf ValStar
  runCompFunc AbsRunMul [_, GStar _] = return $ initConf ValStar

instance ValueIrrelevance (CompFunc AddMulLang) where
  valueIrrelevance RunAdd    = AbsRunAdd
  valueIrrelevance AbsRunAdd = AbsRunAdd
  valueIrrelevance RunMul    = AbsRunMul
  valueIrrelevance AbsRunMul = AbsRunMul

instance Hashable (CompFunc AddMulLang)

instance Lang AddMulLang where
  signature = addMulLangSig
  rules = addMulLangRules

  initConf t = Conf t EmptyState

addMulLangSig :: Signature AddMulLang
addMulLangSig = Signature [ NodeSig "+" ["Exp", "Exp"] "Exp"
                       , NodeSig "*" ["Exp", "Exp"] "Exp"
                       , ValSig "EVal" ["Const"] "Exp"
                       , IntSig "Const" "Const"]

pattern Plus :: Term AddMulLang -> Term AddMulLang -> Term AddMulLang
pattern Plus x y = Node "+" [x, y]

pattern Times :: Term AddMulLang -> Term AddMulLang -> Term AddMulLang
pattern Times x y = Node "*" [x, y]

pattern EVal :: Term AddMulLang -> Term AddMulLang
pattern EVal n = Val "EVal" [n]

pattern Const :: Integer -> Term AddMulLang
pattern Const n = IntNode "Const" n

vv :: MetaVar -> Term AddMulLang
vv = ValVar

tv :: MetaVar -> Term AddMulLang
tv = NonvalVar

mv :: MetaVar -> Term AddMulLang
mv = MetaVar

conf :: Term AddMulLang -> Configuration AddMulLang
conf t = Conf t EmptyState

addMulLangRules :: IO (NamedRules AddMulLang)
addMulLangRules = sequence [

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

                 , name "times-cong-1" $
                   mkRule3 $ \e1 e2 e1' ->
                             let (te1, me2, me1') = (tv e1, mv e2, mv e1') in
                             StepTo (conf $ Times te1 me2)
                               (LetStepTo (conf me1') (conf te1)
                               (Build $ conf $ Times me1' me2))

                 , name "times-cong-2" $
                   mkRule3 $ \v1 e2 e2' ->
                             let (vv1, te2, me2') = (vv v1, tv e2, mv e2') in
                             StepTo (conf $ Times vv1 te2)
                               (LetStepTo (conf me2') (conf te2)
                               (Build $ conf $ Times vv1 me2'))

                 , name "times-eval" $
                   mkRule3 $ \v1 v2 v' ->
                             let (vv1, vv2, vv') = (vv v1, vv v2, vv v') in
                             StepTo (conf $ Times vv1 vv2)
                               (LetComputation (conf vv') (ExtComp RunMul [vv1, vv2])
                               (Build $ conf vv'))
               ]

-----------

term1 :: Term AddMulLang
term1 = Plus (Plus (EVal $ Const 1) (EVal $ Const 2)) (Times (EVal $ Const 3) (EVal $ Const 4))