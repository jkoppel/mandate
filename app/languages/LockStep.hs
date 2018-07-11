{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}

module Languages.Lockstep (
    LockstepLang
  ) where

import GHC.Generics ( Generic )

import Data.Hashable ( Hashable )

import Configuration
import Lang
import Matching
import Semantics.General
import Semantics.PAM
import Semantics.SOS
import Term
import Var



data LockstepLang

instance LangBase LockstepLang where
  type RedState LockstepLang = EmptyState

  data CompFunc LockstepLang = RunAdd
    deriving ( Eq, Generic )

  compFuncName RunAdd = "runAdd"
  runCompFunc RunAdd [Const n1, Const n2] = return $ initConf $ Const (n1+n2)

instance Hashable (CompFunc LockstepLang)

instance Lang LockstepLang where
  signature = lockstepSig
  rules = lockstepRules

  initConf t = Conf t EmptyState

lockstepSig :: Signature LockstepLang
lockstepSig = Signature [ NodeSig "LockstepComp" ["Exp", "Exp"] "Exp"
                        , NodeSig "+" ["Exp", "Exp"] "Exp"
                        , ValSig "EVal" ["Const"] "Exp"
                        , IntSig "Const" "Const"
                        ]

pattern Lockstep :: Term LockstepLang -> Term LockstepLang -> Term LockstepLang
pattern Lockstep x y = Node "LockstepComp" [x, y]


pattern Plus :: Term LockstepLang -> Term LockstepLang -> Term LockstepLang
pattern Plus x y = Node "+" [x, y]

pattern EVal :: Term LockstepLang -> Term LockstepLang
pattern EVal n = Val "Val" [n]

pattern Const :: Integer -> Term LockstepLang
pattern Const n = IntNode "Const" n


mv :: MetaVar -> Term LockstepLang
mv = MetaVar

conf :: Term LockstepLang -> Configuration LockstepLang
conf t = Conf t EmptyState

int :: Integer -> Term LockstepLang
int n = EVal (Const n)

lockstepRules :: IO (NamedRules LockstepLang)
lockstepRules = sequence [
                    name "lockstep" $
                    mkRule4 $ \e1 e2 e1' e2' ->
                              let (me1, me2, me1', me2') = (mv e1, mv e2, mv e1', mv e2') in
                              StepTo (conf $ Lockstep me1 me2)
                                (LetStepTo (conf me1') (conf me1)
                                (LetStepTo (conf me2') (conf me2)
                                (Build $ conf (Lockstep me1' me2'))))

                ,   name "plus-cong-1" $
                    mkRule3 $ \e1 e2 e1' ->
                              let (me1, me2, me1') = (mv e1, mv e2, mv e1') in
                              StepTo (conf $ Plus me1 me2)
                                (LetStepTo (conf me1') (conf me1)
                                (Build $ conf $ Plus me1' me2))

                ,   name "plus-cong-2" $
                    mkRule3 $ \v1 e2 e2' ->
                              let (mv1, me2, me2') = (mv v1, mv e2, mv e2') in
                              StepTo (conf $ Plus (EVal mv1) me2)
                                (LetStepTo (conf me2') (conf me2)
                                (Build $ conf $ Plus (EVal mv1) me2'))

                ,   name "plus-eval" $
                    mkRule3 $ \v1 v2 v' ->
                              let (mv1, mv2, mv') = (mv v1, mv v2, mv v') in
                              StepTo (conf $ Plus (EVal mv1) (EVal mv2))
                                (LetComputation (conf $ MetaVar v') (ExtComp RunAdd [mv1, mv2])
                                (Build $ conf $ EVal mv'))
                ]

-----------

term1 :: Term LockstepLang
term1 = (int 1 `Plus` int 2) `Lockstep` (int 3 `Plus` int 4)
