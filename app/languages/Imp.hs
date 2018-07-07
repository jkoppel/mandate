{-# LANGUAGE DataKinds, EmptyDataDecls, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.Imp (
    impLangSig
  , impLangRules
  ) where

import Prelude hiding ( True, False, LT )

import Data.Interned.ByteString ( InternedByteString(..) )

import Configuration
import Matching
import Semantics
import Term
import Var

data ImpLang

-- Why are the keys of this map terms?
-- Because my infrastructure, namely ExtFunc's, currently assumes metavars are only bound to
-- terms (and maps), so it must be a term.
type instance RedState ImpLang = SimpEnv (Term ImpLang) (Term ImpLang)

impLangSig :: Signature ImpLang
impLangSig = Signature [ NodeSig ":=" ["Var", "Exp"] "Stmt"
                       , ValSig "Skip" [] "Stmt"
                       , NodeSig "Seq" ["Stmt", "Stmt"] "Stmt"
                       , NodeSig "If" ["Exp", "Stmt", "Stmt"] "Stmt"
                       , NodeSig "While" ["Exp", "Stmt"] "Stmt"

                       , NodeSig "Var" ["VarName"] "Var"
                       , StrSig "VarName" "VarName"
                       , NodeSig "VarExp" ["Var"] "Exp"

                       , ValSig "true" [] "Exp"
                       , ValSig "false" [] "Exp"
                       , ValSig "EVal" ["Const"] "Exp"
                       , IntSig "Const" "Const"

                       , NodeSig "+" ["Exp", "Exp"] "Exp"
                       , NodeSig "<" ["Exp", "Exp"] "Exp"
                       ]

pattern Assign :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern Assign x y = Node ":=" [x, y]

pattern Skip :: Term ImpLang v
pattern Skip = Val "Skip" []

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
pattern True = Val "true" []

pattern False :: Term ImpLang v
pattern False = Val "false" []

pattern EVal :: Term ImpLang v -> Term ImpLang v
pattern EVal n = Node "Val" [n]

pattern Const :: Integer -> Term ImpLang v
pattern Const n = IntNode "Const" n

pattern Plus :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern Plus x y = Node "+" [x, y]

pattern LT :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern LT x y = Node "<" [x, y]

varExp :: InternedByteString -> Term ImpLang v
varExp s = VarExp $ Var $ VarName s

pattern (:=) :: InternedByteString -> Term ImpLang v -> Term ImpLang v
pattern (:=) var t = Assign (Var (VarName var)) t

pattern (:<) :: Term ImpLang v -> Term ImpLang v -> Term ImpLang v
pattern (:<) l r = LT l r

intConst :: Integer -> Term ImpLang v
intConst n = EVal $ Const n

conf :: Term ImpLang Open -> MetaVar -> Configuration ImpLang Open
conf t v = Conf t (WholeSimpEnv v)

mv :: MetaVar -> Term ImpLang Open
mv = MetaVar

-- NOTE: Current rules assume that store does not change
-- when evaluating exps. This is currently true.

impLangRules :: IO (NamedRules ImpLang)
impLangRules = sequence [

                   name "assn-cong" $
                   mkRule5 $ \var e e' mu mu' ->
                             let (mvar, me, me') = (mv var, mv e, mv e') in
                             StepTo (conf (Assign mvar me) mu)
                               (LetStepTo (conf me' mu') (conf me mu)
                               (Build $ conf (Assign mvar me') mu'))

                 , name "assn-eval" $
                   mkRule3 $ \var val mu ->
                             let (mvar, mval) = (mv var, mv val) in
                             StepTo (conf (Assign (Var mvar) (EVal mval)) mu)
                               (Build $ Conf Skip (AssocOneVal mu mvar mval))

                 ----------------------------------------------------------------------------

                 , name "seq-cong" $
                   mkRule5 $ \s1 s2 s1' mu mu' ->
                             let (ms1, ms2, ms1') = (mv s1, mv s2, mv s1') in
                             StepTo (conf (Seq ms1 ms2) mu)
                               (LetStepTo (conf ms1' mu') (conf ms1 mu)
                               (Build (conf (Seq ms1' ms2) mu')))

                 , name "seq-eval" $
                   mkRule2 $ \s mu ->
                             let ms = mv s in
                             StepTo (conf (Seq Skip ms) mu)
                               (Build $ conf ms mu)

                 , name "if-cong" $
                   mkRule6 $ \e e' s t mu mu' ->
                             let (me, me', ms, mt) = (mv e, mv e', mv s, mv t) in
                             StepTo (conf (If me ms mt) mu)
                               (LetStepTo (conf me' mu') (conf me mu)
                               (Build $ conf (If me' ms mt) mu))

                 , name "if-true" $
                   mkRule3 $ \s t mu ->
                             let (ms, mt) = (mv s, mv t) in
                             StepTo (conf (If True ms mt) mu)
                               (Build $ conf ms mu)

                 , name "if-false" $
                   mkRule3 $ \s t mu ->
                             let (ms, mt) = (mv s, mv t) in
                             StepTo (conf (If False ms mt) mu)
                               (Build $ conf mt mu)

                 , name "while" $
                   mkRule3 $ \e s mu ->
                            let (me, ms) = (mv e, mv s) in
                            StepTo (conf (While me ms) mu)
                              (Build $ conf (If me (ms `Seq` (While me ms)) Skip) mu)

                 ------------------------ Vars  ---------------------------------------------

                 , name "var-lookup" $
                   mkRule3 $ \var val mu ->
                             let (mvar, mval) = (mv var, mv val) in
                             StepTo (Conf (VarExp (Var mvar)) (AssocOneVal mu mvar mval))
                               (Build $ Conf (EVal mval) (AssocOneVal mu mvar mval))

                 --------------------- Plus and LT ------------------------------------------

                 , name "plus-cong-1" $
                   mkRule4 $ \e1 e2 e1' g ->
                             let (me1, me2, me1') = (mv e1, mv e2, mv e1') in
                             StepTo (conf (Plus me1 me2) g)
                               (LetStepTo (conf me1' g) (conf me1 g)
                               (Build $ conf (Plus me1' me2) g))

                 , name "plus-cong-2" $
                   mkRule4 $ \v1 e2 e2' g ->
                             let (mv1, me2, me2') = (mv v1, mv e2, mv e2') in
                             StepTo (conf (Plus (EVal mv1) me2) g)
                               (LetStepTo (conf me2' g) (conf me2 g)
                               (Build $ conf (Plus (EVal mv1) me2') g))

                 , name "plus-eval" $
                   mkRule4 $ \v1 v2 v' g ->
                             let (mv1, mv2, mv') = (mv v1, mv v2, mv v') in
                             StepTo (conf (Plus (EVal mv1) (EVal mv2)) g)
                               (LetComputation v' ([v1, v2], \[Const n1, Const n2] -> Const (n1+n2))
                               (Build $ conf (EVal mv') g))


                 , name "lt-cong-1" $
                   mkRule4 $ \e1 e2 e1' g ->
                             let (me1, me2, me1') = (mv e1, mv e2, mv e1') in
                             StepTo (conf (LT me1 me2) g)
                               (LetStepTo (conf me1' g) (conf me1 g)
                               (Build $ conf (LT me1' me2) g))

                 , name "lt-cong-2" $
                   mkRule4 $ \v1 e2 e2' g ->
                             let (mv1, me2, me2') = (mv v1, mv e2, mv e2') in
                             StepTo (conf (LT (EVal mv1) me2) g)
                               (LetStepTo (conf me2' g) (conf me2 g)
                               (Build $ conf (LT mv1 me2') g))

                 , name "lt-eval" $
                   mkRule4 $ \v1 v2 v' g ->
                             let (mv1, mv2, mv') = (mv v1, mv v2, mv v') in
                             StepTo (conf (LT (EVal mv1) (EVal mv2)) g)
                               (LetComputation v' ([v1, v2], \[Const n1, Const n2] -> if n1 < n2 then True else False)
                               (Build $ conf mv' g))

                ]

------------------------------------------------------------------------------------------------------------------

term1 :: Term ImpLang Closed
term1 =       ("x" := intConst 1)
        `Seq` ("y" := intConst 2)
        `Seq` ("z" := Plus (varExp "x") (varExp "y"))


conf2 :: Configuration ImpLang Closed
conf2 = Conf (varExp "x") (JustSimpMap $ SingletonSimpMap (VarName "x") (Const 1))

term3 :: Term ImpLang Closed
term3 =       ("i" := intConst 0)
        `Seq` ("s" := intConst 0)
        `Seq` (While (varExp "i" :< intConst 10)
                (      ("s" := Plus (varExp "s") (varExp "i"))
                 `Seq` ("i" := Plus (varExp "i") (intConst 1))))
        `Seq` ("z" := intConst 100)
