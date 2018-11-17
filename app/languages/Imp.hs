{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.Imp (
    ImpLang
  , amStateReduce
  ) where

import Prelude hiding ( True, False, LT )

import GHC.Generics ( Generic )

import Control.Monad
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Interned.ByteString ( InternedByteString(..) )
import Data.Hashable ( Hashable )

import Configuration
import Lang
import Matching
import Semantics.Abstraction
import Semantics.Conversion
import Semantics.Context
import Semantics.General
import Semantics.AbstractMachine
import Semantics.PAM
import Semantics.SOS
import Term
import Var

data ImpLang

-- Why are the keys of this map terms?
-- Because my infrastructure, namely ExtFunc's, currently assumes metavars are only bound to
-- terms (and maps), so it must be a term.
--
-- Also, now may actually need to operate on states with metavariable keys

instance LangBase ImpLang where
  type RedState ImpLang = SimpEnv (Term ImpLang) (Term ImpLang)

  data CompFunc ImpLang = RunAdd    | RunLT    | DoReadInt    | DoWriteInt
                        | AbsRunAdd | AbsRunLT | AbsDoReadInt | AbsDoWriteInt
    deriving ( Eq, Generic )

  compFuncName RunAdd   = "runAdd"
  compFuncName RunLT    = "runLT"
  compFuncName DoReadInt  = "read"
  compFuncName DoWriteInt = "write"

  runCompFunc func (c:cs)  = runExternalComputation func (confState c) (map confTerm (c:cs))

instance Hashable (CompFunc ImpLang)

instance Irrelevance (CompFunc ImpLang) where
  irrelevance _ RunAdd     = AbsRunAdd
  irrelevance _ RunLT      = AbsRunLT
  irrelevance _ DoReadInt  = AbsDoReadInt
  irrelevance _ DoWriteInt = AbsDoWriteInt

  irrelevance _ AbsRunAdd     = AbsRunAdd
  irrelevance _ AbsRunLT      = AbsRunLT
  irrelevance _ AbsDoReadInt  = AbsDoReadInt
  irrelevance _ AbsDoWriteInt = AbsDoWriteInt

instance Lang ImpLang where
  signature = impLangSig
  initConf t = Conf t EmptySimpEnv

instance HasSOS ImpLang where
  rules = impLangRules

impLangSig :: Signature ImpLang
impLangSig = Signature [ NodeSig ":=" ["Var", "Exp"] "Exp"
                       , ValSig "Skip" [] "Exp"
                       , NodeSig "Seq" ["Stmt", "Stmt"] "Stmt"
                       , NodeSig "If" ["Exp", "Stmt", "Stmt"] "Stmt"
                       , NodeSig "While" ["Exp", "Stmt"] "Stmt"

                       , NodeSig "ReadInt" [] "Exp"
                       , NodeSig "WriteInt" ["Exp"] "Exp"

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

pattern Assign :: Term ImpLang -> Term ImpLang -> Term ImpLang
pattern Assign x y = Node ":=" [x, y]

pattern Skip :: Term ImpLang
pattern Skip = Val "Skip" []

pattern Seq :: Term ImpLang -> Term ImpLang -> Term ImpLang
pattern Seq x y = Node "Seq" [x, y]

pattern If :: Term ImpLang -> Term ImpLang -> Term ImpLang -> Term ImpLang
pattern If x y z = Node "If" [x,y,z]

pattern While :: Term ImpLang -> Term ImpLang -> Term ImpLang
pattern While x y = Node "While" [x, y]

pattern ReadInt :: Term ImpLang
pattern ReadInt = Node "ReadInt" []

pattern WriteInt :: Term ImpLang -> Term ImpLang
pattern WriteInt x = Node "WriteInt" [x]

pattern Var :: Term ImpLang -> Term ImpLang
pattern Var x = Node "Var" [x]

pattern VarName :: InternedByteString -> Term ImpLang
pattern VarName v = StrNode "VarName" v

pattern VarExp :: Term ImpLang -> Term ImpLang
pattern VarExp v = Node "VarExp" [v]

pattern True :: Term ImpLang
pattern True = Val "true" []

pattern False :: Term ImpLang
pattern False = Val "false" []

pattern EVal :: Term ImpLang -> Term ImpLang
pattern EVal n = Val "EVal" [n]

pattern Const :: Integer -> Term ImpLang
pattern Const n = IntNode "Const" n

pattern Plus :: Term ImpLang -> Term ImpLang -> Term ImpLang
pattern Plus x y = Node "+" [x, y]

pattern LT :: Term ImpLang -> Term ImpLang -> Term ImpLang
pattern LT x y = Node "<" [x, y]

varExp :: InternedByteString -> Term ImpLang
varExp s = VarExp $ Var $ VarName s

pattern (:=) :: InternedByteString -> Term ImpLang -> Term ImpLang
pattern (:=) var t = Assign (Var (VarName var)) t

pattern (:<) :: Term ImpLang -> Term ImpLang -> Term ImpLang
pattern (:<) l r = LT l r

intConst :: Integer -> Term ImpLang
intConst n = EVal $ Const n

conf :: Term ImpLang -> MetaVar -> Configuration ImpLang
conf t v = Conf t (WholeSimpEnv v)

vv :: MetaVar -> Term ImpLang
vv = ValVar

tv :: MetaVar -> Term ImpLang
tv = NonvalVar

mv :: MetaVar -> Term ImpLang
mv = MetaVar

impLangRules :: IO (NamedRules ImpLang)
impLangRules = sequence [

                   name "assn-cong" $
                   mkRule5 $ \var e e' mu mu' ->
                             let (mvar, te, me') = (mv var, tv e, mv e') in
                             StepTo (conf (Assign mvar te) mu)
                               (LetStepTo (conf me' mu') (conf te mu)
                               (Build $ conf (Assign mvar me') mu'))

                 , name "assn-eval" $
                   mkRule3 $ \var val mu ->
                             let (mvar, vval) = (mv var, vv val) in
                             StepTo (conf (Assign (Var mvar) vval) mu)
                               (Build $ Conf Skip (AssocOneVal mu mvar vval))

                 ----------------------------------------------------------------------------

                 , name "seq-cong" $
                   mkRule5 $ \s1 s2 s1' mu mu' ->
                             let (ts1, ms2, ms1') = (tv s1, mv s2, mv s1') in
                             StepTo (conf (Seq ts1 ms2) mu)
                               (LetStepTo (conf ms1' mu') (conf ts1 mu)
                               (Build (conf (Seq ms1' ms2) mu')))

                 , name "seq-eval" $
                   mkRule2 $ \s mu ->
                             let ms = mv s in
                             StepTo (conf (Seq Skip ms) mu)
                               (Build $ conf ms mu)

                 , name "if-cong" $
                   mkRule6 $ \e e' s t mu mu' ->
                             let (te, me', ms, mt) = (tv e, mv e', mv s, mv t) in
                             StepTo (conf (If te ms mt) mu)
                               (LetStepTo (conf me' mu') (conf te mu)
                               (Build $ conf (If me' ms mt) mu'))

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
                              (Build $ conf (If me (Seq ms (While me ms)) Skip) mu)

                 ----------------------------------------------------------------------------

                 , name "read-int" $
                   mkRule2 $ \val mu ->
                             let (mval) = (mv val) in
                             StepTo (conf ReadInt mu)
                               (LetComputation (initConf mval) (extComp DoReadInt (WholeSimpEnv mu) [Skip])
                               (Build $ conf mval mu))

                 , name "write-int-cong" $
                   mkRule4 $ \arg arg' mu mu' ->
                             let (targ, marg') = (tv arg, mv arg') in
                             StepTo (conf (WriteInt targ) mu)
                               (LetStepTo (conf marg' mu') (conf targ mu)
                               (Build $ conf (WriteInt marg') mu'))

                 , name "write-int" $
                   mkRule3 $ \arg val mu ->
                             let (varg) = (vv arg) in
                             StepTo (conf (WriteInt varg) mu)
                               (LetComputation (initConf $ MetaVar val) (extComp DoWriteInt (WholeSimpEnv mu) [varg])
                               (Build $ conf Skip mu))

                 ------------------------ Vars  ---------------------------------------------

                 , name "var-lookup" $
                   mkRule3 $ \var val mu ->
                             let (mvar, vval) = (mv var, vv val) in
                             StepTo (Conf (VarExp (Var mvar)) (AssocOneVal mu mvar vval))
                               (Build $ Conf vval (AssocOneVal mu mvar vval))

                 --------------------- Plus and LT ------------------------------------------

                 , name "plus-cong-1" $
                   mkRule5 $ \e1 e2 e1' mu mu' ->
                             let (te1, me2, me1') = (tv e1, mv e2, mv e1') in
                             StepTo (conf (Plus te1 me2) mu)
                               (LetStepTo (conf me1' mu') (conf te1 mu)
                               (Build $ conf (Plus me1' me2) mu'))

                 , name "plus-cong-2" $
                   mkRule5 $ \v1 e2 e2' mu mu' ->
                             let (vv1, te2, me2') = (vv v1, tv e2, mv e2') in
                             StepTo (conf (Plus vv1 te2) mu)
                               (LetStepTo (conf me2' mu') (conf te2 mu)
                               (Build $ conf (Plus vv1 me2') mu'))

                 , name "plus-eval" $
                   mkRule4 $ \v1 v2 v' mu ->
                             let (vv1, vv2, vv') = (vv v1, vv v2, vv v') in
                             StepTo (conf (Plus vv1 vv2) mu)
                               (LetComputation (initConf $ ValVar v') (extComp RunAdd (WholeSimpEnv mu) [vv1, vv2])
                               (Build $ conf vv' mu))


                 , name "lt-cong-1" $
                   mkRule5 $ \e1 e2 e1' mu mu' ->
                             let (te1, me2, me1') = (tv e1, mv e2, mv e1') in
                             StepTo (conf (LT te1 me2) mu)
                               (LetStepTo (conf me1' mu') (conf te1 mu)
                               (Build $ conf (LT me1' me2) mu'))

                 , name "lt-cong-2" $
                   mkRule5 $ \v1 e2 e2' mu mu' ->
                             let (vv1, te2, me2') = (vv v1, tv e2, mv e2') in
                             StepTo (conf (LT vv1 te2) mu)
                               (LetStepTo (conf me2' mu') (conf te2 mu)
                               (Build $ conf (LT vv1 me2') mu'))

                 , name "lt-eval" $
                   mkRule4 $ \v1 v2 v' mu ->
                             let (vv1, vv2, vv') = (vv v1, vv v2, vv v') in
                             StepTo (conf (LT vv1 vv2) mu)
                               (LetComputation (initConf vv') (extComp RunLT (WholeSimpEnv mu) [vv1, vv2])
                               (Build $ conf vv' mu))

                ]


runExternalComputation :: CompFunc ImpLang -> RedState ImpLang -> [Term ImpLang] -> MatchEffect (Configuration ImpLang)
runExternalComputation RunAdd state [EVal (Const n1), EVal (Const n2)] = return $ initConf $ EVal (Const (n1+n2))
runExternalComputation RunLT  state [EVal (Const n1), EVal (Const n2)] = if n1 < n2 then return (initConf True) else return (initConf False)

runExternalComputation DoWriteInt state [EVal (Const n)] = matchEffectOutput (BS.pack $ show n) >> return (initConf Skip)
runExternalComputation DoReadInt  state [Skip] = initConf <$> EVal <$> Const <$> read <$> BS.unpack <$> matchEffectInput

runExternalComputation AbsRunAdd state [GStar _, _] = return $ initConf ValStar
runExternalComputation AbsRunAdd state [_, GStar _] = return $ initConf ValStar
runExternalComputation AbsRunLT  state [GStar _, _] = mplus (return $ initConf True) (return $ initConf False)
runExternalComputation AbsRunLT  state [_, GStar _] = mplus (return $ initConf True) (return $ initConf False)

runExternalComputation AbsDoReadInt   state [_] = return $ initConf ValStar
runExternalComputation AbsDoWriteInt  state [_] = return $ initConf Skip

------------------------------------------------------------------------------------------------------------------

whileReduce :: Term ImpLang -> Term ImpLang
whileReduce (While e s) = If e (Seq s (While e s)) ValStar
whileReduce t = t

seqreduce :: Term ImpLang -> Term ImpLang
seqreduce (Seq (Seq ValStar ValStar) s) = seqreduce s
seqreduce (Seq ValStar s) = seqreduce s
seqreduce (Seq s ValStar) = seqreduce s
seqreduce t = t

contextReduce :: Context ImpLang -> Context ImpLang
contextReduce (KPush (KInp _  (KBuild (Conf (Seq _ ValStar)   _))) k) = contextReduce k
contextReduce (KPush (KInp i  (KBuild (Conf (Seq _ (Seq a b)) s))) k) = contextReduce (KPush (KInp i (KBuild (Conf (Seq a b) s))) k)
contextReduce k = k

configurationReduce :: Configuration ImpLang -> Configuration ImpLang
configurationReduce (Conf term state) = Conf (seqreduce (whileReduce term)) state

amStateReduce :: AMState ImpLang -> AMState ImpLang
amStateReduce (AMState c k) = AMState (configurationReduce c) (contextReduce k)

term1 :: Term ImpLang
term1 = Seq ("x" := intConst 1)
      $ Seq ("y" := intConst 2)
            ("z" := Plus (varExp "x") (varExp "y"))


conf2 :: Configuration ImpLang
conf2 = Conf (varExp "x") (JustSimpMap $ SingletonSimpMap (VarName "x") (Const 1))

term3 :: Term ImpLang
term3 = Seq ("u" := ReadInt)
        $ Seq ("i" := intConst 0)
        $ Seq ("s" := intConst 0)
        $ Seq (While (varExp "i" :< varExp "u")
                (Seq ("s" := Plus (varExp "s") (varExp "i"))
                     ("i" := Plus (varExp "i") (intConst 1))))
              (WriteInt $ varExp "s")

term4 :: Term ImpLang
term4 =       ("u" := ReadInt)
        `Seq` ("i" := intConst 5)
        `Seq` ("b" := (varExp "u" :< varExp "i"))
        `Seq` (If (varExp "b") ( (WriteInt $ varExp "u") ) Skip )
        `Seq` (If (varExp "b") ( (WriteInt $ varExp "u") ) Skip )
