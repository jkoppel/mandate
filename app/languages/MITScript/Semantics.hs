{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.MITScript.Semantics (
    MITScript,
    run
  ) where

import Prelude hiding ( True, False, LT, GT )

import GHC.Generics ( Generic )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Interned.ByteString ( InternedByteString(..) )
import Data.Hashable ( Hashable )

import Configuration
import Lang
import Matching
import Semantics.Abstraction
import Semantics.Conversion
import Semantics.General
import Semantics.PAM
import Semantics.SOS
import Term
import Var

import Languages.MITScript.Signature
import Languages.MITScript.Translate
import Languages.MITScript.Parse

instance LangBase MITScript where
        -- these are keyed by terms becuase thats what Imp.hs did and it seemed to be rationalized well
        type RedState MITScript = SimpEnv (Term MITScript) (Term MITScript)

        data CompFunc MITScript = RunAdd    | RunGT    | RunUMinus
                                | AbsRunAdd | AbsRunGT | AbsRunUMinus
            deriving ( Eq, Generic )

        compFuncName RunAdd     = "runAdd"
        compFuncName RunGT      = "runGT"
        compFuncName RunUMinus  = "runUminus"

        runCompFunc RunAdd     [NumConst (ConstInt n1), NumConst (ConstInt n2)] = return $ initConf $ NumConst (ConstInt (n1+n2))
        runCompFunc RunGT      [NumConst (ConstInt n1), NumConst (ConstInt n2)] = if n1 < n2 then return (initConf True) else return (initConf False)
        runCompFunc RunUMinus  [NumConst (ConstInt n1)] = return $ initConf $ NumConst (ConstInt (negate n1))

        runCompFunc AbsRunUMinus [GStar _]    = return $ initConf ValStar
        runCompFunc AbsRunAdd    [GStar _, _] = return $ initConf ValStar
        runCompFunc AbsRunAdd    [_, GStar _] = return $ initConf ValStar
        runCompFunc AbsRunGT     [GStar _, _] = return $ initConf ValStar
        runCompFunc AbsRunGT     [_, GStar _] = return $ initConf ValStar

instance Hashable (CompFunc MITScript)

instance ValueIrrelevance (CompFunc MITScript) where
    valueIrrelevance RunUMinus  = AbsRunUMinus
    valueIrrelevance RunAdd     = AbsRunAdd
    valueIrrelevance RunGT      = AbsRunGT

    valueIrrelevance AbsRunUMinus  = AbsRunUMinus
    valueIrrelevance AbsRunAdd     = AbsRunAdd
    valueIrrelevance AbsRunGT      = AbsRunGT

instance Lang MITScript where
    signature = mitScriptSig
    rules = mitScriptRules

    initConf t = Conf t EmptySimpEnv

conf :: Term MITScript -> MetaVar -> Configuration MITScript
conf t v = Conf t (WholeSimpEnv v)

vv :: MetaVar -> Term MITScript
vv = ValVar

tv :: MetaVar -> Term MITScript
tv = NonvalVar

mv :: MetaVar -> Term MITScript
mv = MetaVar

mitScriptRules :: IO (NamedRules MITScript)
mitScriptRules = sequence [
     -- Sequential Behaviour
    name "block-cong" $
    mkRule4 $ \s s' mu mu' ->
        let (ts, ts') = (tv s, tv s') in
            StepTo (conf (Block ts) mu)
            (LetStepTo (conf ts' mu') (conf ts mu)
            (Build (conf (Block ts') mu')))

    , name "seq-cong" $
    mkRule5 $ \s1 s2 s1' mu mu' ->
        let (ts1, ms2, ms1') = (tv s1, mv s2, mv s1') in
            StepTo (conf (ConsStmt ts1 ms2) mu)
            (LetStepTo (conf ms1' mu') (conf ts1 mu)
            (Build (conf (ConsStmt ms1' ms2) mu')))

    , name "seq-eval" $
    mkRule2 $ \s mu ->
        let ms = mv s in
            StepTo (conf (ConsStmt NilStmt ms) mu)
            (Build $ conf ms mu)

    -- Variable Access
    , name "assn-cong" $
    mkRule5 $ \var e e' mu mu' ->
        let (mvar, te, me') = (mv var, tv e, mv e') in
            StepTo (conf (Assign mvar te) mu)
            (LetStepTo (conf me' mu') (conf te mu)
            (Build $ conf (Assign mvar me') mu'))

    , name "assn-eval" $
    mkRule3 $ \var val mu ->
        let (mvar, vval) = (mv var, vv val) in
            StepTo (conf (Assign (Var mvar) vval) mu)
            (Build $ Conf NilStmt (AssocOneVal mu mvar vval))

    , name "var-lookup" $
    mkRule3 $ \var val mu ->
        let (mvar, vval) = (mv var, vv val) in
            StepTo (Conf (Var mvar) (AssocOneVal mu mvar vval))
            (Build $ Conf vval (AssocOneVal mu mvar vval))

    -- Arithmetic Operations
    -- * -> *
    -- Num -> Num
    , name "uminus-cong" $
    mkRule4 $ \e1 e1' mu mu' ->
        let (te, me') = (tv e1, mv e1') in
            StepTo (conf (UnExp UMINUS te) mu)
            (LetStepTo (conf me' mu') (conf te mu)
            (Build $ conf (UnExp UMINUS me') mu'))

    , name "uminus-eval" $
    mkRule3 $ \v1 v' mu ->
        let (vv1, vv') = (vv v1, vv v') in
            StepTo (conf (UnExp UMINUS vv1) mu)
            (LetComputation (initConf $ ValVar v') (ExtComp RunUMinus [vv1])
            (Build $ conf vv' mu))

    -- Bool -> Bool
    , name "not-cong" $
    mkRule4 $ \e1 e1' mu mu' ->
        let (te, me') = (tv e1, mv e1') in
            StepTo (conf (UnExp NOT te) mu)
            (LetStepTo (conf me' mu') (conf te mu)
            (Build $ conf (UnExp NOT me') mu'))

    , name "not-eval-true" $
    mkRule3 $ \v1 v' mu ->
        let (vv1, vv') = (vv v1, vv v') in
            StepTo (conf (UnExp NOT (BConst True)) mu) (Build $ conf (BConst False) mu)

    , name "not-eval-false" $
    mkRule3 $ \v1 v' mu ->
        let (vv1, vv') = (vv v1, vv v') in
            StepTo (conf (UnExp NOT (BConst False)) mu) (Build $ conf (BConst True) mu)

    -- * -> * -> *
    -- Num -> Num -> Num
    , name "plus-cong-1" $
    mkRule5 $ \e1 e2 e1' mu mu' ->
        let (te1, me2, me1') = (tv e1, mv e2, mv e1') in
            StepTo (conf (BinExp te1 PLUS me2) mu)
            (LetStepTo (conf me1' mu') (conf te1 mu)
            (Build $ conf (BinExp me1' PLUS me2) mu'))

    , name "plus-cong-2" $
    mkRule5 $ \v1 e2 e2' mu mu' ->
        let (vv1, te2, me2') = (vv v1, tv e2, mv e2') in
            StepTo (conf (BinExp vv1 PLUS te2) mu)
            (LetStepTo (conf me2' mu') (conf te2 mu)
            (Build $ conf (BinExp vv1 PLUS me2') mu'))

    , name "plus-eval" $
    mkRule4 $ \v1 v2 v' mu ->
        let (vv1, vv2, vv') = (vv v1, vv v2, vv v') in
            StepTo (conf (BinExp vv1 PLUS vv2) mu)
            (LetComputation (initConf $ ValVar v') (ExtComp RunAdd [vv1, vv2])
            (Build $ conf vv' mu))

    -- Num -> Num -> Bool
    , name "gt-cong-1" $
    mkRule5 $ \e1 e2 e1' mu mu' ->
        let (te1, me2, me1') = (tv e1, mv e2, mv e1') in
            StepTo (conf (BinExp te1 GT me2) mu)
            (LetStepTo (conf me1' mu') (conf te1 mu)
            (Build $ conf (BinExp me1' GT me2) mu'))

    , name "gt-cong-2" $
    mkRule5 $ \v1 e2 e2' mu mu' ->
        let (vv1, te2, me2') = (vv v1, tv e2, mv e2') in
            StepTo (conf (BinExp vv1 GT te2) mu)
            (LetStepTo (conf me2' mu') (conf te2 mu)
            (Build $ conf (BinExp vv1 GT me2') mu'))

    , name "gt-eval" $
    mkRule4 $ \v1 v2 v' mu ->
        let (vv1, vv2, vv') = (vv v1, vv v2, vv v') in
            StepTo (conf (BinExp vv1 GT vv2) mu)
            (LetComputation (initConf $ ValVar v') (ExtComp RunGT [vv1, vv2])
            (Build $ conf vv' mu))

    -- Bool -> Bool -> Bool
    , name "and-cong" $
    mkRule5 $ \e1 e2 e1' mu mu' ->
        let (te1, me2, me1') = (tv e1, mv e2, mv e1') in
            StepTo (conf (BinExp te1 AND me2) mu)
            (LetStepTo (conf me1' mu') (conf te1 mu)
            (Build $ conf (BinExp me1' AND me2) mu'))

    , name "and-eval-short-circuit-false" $
    mkRule2 $ \e mu ->
        let me = mv e in
            StepTo (conf (BinExp (BConst False) AND me) mu) (Build $ conf (BConst False) mu)

    , name "and-eval-short-circuit-true" $
    mkRule2 $ \e mu ->
        let me = mv e in
            StepTo (conf (BinExp (BConst True) AND me) mu) (Build $ conf me mu)

    , name "or-cong" $
    mkRule5 $ \e1 e2 e1' mu mu' ->
        let (te1, me2, me1') = (tv e1, mv e2, mv e1') in
            StepTo (conf (BinExp te1 OR me2) mu)
            (LetStepTo (conf me1' mu') (conf te1 mu)
            (Build $ conf (BinExp me1' OR me2) mu'))

    , name "or-eval-short-circuit-true" $
    mkRule2 $ \e mu ->
        let me = mv e in
            StepTo (conf (BinExp (BConst True) OR me) mu) (Build $ conf (BConst True) mu)

    , name "or-eval-short-circuit-false" $
    mkRule2 $ \e mu ->
        let me = mv e in
            StepTo (conf (BinExp (BConst False) OR me) mu) (Build $ conf me mu)
    ]

term1 :: Term MITScript
term1 = Block $
    ConsStmt
        (Assign
            (Var (Name "x"))
            (BinExp (NumConst (ConstInt 3)) PLUS ( NumConst (ConstInt 5))))
        (ConsStmt
            (Assign
                (Var (Name "y"))
                (BinExp (Var (Name "x")) PLUS ( NumConst (ConstInt 5))))
            NilStmt)

run :: FilePath -> IO (Term MITScript)
run s = do
    x <- parseFile s;
    return $ toGeneric x