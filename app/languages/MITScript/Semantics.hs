{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.MITScript.Semantics (
    MITScript
  ) where

import Prelude hiding ( True, False, LT, GT, EQ )

import GHC.Generics ( Generic )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Interned.ByteString ( InternedByteString(..) )
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

import Languages.MITScript.Signature
import Languages.MITScript.Translate
import Languages.MITScript.Parse

instance LangBase MITScript where
        -- these are keyed by terms becuase thats what Imp.hs did and it seemed to be rationalized well
        type RedState MITScript = SimpEnv (Term MITScript) (Term MITScript)

        data CompFunc MITScript = Compute | AbsCompute deriving ( Eq, Generic )

        compFuncName Compute      = "compute"

        runCompFunc Compute    [UMINUS, NumConst (ConstInt n1)] = return $ initConf $ NumConst (ConstInt (negate n1))

        runCompFunc Compute    [PLUS,  NumConst (ConstInt n1), NumConst (ConstInt n2)] = return $ initConf $ NumConst (ConstInt (n1+n2)) -- todo: strings
        runCompFunc Compute    [MINUS, NumConst (ConstInt n1), NumConst (ConstInt n2)] = return $ initConf $ NumConst (ConstInt (n1-n2))
        runCompFunc Compute    [TIMES, NumConst (ConstInt n1), NumConst (ConstInt n2)] = return $ initConf $ NumConst (ConstInt (n1*n2))
        runCompFunc Compute    [DIV,   NumConst (ConstInt n1), NumConst (ConstInt n2)] = return $ initConf $ NumConst (ConstInt (n1 `div` n2))

        runCompFunc Compute    [GT, NumConst (ConstInt n1), NumConst (ConstInt n2)]   = if n1 > n2  then return (initConf True) else return (initConf False)
        runCompFunc Compute    [GTE, NumConst (ConstInt n1), NumConst (ConstInt n2)]  = if n1 >= n2 then return (initConf True) else return (initConf False)

        runCompFunc Compute    [EQ, NumConst (ConstInt n1), NumConst (ConstInt n2)]   = if n1 == n2 then return (initConf True) else return (initConf False)
        runCompFunc Compute    [EQ, BConst l, BConst r]                               = if l == r   then return (initConf True) else return (initConf False)

        runCompFunc Compute    [NOT, BConst b] = if b == True then return (initConf False) else return (initConf True)

        runCompFunc Compute    [AND, BConst True, BConst True]   = return $ initConf $ BConst True
        runCompFunc Compute    [AND, BConst l, BConst r]         = return $ initConf $ BConst False

        runCompFunc Compute    [OR, BConst False, BConst False] = return $ initConf $ BConst False
        runCompFunc Compute    [OR, BConst l, BConst r]         = return $ initConf $ BConst True

        runCompFunc AbsCompute   [_, GStar _] = return $ initConf ValStar

instance Hashable (CompFunc MITScript)

instance ValueIrrelevance (CompFunc MITScript) where
    valueIrrelevance Compute      = AbsCompute
    valueIrrelevance AbsCompute       = AbsCompute

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

    --- Arithmetic Operations
    -- Unary
    , name "unary-cong" $
    mkRule5 $ \e1 e1' mu mu' op ->
        let (te, me', mop) = (tv e1, mv e1', mv op) in
            StepTo (conf (UnExp mop te) mu)
            (LetStepTo (conf me' mu') (conf te mu)
            (Build $ conf (UnExp mop me') mu'))

    , name "unary-eval" $
    mkRule4 $ \v1 v' mu op ->
        let (vv1, vv', mop) = (vv v1, vv v', mv op) in
            StepTo (conf (UnExp mop vv1) mu)
            (LetComputation (initConf $ ValVar v') (ExtComp Compute [mop, vv1])
            (Build $ conf vv' mu))

    -- Binary
    -- assumes all binary operators are left-associative
    , name "binary-cong-left" $
    mkRule6 $ \e1 e2 e1' mu mu' op ->
        let (te1, me2, me1', mop) = (tv e1, mv e2, mv e1', mv op) in
            StepTo (conf (BinExp te1 mop me2) mu)
            (LetStepTo (conf me1' mu') (conf te1 mu)
            (Build $ conf (BinExp me1' mop me2) mu'))

    , name "binary-cong-right" $
    mkRule6 $ \v1 e2 e2' mu mu' op ->
        let (vv1, te2, me2', mop) = (vv v1, tv e2, mv e2', mv op) in
            StepTo (conf (BinExp vv1 mop te2) mu)
            (LetStepTo (conf me2' mu') (conf te2 mu)
            (Build $ conf (BinExp vv1 mop me2') mu'))

    , name "binary-eval" $
    mkRule5 $ \v1 v2 v' op mu ->
        let (vv1, vv2, vv', mop) = (vv v1, vv v2, vv v', mv op) in
            StepTo (conf (BinExp vv1 mop vv2) mu)
            (LetComputation (initConf $ ValVar v') (ExtComp Compute [mop, vv1, vv2])
            (Build $ conf vv' mu))
    ]