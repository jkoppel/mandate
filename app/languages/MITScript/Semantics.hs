{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.MITScript.Semantics (
    MITScript
  ) where

import Prelude hiding ( True, False, LT, GT, EQ )
import qualified Prelude

import GHC.Generics ( Generic )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Interned.ByteString ( InternedByteString(..) )
import Data.Hashable ( Hashable )
import Data.String ( fromString )

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

        compFuncName Compute = "compute"

        runCompFunc Compute [UMINUS, NumConst (ConstInt n1)] = returnInt $ negate n1
        runCompFunc Compute [NOT, BConst b]                  = returnBool $ not $ toMetaBool b

        runCompFunc Compute [PLUS,  NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnInt $ n1 + n2
        runCompFunc Compute [PLUS,  vv1@(Str (ConstStr s1)), vv2]                   = returnString $ toString vv1 ++ toString vv2
        runCompFunc Compute [PLUS,  vv1, vv2@(Str (ConstStr s1))]                   = returnString $ toString vv1 ++ toString vv2

        runCompFunc Compute [MINUS, NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnInt $ n1 - n2
        runCompFunc Compute [TIMES, NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnInt $ n1 * n2
        runCompFunc Compute [DIV,   NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnInt $ n1 `div` n2

        runCompFunc Compute [GT,  NumConst (ConstInt n1), NumConst (ConstInt n2)]  = returnBool $ n1 > n2
        runCompFunc Compute [GTE, NumConst (ConstInt n1), NumConst (ConstInt n2)]  = returnBool $ n1 >= n2

        runCompFunc Compute [EQ, NumConst (ConstInt n1), NumConst (ConstInt n2)]   = returnBool $ n1 == n2
        runCompFunc Compute [EQ, BConst l, BConst r]                               = returnBool $ l == r
        runCompFunc Compute [EQ, None, None]                                       = returnBool Prelude.True
        runCompFunc Compute [EQ, vv1@(Str (ConstStr s1)), vv2@(Str (ConstStr s2))] = returnBool $ toString vv1 == toString vv2
        runCompFunc Compute [EQ, _, _]                                             = returnBool Prelude.False

        runCompFunc Compute [AND, BConst True, BConst True]   = returnBool Prelude.True
        runCompFunc Compute [AND, BConst l, BConst r]         = returnBool Prelude.False

        runCompFunc Compute [OR, BConst False, BConst False]  = returnBool Prelude.False
        runCompFunc Compute [OR, BConst l, BConst r]          = returnBool Prelude.True

        runCompFunc AbsCompute [_, GStar _] = return $ initConf ValStar

instance Hashable (CompFunc MITScript)

instance ValueIrrelevance (CompFunc MITScript) where
    valueIrrelevance Compute    = AbsCompute
    valueIrrelevance AbsCompute = AbsCompute

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
        let (ts, ms') = (tv s, mv s') in
            StepTo (conf (Block ts) mu)
            (LetStepTo (conf ms' mu') (conf ts mu)
            (Build (conf (Block ms') mu')))

    , name "block-nil" $
    mkRule1 $ \mu ->
        StepTo (conf (Block NilStmt) mu) (Build (conf NilStmt mu))

    , name "seq-cong" $
    mkRule5 $ \s1 s2 s1' mu mu' ->
        let (ts1, ms2, ms1') = (tv s1, mv s2, mv s1') in
            StepTo (conf (ConsStmt ts1 ms2) mu)
            (LetStepTo (conf ms1' mu') (conf ts1 mu)
            (Build (conf (ConsStmt ms1' ms2) mu')))

    , name "seq-nil" $
    mkRule2 $ \s mu ->
        let ms = mv s in
            StepTo (conf (ConsStmt NilStmt ms) mu)
            (Build $ conf ms mu)

    -- Control Flow
    , name "if-cong" $
    mkRule6 $ \e e' s t mu mu' ->
        let (te, me', ms, mt) = (tv e, mv e', mv s, mv t) in
            StepTo (conf (If te ms mt) mu)
            (LetStepTo (conf me' mu') (conf te mu)
            (Build $ conf (If me' ms mt) mu))

    , name "if-true" $
    mkRule3 $ \s t mu ->
        let (ms, mt) = (mv s, mv t) in
            StepTo (conf (If (BConst True) ms mt) mu)
            (Build $ conf ms mu)

    , name "if-false" $
    mkRule3 $ \s t mu ->
        let (ms, mt) = (mv s, mv t) in
            StepTo (conf (If (BConst False) ms mt) mu)
            (Build $ conf mt mu)

    , name "while" $
    mkRule3 $ \e s mu ->
        let (me, ms) = (mv e, mv s) in
            StepTo (conf (While me ms) mu)
            (Build $ conf (If me (ConsStmt ms (While me ms)) NilStmt) mu)

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

    -- all binary operators are left-associative
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

    -- Records Literals -> Runtime Records
    , name "record-cong" $
    mkRule4 $ \r r' mu mu' ->
        let (mr, mr') = (mv r, mv r') in
            StepTo (conf (Record mr) mu)
            (LetStepTo (conf mr' mu') (conf mr mu)
            (Build $ conf (Record mr') mu'))

    , name "record-eval" $
    mkRule2 $ \mu r ->
        let vr = vv r in
            StepTo (conf (Record vr) mu)
            (Build $ conf (ReducedRecord vr) mu)

    , name "cons-record-pair-cong-car" $
    mkRule5 $ \r r' rs mu mu' ->
        let (mr, mr', mrs) = (mv r, mv r', mv rs) in
            StepTo (conf (ConsRecordPair mr mrs) mu)
            (LetStepTo (conf mr' mu') (conf mr mu)
            (Build $ conf (ConsRecordPair mr' mrs) mu'))

    , name "cons-record-pair-cong-cdr" $
    mkRule5 $ \r rs rs' mu mu' ->
        let (vr, mrs, mrs') = (vv r, mv rs, mv rs') in
            StepTo (conf (ConsRecordPair vr mrs) mu)
            (LetStepTo (conf mrs' mu') (conf mrs mu)
            (Build $ conf (ConsRecordPair vr mrs') mu'))

    , name "cons-record-pair-eval" $
    mkRule3 $ \r rs mu ->
        let (vr, vrs) = (vv r, vv rs) in
            StepTo (conf (ConsRecordPair vr vrs) mu)
            (Build $ conf (ReducedRecordCons vr vrs) mu)

    , name "record-pair-cong" $
    mkRule5 $ \k vv vv' mu mu' ->
        let (mk, mvv, mvv') = (mv k, mv vv, mv vv') in
            StepTo (conf (RecordPair mk mvv) mu)
            (LetStepTo (conf mvv' mu') (conf mvv mu)
            (Build $ conf (RecordPair mk mvv') mu'))

    , name "record-pair-eval" $
    mkRule3 $ \key val mu ->
        let (mkey, vval) = (mv key, vv val) in
            StepTo (conf (RecordPair mkey vval) mu)
            (Build $ conf (ReducedRecordPair mkey vval) mu)

    , name "nil-record-pair-eval" $
    mkRule1 $ \mu ->
            StepTo (conf NilRecordPair mu)
            (Build $ conf ReducedRecordNil mu)
    ]

toMetaBool :: Term MITScript -> Bool
toMetaBool True = Prelude.True
toMetaBool False = Prelude.False

fromMetaBool :: Bool -> Term MITScript
fromMetaBool Prelude.True = True
fromMetaBool Prelude.False = False

toString :: Term MITScript -> String
toString (BConst b) = show $ toMetaBool b
toString (NumConst (ConstInt n1)) = show n1
toString (Str (ConstStr s1)) = let str = show s1 in take (length str - 2) $ drop 1 str
toString None = "None"

returnInt :: Monad m => Integer -> m (GConfiguration (RedState MITScript) MITScript)
returnInt x = return $ initConf $ NumConst $ ConstInt x

returnBool :: Monad m => Bool -> m (GConfiguration (RedState MITScript) MITScript)
returnBool x = return $ initConf $ BConst $ fromMetaBool x

returnString :: Monad m => String -> m (GConfiguration (RedState MITScript) MITScript)
returnString x = return $ initConf $ Str $ ConstStr $ fromString x