{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.MITScript.Semantics (
    MITScript
  ) where

import Prelude hiding ( True, False, LT, GT, EQ )
import qualified Prelude

import GHC.Generics ( Generic )

import qualified Data.Map as Map
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Interned ( unintern, intern )
import Data.Interned.ByteString ( InternedByteString(..) )
import Data.Hashable ( Hashable )
import Data.String ( fromString )
import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )

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

        type RedState MITScript = (Term MITScript, SimpEnv (Term MITScript) (Term MITScript))

        data CompFunc MITScript = ReadField    | AbsReadField
                                | WriteField   | AbsWriteField
                                | AllocAddress | AbsAllocAddress
                                | Compute      | AbsCompute
                                | WriteIndex   | AbsWriteIndex
                                | ReadIndex    | AbsReadIndex
                                | RunBuiltin   | AbsRunBuiltin
            deriving ( Eq, Generic )

        compFuncName ReadField    = "readField"
        compFuncName WriteField   = "writeField"
        compFuncName AllocAddress = "allocAddress"
        compFuncName Compute      = "compute"
        compFuncName WriteIndex   = "writeIndex"
        compFuncName ReadIndex    = "readIndex"
        compFuncName RunBuiltin   = "runBuiltin"

        compFuncName AbsReadField    = "absreadField"
        compFuncName AbsWriteField   = "abswriteField"
        compFuncName AbsAllocAddress = "absallocAddress"
        compFuncName AbsCompute      = "abscompute"
        compFuncName AbsWriteIndex   = "abswriteIndex"
        compFuncName AbsReadIndex    = "absreadIndex"
        compFuncName AbsRunBuiltin   = "absrunBuiltin"

        runCompFunc func (c:cs)  = runExternalComputation func (confState c) (map confTerm (c:cs))

instance Hashable (CompFunc MITScript)

instance Irrelevance (CompFunc MITScript) where
    irrelevance _ ReadField    = AbsReadField
    irrelevance _ WriteField   = AbsWriteField
    irrelevance _ AllocAddress = AbsAllocAddress
    irrelevance _ Compute      = AbsCompute
    irrelevance _ ReadIndex    = AbsReadIndex
    irrelevance _ WriteIndex   = AbsWriteIndex
    irrelevance _ RunBuiltin   = AbsRunBuiltin

    irrelevance _ AbsReadField    = AbsReadField
    irrelevance _ AbsAllocAddress = AbsAllocAddress
    irrelevance _ AbsCompute      = AbsCompute
    irrelevance _ AbsReadIndex    = AbsReadIndex
    irrelevance _ AbsWriteIndex   = AbsWriteIndex
    irrelevance _ AbsRunBuiltin   = AbsRunBuiltin

instance Lang MITScript where
    signature = mitScriptSig

    initConf t = Conf t (
        ConsFrame (HeapAddr 0) NilFrame,
        JustSimpMap $ SimpEnvMap $ Map.fromList
            [
                (HeapAddr 0, ReducedRecord
                                $ ReducedRecordCons (ReducedRecordPair (Name "print")   (ReferenceVal $ HeapAddr 1))
                                $ ReducedRecordCons (ReducedRecordPair (Name "read")    (ReferenceVal $ HeapAddr 2))
                                $ ReducedRecordCons (ReducedRecordPair (Name "intcast") (ReferenceVal $ HeapAddr 3))
                                $ Parent $ HeapAddr $ -1)
              , (HeapAddr 1, builtinPrint)
              , (HeapAddr 2, builtinRead)
              , (HeapAddr 3, builtinIntCast)
            ])

instance HasSOS MITScript where
    rules = mitScriptRules

emptyConf :: Term MITScript -> Configuration MITScript
emptyConf t = Conf t (ReducedRecordNil, EmptySimpEnv)

conf :: Term MITScript -> (MetaVar, MetaVar) -> Configuration MITScript
conf t (returnStack, heap) = Conf t (mv returnStack, WholeSimpEnv heap)

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
    mkPairRule2 $ \env env' ->
    mkRule2 $ \s s' ->
        let (ts, ms') = (tv s, mv s') in
            StepTo (conf (Block ts) env)
            (LetStepTo (conf ms' env') (conf ts env)
            (Build (conf (Block ms') env')))

    , name "block-nil" $
    mkPairRule1 $ \env ->
    mkRule1 $ \val ->
        let vval = vv val in
            StepTo (conf (Block vval) env) (Build (conf vval env))

    , name "seq-cong" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \s1 s2 s1' ->
        let (ts1, ms2, ms1') = (tv s1, mv s2, mv s1') in
            StepTo (conf (ConsStmt ts1 ms2) env)
            (LetStepTo (conf ms1' env') (conf ts1 env)
            (Build (conf (ConsStmt ms1' ms2) env')))

    , name "seq-nil" $
    mkPairRule1 $ \env ->
    mkRule1 $ \s ->
        let ms = mv s in
            StepTo (conf (ConsStmt NilStmt ms) env)
            (Build $ conf ms env)

    , name "seq-global" $
    mkPairRule1 $ \env ->
    mkRule2 $ \s g ->
        let (ms, mg) = (mv s, mv g) in
            StepTo (conf (ConsStmt (Global mg) ms) env)
            (Build $ conf (ConsStmt (Assign (Var mg) GlobalVar) ms) env)

    , name "seq-ret-cong" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \s val val' ->
        let (ms, tval, mval) = (mv s, tv val, mv val') in
            StepTo (conf (ConsStmt (Return tval) ms) env)
                (LetStepTo (conf mval env') (conf tval env)
                (Build $ conf (ConsStmt (Return mval) ms) env'))

    , name "seq-ret-eval" $
    mkPairRule1 $ \env ->
    mkRule2 $ \s val ->
        let (ms, vval) = (mv s, vv val) in
            StepTo (conf (ConsStmt (Return vval) ms) env)
            (Build $ conf (Return vval) env)


    , name "exp-stmt-cong" $
    mkPairRule2 $ \env env' ->
    mkRule2 $ \exp exp' ->
        let (texp, mexp) = (tv exp, mv exp') in
            StepTo (conf (ExpStmt texp) env)
            (LetStepTo (conf mexp env') (conf texp env)
            (Build (conf (ExpStmt mexp) env')))

    , name "exp-stmt-eval" $
    mkPairRule2 $ \env env' ->
    mkRule2 $ \exp exp' ->
        let vexp = vv exp in
            StepTo (conf (ExpStmt vexp) env)
            (Build (conf NilStmt env))

    -- Control Flow
    , name "if-cong" $
    mkPairRule2 $ \env env' ->
    mkRule4 $ \e e' s t ->
        let (te, me', ms, mt) = (tv e, mv e', mv s, mv t) in
            StepTo (conf (If te ms mt) env)
            (LetStepTo (conf me' env') (conf te env)
            (Build $ conf (If me' ms mt) env'))

    , name "if-true" $
    mkPairRule1 $ \env ->
    mkRule2 $ \s t ->
        let (ms, mt) = (mv s, mv t) in
            StepTo (conf (If (BConst True) ms mt) env)
            (Build $ conf ms env)

    , name "if-false" $
    mkPairRule1 $ \env ->
    mkRule2 $ \s t ->
        let (ms, mt) = (mv s, mv t) in
            StepTo (conf (If (BConst False) ms mt) env)
            (Build $ conf mt env)

    , name "while" $
    mkPairRule1 $ \env ->
    mkRule2 $ \e s ->
        let (me, ms) = (mv e, mv s) in
            StepTo (conf (While me ms) env)
            (Build $ conf (If me (ConsStmt ms (While me ms)) NilStmt) env)

    -- Variable Things
    -- Read
    , name "var-lookup" $
    mkRule5 $ \var val frame rest h ->
        let (mvar, vval, mframe, mrest) = (mv var, vv val, mv frame, mv rest) in
            StepTo (Conf (Var mvar) (ConsFrame mframe mrest, WholeSimpEnv h))
            (Build $ Conf (FieldAccess (ReferenceVal mframe) mvar) (ConsFrame mframe mrest, WholeSimpEnv h))

    -- Write
    , name "generic-assn-cong" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \var e e' ->
        let (mvar, te, me') = (mv var, tv e, mv e') in
            StepTo (conf (Assign mvar te) env)
            (LetStepTo (conf me' env') (conf te env)
            (Build $ conf (Assign mvar me') env'))

    , name "stack-assn-eval" $
    mkRule6 $ \var val mu h frame rest->
        let (mvar, vval, mframe, mrest) = (mv var, vv val, mv frame, mv rest) in
            StepTo (Conf (Assign (Var mvar) vval) (ConsFrame mframe mrest, WholeSimpEnv h))
            (Build $ Conf (Assign (FieldAccess (ReferenceVal mframe) mvar) vval) (ConsFrame mframe mrest, WholeSimpEnv h))

    , name "field-assn-cong" $
    mkPairRule2 $ \env env' ->
    mkRule4 $ \field val re re'  ->
        let (mfield, vval, tre, mre) = (mv field, vv val, tv re, mv re') in
            StepTo (conf (Assign (FieldAccess tre mfield) vval) env)
            (LetStepTo (conf mre env') (conf tre env)
            (Build $ conf (Assign (FieldAccess mre mfield) vval) env'))

    , name "field-assn-eval-nonglobal" $
    mkRule7 $ \val field ref mu h re re'->
        let (vval, mref, mfield, vre, vre', mmu) = (vv val, mv ref, mv field, vv re, vv re', mv mu) in
            StepTo (Conf (Assign (FieldAccess (ReferenceVal mref) mfield) vval) (mmu, AssocOneVal h mref vre))
            (LetComputation (emptyConf (ReducedRecord vre')) (extComp WriteField (mmu, AssocOneVal h mref vre) [vre, mfield, vval])
            (Build $ Conf NilStmt (mmu, AssocOneVal h mref (ReducedRecord vre'))))

    , name "field-assn-eval-global" $
    mkRule7 $ \val field ref mu h re assign->
        let (vval, mref, mfield, vre, massign, mmu) = (vv val, mv ref, mv field, vv re, mv assign, mv mu) in
            StepTo (Conf (Assign (FieldAccess (ReferenceVal mref) mfield) vval) (mmu, AssocOneVal h mref vre))
            (LetComputation (emptyConf (Assign massign vval)) (extComp WriteField (mmu, AssocOneVal h mref vre) [vre, mfield, vval])
            (Build $ Conf (Assign massign vval) (mmu, AssocOneVal h mref vre)))

    , name "index-assn-cong-1" $
    mkPairRule2 $ \env env' ->
    mkRule4 $ \index val re re'  ->
        let (mindex, vval, tre, mre) = (mv index, vv val, tv re, mv re') in
            StepTo (conf (Assign (Index tre mindex) vval) env)
            (LetStepTo (conf mre env') (conf tre env)
            (Build $ conf (Assign (Index mre mindex) vval) env'))

    , name "index-assn-cong-2" $
    mkPairRule2 $ \env env' ->
    mkRule4 $ \index val re index'  ->
        let (tindex, vval, vre, mindex) = (tv index, vv val, vv re, mv index') in
            StepTo (conf (Assign (Index vre tindex) vval) env)
            (LetStepTo (conf mindex env') (conf tindex env)
            (Build $ conf (Assign (Index vre mindex) vval) env'))

    , name "index-assn-eval" $
    mkRule7 $ \val index ref mu h re re'->
        let (vval, mref, mindex, vre, vre', mmu) = (vv val, mv ref, mv index, vv re, vv re', mv mu) in
            StepTo (Conf (Assign (Index (ReferenceVal mref) mindex) vval) (mmu, AssocOneVal h mref vre))
            (LetComputation (emptyConf vre')
                (extComp WriteIndex (mmu, AssocOneVal h mref vre) [vre, mindex, vval])
                (Build $ Conf NilStmt (mmu, AssocOneVal h mref vre')))

    --- Arithmetic Operations
    , name "unary-cong" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \e1 e1' op ->
        let (te, me', mop) = (tv e1, mv e1', mv op) in
            StepTo (conf (UnExp mop te) env)
            (LetStepTo (conf me' env') (conf te env)
            (Build $ conf (UnExp mop me') env'))

    , name "unary-eval" $
    mkPairRule1 $ \env ->
    mkRule3 $ \v1 v' op ->
        let (vv1, vv', mop) = (vv v1, vv v', mv op) in
            StepTo (conf (UnExp mop vv1) env)
            (LetComputation (emptyConf $ ValVar v') (extComp Compute (matchRedState env) [mop, vv1])
            (Build $ conf vv' env))

    -- all binary operators are left-associative
    , name "binary-cong-left" $
    mkPairRule2 $ \env env' ->
    mkRule4 $ \e1 e2 e1' op ->
        let (te1, me2, me1', mop) = (tv e1, mv e2, mv e1', mv op) in
            StepTo (conf (BinExp te1 mop me2) env)
            (LetStepTo (conf me1' env') (conf te1 env)
            (Build $ conf (BinExp me1' mop me2) env'))

    , name "binary-cong-right" $
    mkPairRule2 $ \env env' ->
    mkRule4 $ \v1 e2 e2' op ->
        let (vv1, te2, me2', mop) = (vv v1, tv e2, mv e2', mv op) in
            StepTo (conf (BinExp vv1 mop te2) env)
            (LetStepTo (conf me2' env') (conf te2 env)
            (Build $ conf (BinExp vv1 mop me2') env'))

    , name "binary-eval" $
    mkPairRule1 $ \env ->
    mkRule4 $ \v1 v2 v' op ->
        let (vv1, vv2, vv', mop) = (vv v1, vv v2, vv v', mv op) in
            StepTo (conf (BinExp vv1 mop vv2) env)
            (LetComputation (emptyConf $ ValVar v') (extComp Compute (matchRedState env) [mop, vv1, vv2])
            (Build $ conf vv' env))

    -- Heap Stuff
    , name "heap-alloc-cong" $
    mkPairRule2 $ \env env' ->
    mkRule2 $ \val val' ->
        let (tval, mval) = (tv val, mv val') in
            StepTo (conf (HeapAlloc tval) env)
            (LetStepTo (conf mval env') (conf tval env)
            (Build $ conf (HeapAlloc mval) env'))

    , name "heap-alloc-eval" $
    mkPairRule1 $ \env ->
    mkRule4 $ \h mu val ref ->
        let (vval, mref, mmu) = (vv val, mv ref, mv mu) in
            StepTo (Conf (HeapAlloc vval) (mmu, WholeSimpEnv h))
            (LetComputation (emptyConf mref) (extComp AllocAddress (matchRedState (mu, h)) [NilStmt])
            (Build $ Conf (ReferenceVal mref) (mmu, AssocOneVal h mref vval)))

    -- Record Literals -> Runtime Records
    , name "record-cong" $
    mkPairRule2 $ \env env' ->
    mkRule2 $ \r r' ->
        let (tr, mr') = (tv r, mv r') in
            StepTo (conf (Record tr) env)
            (LetStepTo (conf mr' env') (conf tr env)
            (Build $ conf (Record mr') env'))

    , name "record-eval" $
    mkPairRule2 $ \env env' ->
    mkRule1 $ \r ->
        let vr = vv r in
            StepTo (conf (Record vr) env)
            (Build $ conf (HeapAlloc (ReducedRecord vr)) env)

    , name "cons-record-pair-cong-car" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \r r' rs ->
        let (tr, mr', mrs) = (tv r, mv r', mv rs) in
            StepTo (conf (ConsRecordPair tr mrs) env)
            (LetStepTo (conf mr' env') (conf tr env)
            (Build $ conf (ConsRecordPair mr' mrs) env'))

    , name "cons-record-pair-cong-cdr" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \r rs rs' ->
        let (vr, trs, mrs') = (vv r, tv rs, mv rs') in
            StepTo (conf (ConsRecordPair vr trs) env)
            (LetStepTo (conf mrs' env') (conf trs env)
            (Build $ conf (ConsRecordPair vr mrs') env'))

    , name "cons-record-pair-eval" $
    mkPairRule1 $ \env ->
    mkRule2 $ \r rs ->
        let (vr, vrs) = (vv r, vv rs) in
            StepTo (conf (ConsRecordPair vr vrs) env)
            (Build $ conf (ReducedRecordCons vr vrs) env)

    , name "record-pair-cong" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \k v v' ->
        let (mk, tvv, mv') = (mv k, tv v, mv v') in
            StepTo (conf (RecordPair mk tvv) env)
            (LetStepTo (conf mv' env') (conf tvv env)
            (Build $ conf (RecordPair mk mv') env'))

    , name "record-pair-eval" $
    mkPairRule1 $ \env ->
    mkRule2 $ \key val ->
        let (mkey, vval) = (mv key, vv val) in
            StepTo (conf (RecordPair mkey vval) env)
            (Build $ conf (ReducedRecordPair mkey vval) env)

    , name "nil-record-pair-eval" $
    mkPairRule1 $ \env ->
    mkRule0 $
            StepTo (conf NilRecordPair env)
            (Build $ conf ReducedRecordNil env)

    -- Index Access
    , name "index-cong-1" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \r i i' ->
        let (mr, ti, mi) = (mv r, tv i, mv i') in
            StepTo (conf (Index mr ti) env)
            (LetStepTo (conf mi env') (conf ti env)
            (Build $ conf (Index mr mi) env'))

    , name "index-cong-2" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \r r' i ->
        let (tr, mr', vi) = (tv r, mv r', vv i) in
            StepTo (conf (Index tr vi) env)
            (LetStepTo (conf mr' env') (conf tr env)
            (Build $ conf (Index mr' vi) env'))

    , name "index-eval" $
    mkRule6 $ \r i v ref mu h ->
        let (vr, vi, v', mref, mmu) = (vv r, vv i, vv v, mv ref, mv mu) in
            StepTo (Conf (Index (ReferenceVal mref) vi) (mmu, AssocOneVal h mref vr))
            (LetComputation (emptyConf v') (extComp ReadIndex (mmu, AssocOneVal h mref vr) [vr, vi])
            (Build $ Conf v' (mmu, AssocOneVal h mref vr)))

    -- Field Access
    , name "field-cong" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \r r' f ->
        let (tr, mr, mf) = (tv r, mv r', mv f) in
            StepTo (conf (FieldAccess tr mf) env)
            (LetStepTo (conf mr env') (conf tr env)
            (Build $ conf (FieldAccess mr mf) env'))

    , name "field-eval" $
    mkRule6 $ \r f v ref mu h ->
        let (vr, mf, v', mref, mmu) = (vv r, mv f, vv v, mv ref, mv mu) in
            StepTo (Conf (FieldAccess (ReferenceVal mref) mf) (mmu, AssocOneVal h mref vr))
            (LetComputation (emptyConf v') (extComp ReadField (mmu, AssocOneVal h mref vr) [vr, mf])
            (Build $ Conf v' (mmu, AssocOneVal h mref vr)))

    -- Functions
    , name "fun-decl-eval" $
    mkRule5 $ \params body frame rest h->
        let (mparams, mbody, mframe, mrest) = (mv params, mv body, mv frame, mv rest) in
            StepTo (Conf (FunDecl mparams mbody) (ConsFrame mframe mrest, WholeSimpEnv h) )
                (Build $ Conf (HeapAlloc (Closure mparams mbody mframe)) (ConsFrame mframe mrest, WholeSimpEnv h))

    , name "fun-call-cong-fun" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \fun fun' args ->
        let (tfun, mfun, margs) = (tv fun, mv fun', mv args) in
            StepTo (conf (FunCall tfun margs) env)
                (LetStepTo (conf mfun env') (conf tfun env)
                (Build $ conf (FunCall mfun margs) env'))

    , name "fun-call-cong-args" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \fun args args' ->
        let (vfun, targs, margs) = (vv fun, tv args, mv args') in
            StepTo (conf (FunCall vfun targs) env)
                (LetStepTo (conf margs env') (conf targs env)
                (Build $ conf (FunCall vfun margs) env'))

    , name "fun-call-cong-deref" $
    mkRule5 $ \ref args fun mu h ->
        let (mref, vargs, mmu, mfun) = (mv ref, vv args, mv mu, mv fun) in
            StepTo (Conf (FunCall (ReferenceVal mref) vargs) (mmu, AssocOneVal h mref mfun))
                    (Build $ Conf (FunCall mfun vargs) (mmu, AssocOneVal h mref mfun))

    , name "fun-call-cong-alloc-frame" $
    mkRule7 $ \params body frame mu ref h args ->
        let (mparams, mbody, mframe, mmu, mref, vargs) = (mv params, mv body, mv frame, mv mu, mv ref, vv args) in
            StepTo (Conf (FunCall (Closure mparams (Block mbody) mframe) vargs) (mmu, WholeSimpEnv h))
                   (LetComputation (emptyConf mref) (extComp AllocAddress (mmu, WholeSimpEnv h) [NilStmt])
                      (Build $ Conf (Scope (Block (ConsStmt (Block mbody) (Return None))) mparams vargs)
                                    (ConsFrame mref mmu, AssocOneVal h mref (ReducedRecord $ Parent mframe))))

    , name "fun-call-cong-assign-args" $
    mkPairRule1 $ \env->
    mkRule5 $ \param params body arg args ->
        let (mparam, mparams, mbody, varg, vargs) = (mv param, mv params, mv body, vv arg, vv args) in
            StepTo (conf (Scope (Block mbody) (ConsName mparam mparams) (ReducedConsExp varg vargs)) env)
                (Build $ conf (Scope (Block (ConsStmt (Assign (Var mparam) varg) mbody)) mparams vargs) env)

    , name "fun-call-cong-body" $
    mkPairRule2 $ \env env'->
    mkRule2 $ \body body' ->
        let (tbody, mbody') = (tv body, mv body') in
            StepTo (conf (Scope tbody NilName ReducedNilExp) env)
                   (LetStepTo (conf mbody' env') (conf tbody env)
                      (Build $ conf (Scope mbody' NilName ReducedNilExp) env'))

    , name "fun-call-eval" $
    mkRule4 $ \result mu rest h ->
        let (vresult, mmu, mrest) = (vv result, mv mu, mv rest) in
            StepTo (Conf (Scope (Return vresult) NilName ReducedNilExp) (ConsFrame mmu mrest, WholeSimpEnv h))
                (Build $ Conf vresult (mrest, WholeSimpEnv h))

    -- Function argument lists
    , name "cons-expr-cong-car" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \e e' rest ->
        let (te, me', mrest) = (tv e, mv e', mv rest) in
            StepTo (conf (ConsExp te mrest) env)
                (LetStepTo (conf me' env') (conf te env)
                    (Build $ conf (ConsExp me' mrest) env'))

    , name "cons-expr-cong-cdr" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \e rest rest' ->
        let (ve, trest, mrest') = (vv e, tv rest, mv rest') in
            StepTo (conf (ConsExp ve trest) env)
                (LetStepTo (conf mrest' env') (conf trest env)
                    (Build $ conf (ConsExp ve mrest') env'))

    , name "cons-expr-eval" $
    mkPairRule1 $ \env ->
    mkRule2 $ \e rest ->
        let (ve, vrest) = (vv e, vv rest) in
            StepTo (conf (ConsExp ve vrest) env)
                    (Build $ conf (ReducedConsExp ve vrest) env)

    , name "nil-cons-expr-eval" $
    mkPairRule1 $ \env ->
    mkRule0 $
            StepTo (conf NilExp env)
                (Build $ conf ReducedNilExp env)

    -- Bultins
    , name "builtin-cong" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \var ret func ->
        let (tvar, mret, mfunc) = (tv var, mv ret, mv func) in
            StepTo (conf (Builtin mfunc tvar) env)
                (LetStepTo (conf mret env') (conf tvar env)
                    (Build $ conf (Builtin mfunc mret) env'))

    , name "builtin-eval" $
    mkPairRule1 $ \env ->
    mkRule3 $ \var ret func ->
        let (vvar, mret, mfunc) = (vv var, mv ret, mv func) in
            StepTo (conf (Builtin mfunc vvar) env)
                (LetComputation (emptyConf mret) (extComp RunBuiltin (matchRedState env) [mfunc, vvar])
                    (Build $ conf mret env))

    ]

ibsToString :: InternedByteString -> String
ibsToString = BS.unpack . unintern

stringToIbs :: String -> InternedByteString
stringToIbs = intern . BS.pack

isGlobal :: SimpEnv (Term MITScript) (Term MITScript) -> Term MITScript -> String -> Bool
isGlobal heap (ReducedRecordCons (ReducedRecordPair (Name k) v) rps) field =
    if ibsToString k == field then
        case v of
            GlobalVar -> Prelude.True
            _ -> Prelude.False
        else isGlobal heap rps field
isGlobal heap (Parent p) f = case Configuration.lookup p heap of
                                    Just parent -> isGlobal heap parent f
                                    Nothing -> Prelude.False
isGlobal _ _ _ = Prelude.False

readField :: SimpEnv (Term MITScript) (Term MITScript) -> Term MITScript -> String -> Term MITScript
readField heap x y = if isGlobal heap x y then
                        case Configuration.lookup (HeapAddr 0) heap of
                                    Just (ReducedRecord parent) -> readField heap parent y
                                    Nothing -> error "ERR: Missing Global Frame"
                     else readField' heap x y
    where
        readField' heap (ReducedRecordCons (ReducedRecordPair (Name k) v) rps) field = if ibsToString k == field then v else readField' heap rps field
        readField' heap (Parent p) f = case Configuration.lookup p heap of
                                            Just (ReducedRecord parent) -> readField heap parent f
                                            Nothing -> error "ERR: Dangling Pointer"
        readField' heap ReducedRecordNil field = None
        readField' heap item field = error (show heap ++ "\n\t" ++ show item ++ "\n\t" ++ show field)

writeField :: SimpEnv (Term MITScript) (Term MITScript) -> Term MITScript -> String -> Term MITScript -> Term MITScript
writeField heap x y z = if isGlobal heap x y then
                            Assign (FieldAccess (ReferenceVal (HeapAddr 0)) (Name (stringToIbs y))) z
                        else
                            ReducedRecord (writeField' x y z)
    where
        writeField' (ReducedRecordCons (ReducedRecordPair (Name k) v) rps) field val =
            if ibsToString k == field then ReducedRecordCons (ReducedRecordPair (Name k) val) rps
                                      else ReducedRecordCons (ReducedRecordPair (Name k) v) (writeField' rps field val)
        writeField' ReducedRecordNil field val = ReducedRecordCons (ReducedRecordPair (Name (stringToIbs field)) val) ReducedRecordNil
        writeField' (Parent p) field val       = ReducedRecordCons (ReducedRecordPair (Name (stringToIbs field)) val) (Parent p)

toMetaBool :: Term MITScript -> Bool
toMetaBool True = Prelude.True
toMetaBool False = Prelude.False

fromMetaBool :: Bool -> Term MITScript
fromMetaBool Prelude.True = True
fromMetaBool Prelude.False = False

toString :: Term MITScript -> SimpEnv (Term MITScript) (Term MITScript) -> String
toString (BConst b) heap = show $ toMetaBool b
toString (NumConst (ConstInt n1)) heap = show n1
toString (Str (ConstStr s)) heap = ibsToString s
toString None heap = "None"
toString Closure {} heap = "FUNCTION"
toString (ReducedRecord rs) heap = "{" ++ toString rs heap ++ "}"
toString (ReducedRecordCons rp rps) heap = toString rp heap ++ toString rps heap
toString ReducedRecordNil heap = ""
toString (ReducedRecordPair (Name n) v) heap = ibsToString n ++ ":" ++ toString v heap ++ " "
toString (ReferenceVal p) heap = case Configuration.lookup p heap of
                                                Just term -> toString term heap
                                                Nothing -> "ERR: Dangling Pointer"

returnInt :: Monad m => Integer -> m (Configuration MITScript)
returnInt x = return $ emptyConf $ NumConst $ ConstInt x

returnBool :: Monad m => Bool -> m (Configuration MITScript)
returnBool x = return $ emptyConf $ BConst $ fromMetaBool x

returnString :: Monad m => String -> m (Configuration MITScript)
returnString x = return $ emptyConf $ Str $ ConstStr $ fromString x

matchRedState :: (MetaVar, MetaVar) -> RedState MITScript
matchRedState (stack, heap) = (mv stack, WholeSimpEnv heap)

builtinPrint :: Term MITScript
builtinPrint = Closure
                    (ConsName (Name "x") NilName)
                    (Block (ConsStmt (Return (Builtin Print (Var $ Name "x"))) NilStmt))
                    (HeapAddr $ -1)

builtinIntCast :: Term MITScript
builtinIntCast = Closure
                    (ConsName (Name "x") NilName)
                    (Block (ConsStmt (Return (Builtin IntCast (Var $ Name "x"))) NilStmt))
                    (HeapAddr $ -1)

builtinRead :: Term MITScript
builtinRead = Closure
                    NilName
                    (Block (ConsStmt (Return (Builtin Read None)) NilStmt))
                    (HeapAddr $ -1)

runExternalComputation :: CompFunc MITScript -> RedState MITScript -> [Term MITScript] -> MatchEffect (Configuration MITScript)
runExternalComputation Compute state [UMINUS, NumConst (ConstInt n1)] = returnInt $ negate n1
runExternalComputation Compute state [NOT, BConst b]                  = returnBool $ not $ toMetaBool b

runExternalComputation Compute state [PLUS,  NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnInt $ n1 + n2
runExternalComputation Compute (stack, heap) [PLUS,  vv1@(Str (ConstStr s1)), vv2]           = returnString $ toString vv1 heap ++ toString vv2 heap
runExternalComputation Compute (stack, heap) [PLUS,  vv1, vv2@(Str (ConstStr s1))]           = returnString $ toString vv1 heap ++ toString vv2 heap

runExternalComputation Compute state [MINUS, NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnInt $ n1 - n2
runExternalComputation Compute state [TIMES, NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnInt $ n1 * n2
runExternalComputation Compute state [DIV,   NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnInt $ n1 `div` n2

runExternalComputation Compute state [GT,  NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnBool $ n1 > n2
runExternalComputation Compute state [GTE, NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnBool $ n1 >= n2

runExternalComputation Compute state [EQ, NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnBool $ n1 == n2
runExternalComputation Compute state [EQ, BConst l, BConst r]                             = returnBool $ l == r
runExternalComputation Compute state [EQ, None, None]                                     = returnBool Prelude.True
runExternalComputation Compute state [EQ, ReferenceVal p1, ReferenceVal  p2]              = returnBool $ p1 == p2
runExternalComputation Compute (stack, heap) [EQ, vv1@(Str (ConstStr s1)), vv2@(Str (ConstStr s2))] = returnBool $ toString vv1 heap == toString vv2 heap
runExternalComputation Compute state [EQ, _, _]                                           = returnBool Prelude.False

runExternalComputation Compute state [AND, BConst True, BConst True] = returnBool Prelude.True
runExternalComputation Compute state [AND, BConst l, BConst r]       = returnBool Prelude.False

runExternalComputation Compute state [OR, BConst False, BConst False] = returnBool Prelude.False
runExternalComputation Compute state [OR, BConst l, BConst r]         = returnBool Prelude.True

runExternalComputation AllocAddress (stack, heap) _ = return $ emptyConf (HeapAddr $ size heap)

runExternalComputation ReadIndex (stack, heap)  [ReducedRecord r, i]      = return $ emptyConf $ readField heap r (toString i heap)
runExternalComputation WriteIndex (stack, heap) [ReducedRecord r, i, val] = return $ emptyConf $ writeField heap r (toString i heap) val

runExternalComputation ReadField (stack, heap)  [ReducedRecord r, Name f] = return $ emptyConf $ readField heap r (ibsToString f)
runExternalComputation WriteField (stack, heap) [ReducedRecord r, Name f, val]   = return $ emptyConf $ writeField heap r (ibsToString f) val

runExternalComputation RunBuiltin state         [Read, None]  = emptyConf <$> Str <$> ConstStr <$> intern <$> matchEffectInput
runExternalComputation RunBuiltin (stack, heap) [Print, vv]   = matchEffectOutput (BS.pack $ fromString $ toString vv heap) >> return (emptyConf None)
runExternalComputation RunBuiltin (stack, heap) [IntCast, vv] = returnInt $ read $ toString vv heap

runExternalComputation AbsAllocAddress _ _ = return $ emptyConf ValStar

runExternalComputation func state [GStar _] = return $ emptyConf ValStar
runExternalComputation func state [_]       = return $ emptyConf ValStar

runExternalComputation func state [GStar _, _] = return $ emptyConf ValStar
runExternalComputation func state [_, GStar _] = return $ emptyConf ValStar

runExternalComputation func state [GStar _, _, _] = return $ emptyConf ValStar
runExternalComputation func state [_, GStar _, _] = return $ emptyConf ValStar
runExternalComputation func state [_, _, GStar _] = return $ emptyConf ValStar
runExternalComputation func state terms = error $ show func ++ "\t" ++ show state ++ "\t" ++ show terms