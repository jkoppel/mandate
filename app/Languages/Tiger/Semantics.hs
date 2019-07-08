{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.Tiger.Semantics (
    Tiger
  ) where


import Control.Monad ( mzero )

import Data.Char ( ord, chr )
import qualified Data.Map as Map
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Interned ( unintern, intern )
import Data.Interned.ByteString ( InternedByteString(..) )
import Data.Hashable ( Hashable )
import Data.String ( fromString )

import GHC.Generics ( Generic )

import Configuration
import Lang
import Matching
import Semantics.Abstraction
import Semantics.Conversion
import Semantics.General
import Semantics.GraphPattern
import Semantics.PAM
import Semantics.SOS
import Term hiding ( Symbol )
import Var

import Languages.Tiger.Parse
import Languages.Tiger.Signature
import Languages.Tiger.Translate

instance Lang Tiger where

        type RedState Tiger = (Term Tiger, SimpEnv (Term Tiger) (Term Tiger))

        data CompFunc Tiger = ReadField    | AbsReadField
                            | WriteField   | AbsWriteField
                            | AllocAddress | AbsAllocAddress
                            | Compute      | AbsCompute
                            | WriteIndex   | AbsWriteIndex
                            | ReadIndex    | AbsReadIndex
                            | MakeArray    | AbsMakeArray
                            | RunBuiltin   | AbsRunBuiltin
                            | ValIsTrue    | AbsValIsTrue

                            | OpIsntShortCircuit
            deriving ( Eq, Generic )

        compFuncName ReadField    = "readField"
        compFuncName WriteField   = "writeField"
        compFuncName AllocAddress = "allocAddress"
        compFuncName Compute      = "compute"
        compFuncName WriteIndex   = "writeIndex"
        compFuncName ReadIndex    = "readIndex"
        compFuncName MakeArray    = "makeArray"
        compFuncName RunBuiltin   = "runBuiltin"
        compFuncName ValIsTrue    = "valIsTrue"

        compFuncName AbsReadField    = "absreadField"
        compFuncName AbsWriteField   = "abswriteField"
        compFuncName AbsAllocAddress = "absallocAddress"
        compFuncName AbsCompute      = "abscompute"
        compFuncName AbsWriteIndex   = "abswriteIndex"
        compFuncName AbsMakeArray    = "absMakeArray"
        compFuncName AbsReadIndex    = "absreadIndex"
        compFuncName AbsRunBuiltin   = "absrunBuiltin"
        compFuncName AbsValIsTrue    = "absvalIsTrue"

        compFuncName OpIsntShortCircuit = "opIsntShortCircuit"

        runCompFunc func (c:cs)  = runExternalComputation func (confState c) (map confTerm (c:cs))

        signature = tigerSig

        initConf t = Conf t (ConsFrame (HeapAddr 0) NilFrame,
                             realStartingEnv)


instance Hashable (CompFunc Tiger)

instance Irrelevance (CompFunc Tiger) where
    irrelevance _ ReadField    = AbsReadField
    irrelevance _ WriteField   = AbsWriteField
    irrelevance _ AllocAddress = AbsAllocAddress
    irrelevance _ Compute      = AbsCompute
    irrelevance _ ReadIndex    = AbsReadIndex
    irrelevance _ WriteIndex   = AbsWriteIndex
    irrelevance _ MakeArray    = AbsMakeArray
    irrelevance _ RunBuiltin   = AbsRunBuiltin
    irrelevance _ ValIsTrue    = AbsValIsTrue

    irrelevance _ AbsReadField    = AbsReadField
    irrelevance _ AbsAllocAddress = AbsAllocAddress
    irrelevance _ AbsCompute      = AbsCompute
    irrelevance _ AbsReadIndex    = AbsReadIndex
    irrelevance _ AbsWriteIndex   = AbsWriteIndex
    irrelevance _ AbsMakeArray    = AbsMakeArray
    irrelevance _ AbsRunBuiltin   = AbsRunBuiltin
    irrelevance _ AbsValIsTrue    = AbsValIsTrue

    irrelevance _ OpIsntShortCircuit = OpIsntShortCircuit

instance HasTopState Tiger where
  topRedState = (ValStar, JustSimpMap $ SingletonSimpMap Star ValStar)

  normalizeTopState (stack, AssocOneVal h Star ValStar) = (stack, JustSimpMap $ SingletonSimpMap Star ValStar)
  normalizeTopState x                                     = x

realStartingEnv :: SimpEnv (Term Tiger) (Term Tiger)
realStartingEnv = JustSimpMap $ SimpEnvMap $ Map.fromList
                      [
                        (HeapAddr 0, ReducedRecord
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "print")     builtinPrint)
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "flush")     builtinFlush)
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "getchar")   builtinGetChar)
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "ord")       builtinOrd)
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "chr")       builtinChr)
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "size")      builtinSize)
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "substring") builtinSubstring)
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "concat")    builtinConcat)
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "not")       builtinNot)
                                         $ ReducedRecordCons (ReducedRecordPair (Symbol "exit")      builtinExit)
                                         $ Parent $ HeapAddr $ -1)
                      ]

reducedStartingEnv :: SimpEnv (Term Tiger) (Term Tiger)
reducedStartingEnv = JustSimpMap $ SingletonSimpMap
                                     (HeapAddr 0)
                                     (ReducedRecord $ Parent $ HeapAddr (-1))

instance HasSOS Tiger where
    rules = tigerRules

emptyConf :: Term Tiger -> Configuration Tiger
emptyConf t = Conf t (ReducedRecordNil, EmptySimpEnv)

conf :: Term Tiger -> (MetaVar, MetaVar) -> Configuration Tiger
conf t (returnStack, heap) = Conf t (mv returnStack, WholeSimpEnv heap)

vv :: MetaVar -> Term Tiger
vv = ValVar

tv :: MetaVar -> Term Tiger
tv = NonvalVar

mv :: MetaVar -> Term Tiger
mv = MetaVar

loopHiSym :: Term Tiger
loopHiSym = Symbol "___for_loop_hi_bound"

tigerRules :: IO (NamedRules Tiger)
tigerRules = sequence [

      --- PExp
      name "pexp-cong" $
      mkPairRule2 $ \env env' ->
      mkRule2 $ \es es' ->
        let (tes, mes') = (tv es, mv es') in
          StepTo (conf (PExp tes) env)
            (LetStepTo (conf mes' env') (conf tes env)
              (Build (conf (PExp mes') env')))

    , name "pexp-done" $
      mkPairRule1 $ \env ->
      mkRule1 $ \es ->
        let ves = vv es in
          StepTo (conf (PExp ves) env)
            (Build (conf ves env))


      -- PDecs
    , name "pdecs-cong" $
      mkPairRule2 $ \env env' ->
      mkRule2 $ \es es' ->
        let (tes, mes') = (tv es, mv es') in
          StepTo (conf (PDecs tes) env)
            (LetStepTo (conf mes' env') (conf tes env)
              (Build (conf (PDecs mes') env')))

    , name "pdecs-done" $
      mkPairRule1 $ \env ->
      mkRule1 $ \es ->
        let ves = vv es in
          StepTo (conf (PDecs ves) env)
            (Build (conf ves env))

      ---- Vars

    , name "var-lookup" $
      mkRule4 $ \var frame rest h ->
        let (mvar, mframe, mrest) = (mv var, mv frame, mv rest) in
          StepTo (Conf (SimpleVar mvar) (ConsFrame mframe mrest, WholeSimpEnv h))
            (Build $ Conf (FieldVar (ReferenceVal mframe) mvar) (ConsFrame mframe mrest, WholeSimpEnv h))

    , name "field-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \x x' s ->
        let (tx, mx', ms) = (tv x, mv x', mv s) in
          StepTo (conf (FieldVar tx ms) env)
            (LetStepTo (conf mx' env') (conf tx env)
              (Build (conf (FieldVar mx' ms) env')))

    , name "field-read" $
      mkRule6 $ \ref sym stack h fr val ->
        let (mref, msym, mstack, vfr, vval) = (mv ref, mv sym, mv stack, vv fr, vv val) in
          StepTo (Conf (FieldVar (ReferenceVal mref) msym) (mstack, AssocOneVal h mref vfr))
            (LetComputation (emptyConf vval) (extComp ReadField (mstack, AssocOneVal h mref vfr) [vfr, msym])
              (Build $ Conf vval (mstack, AssocOneVal h mref vfr)))

    , name "subscript-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \x x' s ->
        let (tx, mx', ms) = (tv x, mv x', mv s) in
          StepTo (conf (SubscriptVar tx ms) env)
            (LetStepTo (conf mx' env') (conf tx env)
              (Build (conf (SubscriptVar mx' ms) env')))

    , name "subscript-cong-index" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \arr e e' ->
        let (varr, te, me') = (vv arr, tv e, mv e') in
          StepTo (conf (SubscriptVar varr te) env)
            (LetStepTo (conf me' env') (conf te env)
              (Build (conf (SubscriptVar varr me') env')))

    , name "subscript-read" $
      mkRule6 $ \ref i stack h fr val ->
        let (mref, vi, mstack, vfr, vval) = (mv ref, vv i, mv stack, vv fr, vv val) in
          StepTo (Conf (SubscriptVar (ReferenceVal mref) vi) (mstack, AssocOneVal h mref vfr))
            (LetComputation (emptyConf vval) (extComp ReadIndex (mstack, AssocOneVal h mref vfr) [vfr, vi])
              (Build $ Conf vval (mstack, AssocOneVal h mref vfr)))

      ---- Seq

    , name "seq-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \s1 s2 s1' ->
          let (ts1, ms2, ms1') = (tv s1, mv s2, mv s1') in
              StepTo (conf (Seq ts1 ms2) env)
                (LetStepTo (conf ms1' env')(conf ts1 env)
                  (Build (conf (Seq ms1' ms2) env')))

    , name "seq-next" $
      mkPairRule1 $ \env ->
      mkRule2 $ \val ss ->
        let (vval, mss) = (vv val, mv ss) in
            StepTo (conf (Seq vval mss) env)
                (Build (conf mss env))


    , name "seq-break" $
      mkPairRule1 $ \env ->
      mkRule1 $ \s ->
        let ms = mv s in
            StepTo (conf (Seq BreakExp ms) env)
                (Build (conf BreakExp env))


    , name "seq-exit" $
      mkPairRule1 $ \env ->
      mkRule2 $ \s val ->
        let (ms, mval) = (mv s, mv val) in
            StepTo (conf (Seq (DoExit mval) ms) env)
                (Build (conf (DoExit mval) env))

      --- VarExp

    , name "varexp-cong" $
      mkPairRule2 $ \env env' ->
      mkRule2 $ \x x' ->
        let (tx, mx') = (tv x, mv x') in
          StepTo (conf (VarExp tx) env)
            (LetStepTo (conf mx' env') (conf tx env)
              (Build (conf (VarExp mx') env')))

    , name "varexp-done" $
      mkPairRule1 $ \env ->
      mkRule1 $ \val ->
        let vval = vv val in
          StepTo (conf (VarExp vval) env)
            (Build (conf vval env))

      ---- App: Explist

    , name "explist-cong-1" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \e1 e1' es ->
        let (te1, me1', mes) = (tv e1, mv e1', mv es) in
          StepTo (conf (ConsExpList te1 mes) env)
            (LetStepTo (conf me1' env') (conf te1 env)
              (Build (conf (ConsExpList me1' mes) env')))

    , name "explist-cong-2" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \e1 es es' ->
        let (ve1, tes, mes') = (vv e1, tv es, mv es') in
          StepTo (conf (ConsExpList ve1 tes) env)
            (LetStepTo (conf mes' env') (conf tes env)
              (Build (conf (ConsExpList ve1 mes') env')))

    , name "explist-nil" $
      mkPairRule1 $ \env ->
      mkRule0 $
        StepTo (conf NilExpList env) (Build $ conf ReducedNilExp env)

    , name "explist-eval" $
      mkPairRule1 $ \env ->
      mkRule2 $ \e1 es ->
        let (ve1, ves) = (vv e1, vv es) in
          StepTo (conf (ConsExpList ve1 ves) env) (Build $ conf (ReducedConsExp ve1 ves) env)

      -- App: The real part

    , name "appexp-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \fn args args' ->
        let (mfn, targs, margs') = (mv fn, tv args, mv args') in
          StepTo (conf (AppExp mfn targs) env)
            (LetStepTo (conf margs' env') (conf targs env)
              (Build (conf (AppExp mfn margs') env')))

    , name "app-cong-fn" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \fn fn' args ->
        let (tfn, mfn', vargs) = (tv fn, mv fn', vv args) in
          StepTo (conf (AppExp tfn vargs) env)
            (LetStepTo (conf mfn' env') (conf tfn env)
              (Build (conf (AppExp mfn' vargs) env')))

    , name "app-enter" $
      mkRule6 $ \params body addr args stack h ->
        let (mparams, mbody, maddr, vargs, mstack) = (mv params, mv body, mv addr, vv args, mv stack) in
          StepTo (Conf (AppExp (Closure mparams mbody maddr) vargs) (mstack, WholeSimpEnv h))
            (Build (Conf (Scope (LetExp (AssnFnArgs mparams vargs)
                                        mbody))
                         (ConsFrame maddr mstack, WholeSimpEnv h)))

    , name "assn-fn-args-1" $
      mkPairRule1 $ \env ->
      mkRule5 $ \nm ty ps a as ->
        let (mnm, mty, mps, va, vas) = (mv nm, mv ty, mv ps, vv a, vv as) in
          StepTo (conf (AssnFnArgs (ConsTField (TField mnm mty) mps) (ReducedConsExp va vas)) env)
            (Build (conf (ConsDecList (VarDecDec (VarDec mnm) (JustSym mty) va) (AssnFnArgs mps vas)) env))

    , name "assn-fn-args-done" $
      mkPairRule1 $ \env ->
      mkRule0 $
        StepTo (conf (AssnFnArgs NilTField ReducedNilExp) env)
          (Build (conf NilDecList env))

      ---- Op's

    , name "op-cong-left" $
      mkPairRule2 $ \env env' ->
      mkRule4 $ \e1 e2 e1' op ->
        let (te1, me2, me1', mop) = (tv e1, mv e2, mv e1', mv op) in
          StepTo (conf (OpExp te1 mop me2) env)
            (LetStepTo (conf me1' env') (conf te1 env)
              (Build (conf (OpExp me1' mop me2) env')))

    , name "op-cong-right" $
      mkPairRule2 $ \env env' ->
      mkRule4 $ \e1 e2 e2' op->
        let (ve1, te2, me2', mop) = (vv e1, tv e2, mv e2', mv op) in
          StepTo (conf (OpExp ve1 mop te2) env)
            (LetComputation (emptyConf NilExp) (extComp OpIsntShortCircuit (matchRedState env) [mop])
              (LetStepTo (conf me2' env') (conf te2 env)
                (Build (conf (OpExp ve1 mop me2') env'))))

    , name "op-eval" $
      mkPairRule1 $ \env ->
      mkRule4 $ \v1 v2 v' op ->
        let (vv1, vv2, vv', mop) = (vv v1, vv v2, vv v', mv op) in
          StepTo (conf (OpExp vv1 mop vv2) env)
            (LetComputation (emptyConf vv') (extComp Compute (matchRedState env) [mop, vv1, vv2])
              (Build (conf vv' env)))

    , name "and-0" $
      mkPairRule1 $ \env ->
      mkRule1 $ \e2 ->
        let me2 = mv e2 in
          StepTo (conf (OpExp (IntExp (ConstInt 0)) AndOp me2) env)
            (Build (conf (IntExp (ConstInt 0)) env))

    , name "or-0" $
      mkPairRule1 $ \env ->
      mkRule1 $ \e2 ->
        let me2 = mv e2 in
          StepTo (conf (OpExp (IntExp (ConstInt 0)) OrOp me2) env)
            (Build (conf me2 env))

    , name "and-true" $
      mkPairRule1 $ \env ->
      mkRule2 $ \v1 e2 ->
        let (vv1, me2) = (vv v1, mv e2) in
        StepTo (conf (OpExp vv1 AndOp me2) env)
          (LetComputation (emptyConf NilExp) (extComp ValIsTrue (matchRedState env) [vv1])
            (Build (conf me2 env)))

    --- FIXME: I'm not sure if it should return v1 or (ConstInt 1). For CFG purposes, doesn't matter
    , name "or-true" $
      mkPairRule1 $ \env ->
      mkRule2 $ \v1 e2 ->
        let (vv1, me2) = (vv v1, mv e2) in
        StepTo (conf (OpExp vv1 OrOp me2) env)
          (LetComputation (emptyConf NilExp) (extComp ValIsTrue (matchRedState env) [vv1])
            (Build (conf vv1 env)))

      ---- Heap allocation

    , name "heap-alloc-eval" $
      mkPairRule1 $ \env ->
      mkRule4 $ \h mu val ref ->
        let (vval, mref, mmu) = (vv val, mv ref, mv mu) in
          StepTo (Conf (HeapAlloc vval) (mmu, WholeSimpEnv h))
            (LetComputation (emptyConf (ReferenceVal mref)) (extComp AllocAddress (matchRedState (mu, h)) [NilExp])
              (Build $ Conf (ReferenceVal mref) (mmu, AssocOneVal h mref vval)))

      ---- Record Exp

    , name "record-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \r r' s ->
        let (tr, mr', ms) = (tv r, mv r', mv s) in
            StepTo (conf (RecordExp tr ms) env)
            (LetStepTo (conf mr' env') (conf tr env)
              (Build $ conf (RecordExp mr' ms) env'))

    , name "record-eval" $
      mkPairRule2 $ \env env' ->
      mkRule2 $ \r s ->
        let (vr, ms) = (vv r, mv s) in
            StepTo (conf (RecordExp vr ms) env)
              (Build $ conf (HeapAlloc (ReducedRecord vr)) env)

    , name "cons-efield-cong-car" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \r r' rs ->
        let (tr, mr', mrs) = (tv r, mv r', mv rs) in
          StepTo (conf (ConsEField tr mrs) env)
            (LetStepTo (conf mr' env') (conf tr env)
              (Build $ conf (ConsEField mr' mrs) env'))

    , name "cons-efield-cong-cdr" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \r rs rs' ->
        let (vr, trs, mrs') = (vv r, tv rs, mv rs') in
          StepTo (conf (ConsEField vr trs) env)
            (LetStepTo (conf mrs' env') (conf trs env)
              (Build $ conf (ConsEField vr mrs') env'))

    , name "cons-efield-eval" $
      mkPairRule1 $ \env ->
      mkRule2 $ \r rs ->
        let (vr, vrs) = (vv r, vv rs) in
          StepTo (conf (ConsEField vr vrs) env)
            (Build $ conf (ReducedRecordCons vr vrs) env)

    , name "efield-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \k v v' ->
        let (mk, tvv, mv') = (mv k, tv v, mv v') in
          StepTo (conf (EField mk tvv) env)
            (LetStepTo (conf mv' env') (conf tvv env)
              (Build $ conf (EField mk mv') env'))

    , name "efield-eval" $
      mkPairRule1 $ \env ->
      mkRule2 $ \key val ->
        let (mkey, vval) = (mv key, vv val) in
          StepTo (conf (EField mkey vval) env)
            (Build $ conf (ReducedRecordPair mkey vval) env)

    , name "nil-efield-eval" $
      mkPairRule1 $ \env ->
      mkRule0 $
        StepTo (conf NilEField env)
          (Build $ conf ReducedRecordNil env)


      ---- ArrayExp

    , name "array-exp-cong-1" $
      mkPairRule2 $ \env env' ->
      mkRule4 $ \t n n' init ->
        let (mt, tn, mn', minit) = (mv t, tv n, mv n', mv init) in
          StepTo (conf (ArrayExp mt tn minit) env)
            (LetStepTo (conf mn' env') (conf tn env)
              (Build (conf (ArrayExp mt mn' minit) env')))

    , name "array-exp-cong-2" $
      mkPairRule2 $ \env env' ->
      mkRule4 $ \t n init init' ->
        let (mt, vn, tinit, minit') = (mv t, vv n, tv init, mv init') in
          StepTo (conf (ArrayExp mt vn tinit) env)
            (LetStepTo (conf minit' env') (conf tinit env)
              (Build (conf (ArrayExp mt vn minit') env')))

    , name "array-exp-eval" $
      mkPairRule1 $ \env ->
      mkRule4 $ \t n init val ->
        let (mt, vn, vinit, vval) = (mv t, vv n, vv init, vv val) in
          StepTo (conf (ArrayExp mt vn vinit) env)
            (LetComputation (emptyConf vval) (extComp MakeArray (matchRedState env) [vn, vinit])
              (Build (conf (HeapAlloc vval) env)))


      ---- LVal's


    , name "lsimplervar-eval" $
      mkRule5 $ \var mu h frame rest->
        let (mvar, mframe, mrest) = (mv var, mv frame, mv rest) in
          StepTo (Conf (LSimpleVar mvar) (ConsFrame mframe mrest, WholeSimpEnv h))
            (Build $ Conf (LFieldVar (ReferenceVal mframe) mvar) (ConsFrame mframe mrest, WholeSimpEnv h))

    , name "lfieldvar-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \var var' field ->
        let (tvar, mvar', mfield) = (tv var, mv var', mv field) in
          StepTo (conf (MkLFieldVar tvar mfield) env)
            (LetStepTo (conf mvar' env') (conf tvar env)
              (Build (conf (MkLFieldVar mvar' mfield) env')))

    , name "lfieldvar-done" $
      mkPairRule1 $ \env ->
      mkRule2 $ \val field ->
        let (vval, mfield) = (vv val, mv field) in
          StepTo (conf (MkLFieldVar vval mfield) env)
            (Build (conf (LFieldVar vval mfield) env))


    , name "lsubscript-cong-1" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \x x' s ->
        let (tx, mx', ms) = (tv x, mv x', mv s) in
          StepTo (conf (MkLSubscriptVar tx ms) env)
            (LetStepTo (conf mx' env') (conf tx env)
              (Build (conf (MkLSubscriptVar mx' ms) env')))

    , name "lsubscript-cong-2" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \arr e e' ->
        let (varr, te, me') = (vv arr, tv e, mv e') in
          StepTo (conf (MkLSubscriptVar varr te) env)
            (LetStepTo (conf me' env') (conf te env)
              (Build (conf (MkLSubscriptVar varr me') env')))

    , name "lsubscript-done" $
      mkPairRule1 $ \env ->
      mkRule2 $ \arr idx ->
        let (varr, vidx) = (vv arr, vv idx) in
          StepTo (conf (MkLSubscriptVar varr vidx) env)
            (Build (conf (LSubscriptVar varr vidx) env))

      ---- AssignExp

    , name "assn-rhs-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \l e e' ->
        let (ml, te, me') = (mv l, tv e, mv e') in
          StepTo (conf (AssignExp ml te) env)
            (LetStepTo (conf me' env') (conf te env)
              (Build (conf (AssignExp ml me') env')))

    , name "assn-lhs-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \l l' e ->
        let (tl, ml', ve) = (tv l, mv l', vv e) in
          StepTo (conf (AssignExp tl ve) env)
            (LetStepTo (conf ml' env') (conf tl env)
              (Build (conf (AssignExp ml' ve) env')))

    , name "field-assn-eval-field" $
      mkRule7 $ \val ref field re' addr mu h->
        let (vval, mref, mfield, vre', maddr, mmu) = (vv val, mv ref, mv field, vv re', mv addr, mv mu) in
          StepTo (Conf (AssignExp (LFieldVar (ReferenceVal mref) mfield) vval) (mmu, WholeSimpEnv h))
            (LetComputation (emptyConf (ReducedRecordPair maddr vre')) (extComp WriteField (mmu, WholeSimpEnv h) [mref, mfield, vval])
              (Build $ Conf NilExp (mmu, AssocOneVal h maddr vre')))

    , name "index-assn-eval" $
      mkRule7 $ \val ref index re' addr mu h ->
        let (vval, mref, vindex, vre', maddr, mmu) = (vv val, mv ref, vv index, vv re', mv addr, mv mu) in
          StepTo (Conf (AssignExp (LSubscriptVar (ReferenceVal mref) vindex) vval) (mmu, WholeSimpEnv h))
            (LetComputation (emptyConf (ReducedRecordPair maddr vre')) (extComp WriteIndex (mmu, WholeSimpEnv h) [mref, vindex, vval])
              (Build $ Conf NilExp (mmu, AssocOneVal h maddr vre')))

      ---- IfExp

    , name "if-cong" $
      mkPairRule2 $ \env env' ->
      mkRule4 $ \e1 s1 s2 e1' ->
        let (te1, ms1, ms2, me1') = (tv e1, mv s1, mv s2, mv e1') in
          StepTo (conf (IfExp te1 ms1 ms2) env)
            (LetStepTo (conf me1' env') (conf te1 env)
              (Build (conf (IfExp me1' ms1 ms2) env')))

    , name "if-0" $
      mkPairRule1 $ \env ->
      mkRule2 $ \s1 s2 ->
        let (ms1, ms2) = (mv s1, mv s2) in
          StepTo (conf (IfExp (IntExp (ConstInt 0)) ms1 ms2) env)
            (Build (conf ms2 env))

    , name "if-true" $
      mkPairRule1 $ \env ->
      mkRule3 $ \val s1 s2 ->
        let (vval, ms1, ms2) = (vv val, mv s1, mv s2) in
          StepTo (conf (IfExp vval ms1 ms2) env)
            (LetComputation (emptyConf NilExp) (extComp ValIsTrue (matchRedState env) [vval])
              (Build (conf ms1 env)))

      ---- WhileExp
    , name "while-start" $
      mkPairRule1 $ \env ->
      mkRule2 $ \e s ->
        let (me, ms) = (mv e, mv s) in
          StepTo (conf (WhileExp me ms) env)
            (Build (conf (IfExp me (LoopBody ms (WhileExp me ms)) NilExp) env))

    , name "loop-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \a a' b ->
        let (ta, ma', mb) = (tv a, mv a', mv b) in
          StepTo (conf (LoopBody ta mb) env)
            (LetStepTo (conf ma' env') (conf ta env)
              (Build (conf (LoopBody ma' mb) env')))

    , name "loop-break" $
      mkPairRule1 $ \env ->
      mkRule1 $ \b ->
        let mb = mv b in
          StepTo (conf (LoopBody BreakExp mb) env)
            (Build (conf NilExp env))

    , name "loop-done" $
      mkPairRule1 $ \env ->
      mkRule2 $ \a b ->
        let (va, mb) = (vv a, mv b) in
          StepTo (conf (LoopBody va mb) env)
            (Build (conf mb env))

      ---- ForExp
    , name "do-for" $
      mkPairRule1 $ \env ->
      mkRule4 $ \i low hi e ->
        let (mi, mlow, mhi, me) = (mv i, mv low, mv hi, mv e) in
          StepTo (conf (ForExp (VarDec mi) mlow mhi me) env)
            (Build $ conf (LetExp (ConsDecList (VarDecDec (VarDec mi) NoneSym mlow)
                                     (ConsDecList (VarDecDec (VarDec loopHiSym) NoneSym mhi)
                                        NilDecList))
                                  (WhileExp (OpExp (VarExp (SimpleVar mi)) LeOp (VarExp (SimpleVar loopHiSym)))
                                       (Seq me
                                         (AssignExp (LSimpleVar mi) (OpExp (VarExp $ SimpleVar mi) PlusOp (IntExp (ConstInt 1)))))))
                          env)

      ---- LetExp


    , name "let-start" $
      mkRule6 $ \ds e top stack addr h ->
        let (mds, me, mtop, mstack, maddr) = (mv ds, mv e, mv top, mv stack, mv addr) in
          StepTo (Conf (LetExp mds me) (ConsFrame mtop mstack, WholeSimpEnv h))
            (LetComputation (emptyConf (ReferenceVal maddr)) (extComp AllocAddress (ConsFrame mtop mstack, WholeSimpEnv h) [NilExp])
              (Build (Conf (Scope (DoLet mds me))
                           (ConsFrame maddr (ConsFrame mtop mstack), AssocOneVal h maddr (ReducedRecord $ Parent mtop)))))

    , name "scope-cong" $
      mkPairRule2 $ \env env' ->
      mkRule2 $ \e e' ->
        let (te, me') = (tv e, mv e') in
          StepTo (conf (Scope te) env)
            (LetStepTo (conf me' env') (conf te env)
              (Build (conf (Scope me') env')))

    , name "scope-done" $
      mkRule4 $ \val top stack h ->
        let (vval, mtop, mstack) = (vv val, mv top, mv stack) in
          StepTo (Conf (Scope vval) (ConsFrame mtop mstack, WholeSimpEnv h))
            (Build (Conf vval (mstack, WholeSimpEnv h)))

    , name "dolet-cong-decl" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \ds ds' e ->
        let (tds, mds', me) = (tv ds, mv ds', mv e) in
          StepTo (conf (DoLet tds me) env)
            (LetStepTo (conf mds' env') (conf tds env)
              (Build (conf (DoLet mds' me) env')))

    , name "dolet-finish-decls" $
      mkPairRule1 $ \env ->
      mkRule1 $ \e ->
        let me = mv e in
          StepTo (conf (DoLet NilDecList me) env)
            (Build (conf me env))

      --- Decs

    , name "declist-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \d d' ds ->
        let (td, md', mds) = (tv d, mv d', mv ds) in
          StepTo (conf (ConsDecList td mds) env)
            (LetStepTo (conf md' env') (conf td env)
              (Build (conf (ConsDecList md' mds) env')))

    , name "declist-next" $
      mkPairRule1 $ \env ->
      mkRule1 $ \ds ->
        let mds = mv ds in
          StepTo (conf (ConsDecList NilExp mds) env)
            (Build (conf mds env))

    , name "vardec-cong" $
      mkPairRule2 $ \env env' ->
      mkRule4 $ \nm ty e e' ->
        let (mnm, mty, te, me') = (mv nm, mv ty, tv e, mv e') in
          StepTo (conf (VarDecDec mnm mty te) env)
            (LetStepTo (conf me' env') (conf te env)
              (Build (conf (VarDecDec mnm mty me') env')))

    , name "vardec-initialize" $
      mkRule7 $ \addr rest re nm ty val h ->
        let (maddr, mrest, vre, mnm, mty, vval) = (mv addr, mv rest, vv re, mv nm, mv ty, vv val) in
          StepTo (Conf (VarDecDec (VarDec mnm) mty vval) (ConsFrame maddr mrest, AssocOneVal h maddr (ReducedRecord vre)))
            (Build (Conf NilExp (ConsFrame maddr mrest,
                                 AssocOneVal h maddr
                                               (ReducedRecord (ReducedRecordCons
                                                                 (ReducedRecordPair mnm vval)
                                                                 vre)))))

    , name "typedec-skip" $
      mkPairRule1 $ \env ->
      mkRule1 $ \td ->
        let mtd = mv td in
          StepTo (conf (TypeDecDec mtd) env)
            (Build (conf NilExp env))

    , name "functiondec-cong" $
      mkPairRule2 $ \env env' ->
      mkRule2 $ \fs fs' ->
        let (tfs, mfs') = (tv fs, mv fs') in
          StepTo (conf (FunctionDec tfs) env)
            (LetStepTo (conf mfs' env') (conf tfs env)
              (Build (conf (FunctionDec mfs') env')))

    , name "functiondec-done" $
      mkPairRule1 $ \env ->
      mkRule0 $
        StepTo (conf (FunctionDec NilExp) env)
          (Build (conf NilExp env))

    --- Function dec

    , name "fundeclist-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \d d' ds ->
        let (td, md', mds) = (tv d, mv d', mv ds) in
          StepTo (conf (ConsFunDec td mds) env)
            (LetStepTo (conf md' env') (conf td env)
              (Build (conf (ConsFunDec md' mds) env')))

    , name "fundeclist-next" $
      mkPairRule1 $ \env ->
      mkRule1 $ \d ->
        let md = mv d in
          StepTo (conf (ConsFunDec NilExp md) env)
            (Build (conf md env))

    , name "fundeclist-done" $
      mkPairRule1 $ \env ->
      mkRule0 $
        StepTo (conf NilFunDec env)
          (Build (conf NilExp env))

    -- NOTE: If there are multiple mutually-recursive function blocks in a single let-exp
    -- with overlapping names, not sure this works properly
    , name "fundec-initialize" $
      mkRule8 $ \nm args sym body addr stack h rec ->
        let (mnm, margs, msym, mbody, maddr, mstack, vrec) =
                                    (mv nm, mv args, mv sym, mv body, mv addr, mv stack, vv rec) in
          StepTo (Conf (FunDec mnm margs msym mbody) (ConsFrame maddr mstack,
                                                      AssocOneVal h maddr (ReducedRecord vrec)))
            (Build $ Conf NilExp
                          (ConsFrame maddr mstack,
                           AssocOneVal h maddr
                                         (ReducedRecord $
                                            ReducedRecordCons
                                              (ReducedRecordPair mnm (Closure margs mbody maddr))
                                              vrec)))

      ---- Builtins

    , name "builtin-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \b es es' ->
        let (mb, tes, mes') = (mv b, tv es, mv es') in
          StepTo (conf (Builtin mb tes) env)
            (LetStepTo (conf mes' env') (conf tes env)
              (Build (conf (Builtin mb mes') env')))

    , name "builtin-eval" $
      mkPairRule1 $ \env ->
      mkRule3 $ \func ret arg ->
        let (mfunc, vret, varg) = (mv func, vv ret, vv arg) in
          StepTo (conf (Builtin mfunc varg) env)
            (LetComputation (emptyConf vret) (extComp RunBuiltin (matchRedState env) [mfunc, varg])
              (Build (conf vret env)))
    ]

ibsToString :: InternedByteString -> String
ibsToString = BS.unpack . unintern

stringToIbs :: String -> InternedByteString
stringToIbs = intern . BS.pack

readField :: SimpEnv (Term Tiger) (Term Tiger) -> Term Tiger -> String -> Term Tiger
readField heap (ReducedRecordCons (ReducedRecordPair (Symbol k) v) rps) field = if ibsToString k == field then v else readField heap rps field
readField heap (Parent p) f = case Configuration.lookup p heap of
                                Just (ReducedRecord parent) -> readField heap parent f
                                Nothing -> error ("ERR: Dangling Pointer " ++ show f)
readField heap item field = error ("Error in read: \n" ++ show heap ++ "\n\t" ++ show item ++ "\n\t" ++ show field)


-- Returns (as a ReducedRecordPair, with some abuse of constructors) a pair of (addr of frame to update, new frame bindings)
writeField :: SimpEnv (Term Tiger) (Term Tiger) -> Term Tiger -> String -> Term Tiger -> Term Tiger
writeField heap addr field val = writeField' (Parent addr) (\t -> error "Unreachable")
  where
    writeField' :: Term Tiger -> (Term Tiger -> Term Tiger) -> Term Tiger
    writeField' (ReducedRecordCons (ReducedRecordPair (Symbol k) v) rps) kont =
      if ibsToString k == field then kont $ ReducedRecordCons (ReducedRecordPair (Symbol k) val) rps
                                else writeField' rps (\t -> kont $ ReducedRecordCons (ReducedRecordPair (Symbol k) v) t)
    writeField' ReducedRecordNil kont = error ("ERR: Dangling pointer " ++ show field)
    writeField' (Parent addr)    kont =
        case Configuration.lookup addr heap of
          Just (ReducedRecord parRec) -> writeField' parRec
                                                     (\t -> ReducedRecordPair addr (ReducedRecord t))

          Nothing -> error ("ERR: Dangling Pointer " ++ show field)


-- NOTE: I'm being very lazy/deadline-pressured in just turning integers to strings to use as keys;
--       would better to refactor to let other vals be keys
makeArray :: Integer -> Term Tiger -> Term Tiger
makeArray n v = ReducedRecord $ makeArray' n
  where
    makeArray' 0 = ReducedRecordNil
    makeArray' x = ReducedRecordCons (ReducedRecordPair (Symbol (stringToIbs $ show (x-1))) v) $ makeArray' (x-1)

returnInt :: (Integral a, Monad m) => a -> m (Configuration Tiger)
returnInt x = return $ emptyConf $ IntExp $ ConstInt $ toInteger x

returnBool :: Monad m => Bool -> m (Configuration Tiger)
returnBool True  = return $ emptyConf $ IntExp $ ConstInt 1
returnBool False = return $ emptyConf $ IntExp $ ConstInt 0

matchRedState :: (MetaVar, MetaVar) -> RedState Tiger
matchRedState (stack, heap) = (mv stack, WholeSimpEnv heap)

builtinPrint :: Term Tiger
builtinPrint = Closure
                    (ConsTField (TField (Symbol "x") (Symbol "int")) NilTField)
                    (Builtin Print (ConsExpList (VarExp (SimpleVar (Symbol "x"))) NilExpList))
                    (HeapAddr $ -1)

builtinFlush :: Term Tiger
builtinFlush = Closure
                    NilTField
                    (Builtin Flush NilExpList)
                    (HeapAddr $ -1)

builtinGetChar :: Term Tiger
builtinGetChar = Closure
                    NilTField
                    (Builtin GetChar NilExpList)
                    (HeapAddr $ -1)

builtinOrd :: Term Tiger
builtinOrd = Closure
                    (ConsTField (TField (Symbol "x") (Symbol "string")) NilTField)
                    (Builtin Ord (ConsExpList (VarExp (SimpleVar (Symbol "x"))) NilExpList))
                    (HeapAddr $ -1)

builtinChr :: Term Tiger
builtinChr = Closure
                    (ConsTField (TField (Symbol "x") (Symbol "int")) NilTField)
                    (Builtin Chr (ConsExpList (VarExp (SimpleVar (Symbol "x"))) NilExpList))
                    (HeapAddr $ -1)

builtinSize :: Term Tiger
builtinSize = Closure
                    (ConsTField (TField (Symbol "x") (Symbol "string")) NilTField)
                    (Builtin Size (ConsExpList (VarExp (SimpleVar (Symbol "x"))) NilExpList))
                    (HeapAddr $ -1)

builtinSubstring :: Term Tiger
builtinSubstring = Closure
                    (ConsTField (TField (Symbol "s") (Symbol "string"))
                         $ ConsTField (TField (Symbol "f") (Symbol "int"))
                         $ ConsTField (TField (Symbol "n") (Symbol "int"))
                           NilTField)
                    (Builtin Substring (ConsExpList (VarExp (SimpleVar (Symbol "s")))
                                          $ ConsExpList (VarExp (SimpleVar (Symbol "f")))
                                          $ ConsExpList (VarExp (SimpleVar (Symbol "n")))
                                            NilExpList))
                    (HeapAddr $ -1)

builtinConcat :: Term Tiger
builtinConcat = Closure
                    (ConsTField (TField (Symbol "s1") (Symbol "string"))
                         $ ConsTField (TField (Symbol "s2") (Symbol "string"))
                           NilTField)
                    (Builtin Concat (ConsExpList (VarExp (SimpleVar (Symbol "s1")))
                                      $ ConsExpList (VarExp (SimpleVar (Symbol "s2")))
                                        NilExpList))
                    (HeapAddr $ -1)

builtinNot :: Term Tiger
builtinNot = Closure
                    (ConsTField (TField (Symbol "x") (Symbol "int")) NilTField)
                    (Builtin Not (ConsExpList (VarExp (SimpleVar (Symbol "x"))) NilExpList))
                    (HeapAddr $ -1)

builtinExit :: Term Tiger
builtinExit = Closure
                    (ConsTField (TField (Symbol "x") (Symbol "int")) NilTField)
                    (Builtin Exit (ConsExpList (VarExp (SimpleVar (Symbol "x"))) NilExpList))
                    (HeapAddr $ -1)


runOrd :: String -> Int
runOrd ""     = -1
runOrd (c:cs) = ord c

runChr :: Integer -> MatchEffect (Configuration Tiger)
runChr n
  | n >= 0 && n < 128 = return $ emptyConf $ StringExp $ ConstStr $ stringToIbs [chr $ fromInteger n]
  | otherwise         = mzero

sublist :: [a] -> Integer -> Integer -> [a]
sublist l i n = take (fromInteger n) $ drop (fromInteger i) l

runExternalComputation :: CompFunc Tiger -> RedState Tiger -> [Term Tiger] -> MatchEffect (Configuration Tiger)

runExternalComputation OpIsntShortCircuit state [OrOp]  = mzero
runExternalComputation OpIsntShortCircuit state [AndOp] = mzero
runExternalComputation OpIsntShortCircuit state [_]     = return $ emptyConf NilExp


runExternalComputation ValIsTrue state [IntExp (ConstInt 0)] = mzero
runExternalComputation ValIsTrue state [_]                   = return $ emptyConf NilExp



runExternalComputation Compute state [PlusOp,     IntExp (ConstInt n1), IntExp (ConstInt n2)] = returnInt $ n1 + n2
runExternalComputation Compute state [MinusOp,    IntExp (ConstInt n1), IntExp (ConstInt n2)] = returnInt $ n1 - n2
runExternalComputation Compute state [TimesOp,    IntExp (ConstInt n1), IntExp (ConstInt n2)] = returnInt $ n1 * n2
runExternalComputation Compute state [DivideOp,   IntExp (ConstInt n1), IntExp (ConstInt n2)] = returnInt $ n1 `div` n2

runExternalComputation Compute state [EqOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 == n2
runExternalComputation Compute state [EqOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 == n2
runExternalComputation Compute state [EqOp, NilExp,                  NilExp                 ] = returnBool $ True

runExternalComputation Compute state [NeqOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 /= n2
runExternalComputation Compute state [NeqOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 /= n2
runExternalComputation Compute state [NeqOp, NilExp,                  NilExp                 ] = returnBool $ False

runExternalComputation Compute state [LtOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 < n2
runExternalComputation Compute state [LtOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 < n2

runExternalComputation Compute state [LeOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 <= n2
runExternalComputation Compute state [LeOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 <= n2

runExternalComputation Compute state [GtOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 > n2
runExternalComputation Compute state [GtOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 > n2

runExternalComputation Compute state [GeOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 >= n2
runExternalComputation Compute state [GeOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 >= n2

runExternalComputation AllocAddress (stack, heap) _ = return $ emptyConf (ReferenceVal $ HeapAddr $ size heap)

runExternalComputation ReadIndex  (stack, heap) [ReducedRecord r, IntExp (ConstInt i)] = return $ emptyConf $ readField  heap r (show i)
runExternalComputation WriteIndex (stack, heap) [addr, IntExp (ConstInt i), val]       = return $ emptyConf $ writeField heap addr (show i) val

runExternalComputation ReadField  (stack, heap) [ReducedRecord r, Symbol f] = return $ emptyConf $ readField  heap r (ibsToString f)
runExternalComputation WriteField (stack, heap) [addr, Symbol f, val]       = return $ emptyConf $ writeField heap addr (ibsToString f) val

runExternalComputation MakeArray (stack, heap) [IntExp (ConstInt n), val] = return $ emptyConf $ makeArray n val

runExternalComputation RunBuiltin (stack, heap) [Print, SingExp (StringExp (ConstStr s))] = matchEffectOutput (BS.pack $ ibsToString s) >> return (emptyConf NilExp)
runExternalComputation RunBuiltin (stack, heap) [Flush, ReducedNilExp] = matchEffectFlush >> return (emptyConf NilExp)
runExternalComputation RunBuiltin (stack, heap) [GetChar, ReducedNilExp] = emptyConf <$> StringExp <$> ConstStr <$> stringToIbs <$> (:[]) <$> (matchEffectOutput "Prompt: " >> matchEffectInputChar)
runExternalComputation RunBuiltin (stack, heap) [Ord, SingExp (StringExp (ConstStr s))] = returnInt $ runOrd $ ibsToString s
runExternalComputation RunBuiltin (stack, heap) [Chr, SingExp (IntExp (ConstInt n))] = runChr n
runExternalComputation RunBuiltin (stack, heap) [Size, SingExp (StringExp (ConstStr s))] = returnInt $ length $ ibsToString s
runExternalComputation RunBuiltin (stack, heap) [Substring, TripExp (StringExp (ConstStr s))
                                                                    (IntExp (ConstInt i))
                                                                    (IntExp (ConstInt n))] = return $ emptyConf $ StringExp (ConstStr $ stringToIbs $ sublist (ibsToString s) i n)
runExternalComputation RunBuiltin (stack, heap) [Concat, DoubExp (StringExp (ConstStr s1))
                                                                 (StringExp (ConstStr s2))] = return $ emptyConf $ StringExp $ ConstStr $ stringToIbs $ ibsToString s1 ++ibsToString s2
runExternalComputation RunBuiltin (stack, heap) [Not, SingExp (IntExp (ConstInt n))] = if n == 0 then returnInt 1 else returnInt 0
runExternalComputation RunBuiltin (stack, heap) [Exit, SingExp (IntExp (ConstInt n))] = return $ emptyConf $ DoExit (ConstInt n)

runExternalComputation AbsAllocAddress _ _ = return $ emptyConf ValStar

runExternalComputation func state [GStar  _] = return $ emptyConf ValStar
runExternalComputation func state [ValVar _] = return $ emptyConf ValStar

runExternalComputation func state [GStar _, _]  = return $ emptyConf ValStar
runExternalComputation func state [_, GStar _]  = return $ emptyConf ValStar
runExternalComputation func state [ValVar _, _] = return $ emptyConf ValStar
runExternalComputation func state [_, ValVar _] = return $ emptyConf ValStar

runExternalComputation func state [GStar _, _, _]  = return $ emptyConf ValStar
runExternalComputation func state [_, GStar _, _]  = return $ emptyConf ValStar
runExternalComputation func state [_, _, GStar _]  = return $ emptyConf ValStar
runExternalComputation func state [ValVar _, _, _] = return $ emptyConf ValStar
runExternalComputation func state [_, ValVar _, _] = return $ emptyConf ValStar
runExternalComputation func state [_, _, ValVar _] = return $ emptyConf ValStar

runExternalComputation fn _ args = error ("Unhandled case in runExternalComputation: " ++ show fn ++ " " ++ show args)
