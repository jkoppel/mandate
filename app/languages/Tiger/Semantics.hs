{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.Tiger.Semantics (
    Tiger
  ) where


import qualified Data.Map as Map
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Interned ( unintern, intern )
import Data.Interned.ByteString ( InternedByteString(..) )
import Data.Hashable ( Hashable )
import Data.String ( fromString )

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

import Languages.Tiger.Signature

instance LangBase Tiger where

        type RedState Tiger = (Term Tiger, SimpEnv (Term Tiger) (Term Tiger))

        data CompFunc Tiger = ReadField    | AbsReadField
                            | WriteField   | AbsWriteField
                            | AllocAddress | AbsAllocAddress
                            | Compute      | AbsCompute
                            | WriteIndex   | AbsWriteIndex
                            | ReadIndex    | AbsReadIndex
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
        compFuncName RunBuiltin   = "runBuiltin"
        compFuncName ValIsTrue    = "valIsTrue"

        compFuncName AbsReadField    = "absreadField"
        compFuncName AbsWriteField   = "abswriteField"
        compFuncName AbsAllocAddress = "absallocAddress"
        compFuncName AbsCompute      = "abscompute"
        compFuncName AbsWriteIndex   = "abswriteIndex"
        compFuncName AbsReadIndex    = "absreadIndex"
        compFuncName AbsRunBuiltin   = "absrunBuiltin"
        compFuncName AbsValIsTrue    = "absvalIsTrue"

        compFuncName OpIsntShortCircuit = "opIsntShortCircuit"

        runCompFunc func (c:cs)  = runExternalComputation func (confState c) (map confTerm (c:cs))

instance Hashable (CompFunc Tiger)

instance Irrelevance (CompFunc Tiger) where
    irrelevance _ ReadField    = AbsReadField
    irrelevance _ WriteField   = AbsWriteField
    irrelevance _ AllocAddress = AbsAllocAddress
    irrelevance _ Compute      = AbsCompute
    irrelevance _ ReadIndex    = AbsReadIndex
    irrelevance _ WriteIndex   = AbsWriteIndex
    irrelevance _ RunBuiltin   = AbsRunBuiltin
    irrelevance _ ValIsTrue    = AbsValIsTrue

    irrelevance _ AbsReadField    = AbsReadField
    irrelevance _ AbsAllocAddress = AbsAllocAddress
    irrelevance _ AbsCompute      = AbsCompute
    irrelevance _ AbsReadIndex    = AbsReadIndex
    irrelevance _ AbsWriteIndex   = AbsWriteIndex
    irrelevance _ AbsRunBuiltin   = AbsRunBuiltin
    irrelevance _ AbsValIsTrue    = AbsValIsTrue

    irrelevance _ OpIsntShortCircuit = OpIsntShortCircuit

instance Lang Tiger where
    signature = tigerSig

    initConf t = Conf t (
        ConsFrame (HeapAddr 0) NilFrame,
        JustSimpMap $ SimpEnvMap $ Map.fromList
            [
                (HeapAddr 0, ReducedRecord
                                $ ReducedRecordCons (ReducedRecordPair (Name "print")     (ReferenceVal $ HeapAddr 1))
                                $ ReducedRecordCons (ReducedRecordPair (Name "flush")     (ReferenceVal $ HeapAddr 2))
                                $ ReducedRecordCons (ReducedRecordPair (Name "getchar")   (ReferenceVal $ HeapAddr 3))
                                $ ReducedRecordCons (ReducedRecordPair (Name "ord")       (ReferenceVal $ HeapAddr 4))
                                $ ReducedRecordCons (ReducedRecordPair (Name "chr")       (ReferenceVal $ HeapAddr 5))
                                $ ReducedRecordCons (ReducedRecordPair (Name "size")      (ReferenceVal $ HeapAddr 6))
                                $ ReducedRecordCons (ReducedRecordPair (Name "substring") (ReferenceVal $ HeapAddr 7))
                                $ ReducedRecordCons (ReducedRecordPair (Name "concat")    (ReferenceVal $ HeapAddr 8))
                                $ ReducedRecordCons (ReducedRecordPair (Name "not")       (ReferenceVal $ HeapAddr 9))
                                $ ReducedRecordCons (ReducedRecordPair (Name "exit")      (ReferenceVal $ HeapAddr 10))
                                $ Parent $ HeapAddr $ -1)
              , (HeapAddr 1,  builtinPrint)
              , (HeapAddr 2,  builtinFlush)
              , (HeapAddr 3,  builtinGetChar)
              , (HeapAddr 4,  builtinOrd)
              , (HeapAddr 5,  builtinChr)
              , (HeapAddr 6,  builtinSize)
              , (HeapAddr 7,  builtinSubstring)
              , (HeapAddr 8,  builtinConcat)
              , (HeapAddr 9,  builtinNot)
              , (HeapAddr 10, builtinExit)
            ])


instance HasSOS Tiger where
    rules = tigerRules

conf :: Term Tiger -> (MetaVar, MetaVar) -> Configuration Tiger
conf t (returnStack, heap) = Conf t (mv returnStack, WholeSimpEnv heap)

vv :: MetaVar -> Term Tiger
vv = ValVar

tv :: MetaVar -> Term Tiger
tv = NonvalVar

mv :: MetaVar -> Term Tiger
mv = MetaVar


tigerRules :: IO (NamedRules Tiger)
tigerRules = sequence [

      --- PExp, PDecs

      ---- Vars

      name "var-lookup" $
      mkRule4 $ \var frame rest h ->
        let (mvar, mframe, mrest) = (mv var, mv frame, mv rest) in
          StepTo (Conf (SimpleVar mvar) (ConsFrame mframe mrest, WholeSimpEnv h))
            (Build $ Conf (FieldVar (ReferenceVal mframe) mvar) (ConsFrame mframe mrest, WholeSimpEnv h))

    , name "field-cong" $
      mkPairRule2 $ \env env' ->
      mkRule4 $ \x x' s ->
        let (tx, mx', ms) = (tv x, mv x', mv s) in
          StepTo (conf (FieldVar tx s) env)
            (LetStepTo (conf mx' env') (conf tx env)
              (Build (conf (FieldVar mx' s))))

    , name "field-read" $
      mkRule6 $ \ref sym stack h fr val ->
        let (mref, msym, mstack, vfr, vval) = (mv ref, mv sym, mv stack, vv fr, vv val) in
          StepTo (Conf (FieldVar (ReferenceVal mref) msym) (mstack, AssocOneVal h mref vfr))
            (LetComputation (initConf vval) (extComp ReadField (mstack, AssocOneVal h mref vfr) [vfr, msym])
              (Build $ Conf vval (mstack, AssocOneVal h mref vfr)))

    , name "subscript-cong" $
      mkPairRule2 $ \env env' ->
      mkRule4 $ \x x' s ->
        let (tx, mx', ms) = (tv x, mv x', mv s) in
          StepTo (conf (SubscriptVar tx s) env)
            (LetStepTo (conf mx' env') (conf tx env)
              (Build (conf (SubscriptVar mx' s))))


    , name "subscript-read" $
      mkRule6 $ \ref i stack h fr val ->
        let (mref, vi, mstack, vfr, vval) = (mv ref, vv i, mv stack, vv fr, vv val) in
          StepTo (Conf (SubscriptVar (ReferenceVal mref) i) (mstack, AssocOneVal h mref vfr))
            (LetComputation (initConf vval) (extComp ReadIndex (mstack, AssocOneVal h mref vfr) [vfr, vi])
              (Build $ Conf vval (mstack, AssocOneVal h mref vfr)))

      ---- Seq

    , name "seq-cong" $
      mkPairRule2 $ \env env' ->
      mkRule3 $ \s1 s2 s1' ->
          let (ts1, ms2, ms1') = (tv s1, mv s2, mv s1') in
              StepTo (conf (SeqExp ts1 ms2) env)
                (LetStepTo (conf ms1' env')(conf ts1 env)
                  (Build (conf (SeqExp ms1' ms2) env')))

    , name "seq-nil" $
      mkPairRule1 $ \env ->
      mkRule1 $ \s ->
        let ms = mv s in
            StepTo (conf (SeqExp NilExp ms) env)
                (Build (conf ms env))


    , name "seq-break" $
      mkPairRule1 $ \env ->
      mkRule1 $ \s ->
        let ms = mv s in
            StepTo (conf (SeqExp BreakExp ms) env)
                (Build (conf BreakExp env))


    , name "seq-exit" $
      mkPairRule1 $ \env ->
      mkRule2 $ \s v ->
        let (ms, mv) = (mv s, mv v) in
            StepTo (conf (SeqExp (DoExit mv) ms) env)
                (Build (conf (DoExit mv) env))


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
        let vval = vv val
          StepTo (conf (VarExp vval) env)
            (Build (conf vval env))

      ---- App

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
            (LetComputation (initConf NilExp) (extComp OpIsntShortCircuit (matchRedState env) [mop])
              (LetStepTo (conf me2' env') (conf te2 env)
                (Build (conf (OpExp ve1 mop me2') env'))))

    , name "op-eval" $
      mkPairRule1 $ \env ->
      mkRule4 $ \v1 v2 v' op ->
        let (vv1, vv2, vv', mop) = (vv v1, vv v2, vv v', mv op) in
          StepTo (conf (OpExp vv1 mop vv2))
            (LetComputation (initConf vv') (extComp Compute (matchRedState env) [mop, vv1, vv2])
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
          (LetComputation (initConf NilExp) (extComp ValIsTrue (matchRedState env) [vv1])
            (Build (conf me2 env)))

    --- FIXME: I'm not sure if it should return v1 or (ConstInt 1). For CFG purposes, doesn't matter
    , name "or-true" $
      mkPairRule1 $ \env ->
      mkRule2 $ \v1 e2 ->
        let (vv1, me2) = (vv v1, mv e2) in
        StepTo (conf (OpExp vv1 OrOp me2) env)
          (LetComputation (initConf NilExp) (extComp ValIsTrue (matchRedState env) [vv1])
            (Build (conf vv1 env)))

      ---- Record Exp
      ---- AssignExp

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
        mkRule3 $ \v s1 s2 ->
          let (vv, ms1, ms2) = (vv v, mv s1, mv s2) in
            StepTo (conf (IfExp vv ms1 ms2) env)
              (LetComputation (initConf NilExp) (extComp ValIsTrue (matchRedState env) [vv])
                (Build (conf ms1 env)))

      ---- WhileExp
      ---- ForExp
      ---- LetExp
      ---- ArrayExp

      --- Stop at FunctionDec
    ]



readField :: SimpEnv (Term Tiger) (Term Tiger) -> Term Tiger -> String -> Term Tiger
readField' heap (ReducedRecordCons (ReducedRecordPair (Symbol k) v) rps) field = if ibsToString k == field then v else readField' heap rps field
readField' heap (Parent p) f = case Configuration.lookup p heap of
                                 Just (ReducedRecord parent) -> readField heap parent f
                                 Nothing -> error "ERR: Dangling Pointer"
readField' heap item field = error (show heap ++ "\n\t" ++ show item ++ "\n\t" ++ show field)

returnInt :: Monad m => Integer -> m (Configuration Tiger)
returnInt x = return $ initConf $ IntExp $ ConstInt x

returnBool :: Monad m => Bool -> m (Configuration Tiger)
returnBool True  = return $ initConf $ IntExp $ ConstInt 1
returnBool False = return $ initConf $ IntExp $ ConstInt 0


runExternalComputation :: CompFunc Tiger -> RedState Tiger -> [Term Tiger] -> MatchEffect (Configuration Tiger)

runExternalComputation Compute state [PlusOp,     IntExp (ConstInt n1), IntExp (ConstInt n2)] = returnInt $ n1 + n2
runExternalComputation Compute state [MinusOp,    IntExp (ConstInt n1), IntExp (ConstInt n2)] = returnInt $ n1 - n2
runExternalComputation Compute state [TimesOp,    IntExp (ConstInt n1), IntExp (ConstInt n2)] = returnInt $ n1 * n2
runExternalComputation Compute state [DivideOp,   IntExp (ConstInt n1), IntExp (ConstInt n2)] = returnInt $ n1 `div` n2

runExternalComputation Compute state [EqOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 == n2
runExternalComputation Compute state [EqOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 == n2

runExternalComputation Compute state [NeqOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 /= n2
runExternalComputation Compute state [NeqOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 /= n2

runExternalComputation Compute state [LtOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 < n2
runExternalComputation Compute state [LtOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 < n2

runExternalComputation Compute state [LeOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 <= n2
runExternalComputation Compute state [LeOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 <= n2

runExternalComputation Compute state [GtOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 > n2
runExternalComputation Compute state [GtOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 > n2

runExternalComputation Compute state [GeOp, IntExp    (ConstInt n1), IntExp    (ConstInt n2)] = returnBool $ n1 >= n2
runExternalComputation Compute state [GeOp, StringExp (ConstStr n1), StringExp (ConstStr n2)] = returnBool $ n1 >= n2


runExternalComputation Compute state [GT,  NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnBool $ n1 > n2
runExternalComputation Compute state [GTE, NumConst (ConstInt n1), NumConst (ConstInt n2)] = returnBool $ n1 >= n2


runExternalComputation Compute state [AND, BConst True, BConst True] = returnBool Prelude.True
runExternalComputation Compute state [AND, BConst l, BConst r]       = returnBool Prelude.False

runExternalComputation Compute state [OR, BConst False, BConst False] = returnBool Prelude.False
runExternalComputation Compute state [OR, BConst l, BConst r]         = returnBool Prelude.True

runExternalComputation ReadIndex (stack, heap)  [ReducedRecord r, i]      = return $ initConf $ readField heap r (toString i heap)

runExternalComputation ReadField (stack, heap)  [ReducedRecord r, Symbol s] = return $ initConf $ readField heap r (ibsToString s)

