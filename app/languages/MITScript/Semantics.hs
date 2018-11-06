{-# LANGUAGE DeriveGeneric, EmptyDataDecls, FlexibleInstances, OverloadedStrings, PatternSynonyms, TypeFamilies #-}


module Languages.MITScript.Semantics (
    MITScript
  ) where

import Prelude hiding ( True, False, LT, GT, EQ )
import qualified Prelude

import GHC.Generics ( Generic )

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
        type RedState MITScript = (SimpEnv (Term MITScript) (Term MITScript), SimpEnv (Term MITScript) (Term MITScript))

        data CompFunc MITScript = Compute     | AbsCompute
                                | ReadField   | AbsReadField
                                | ReadIndex   | AbsReadIndex
                                | WriteField  | AbsWriteField
                                | WriteIndex  | AbsWriteIndex
                                | AllocAddress| AbsAllocAddress
            deriving ( Eq, Generic )

        compFuncName Compute = "compute"
        compFuncName ReadField = "readField"
        compFuncName ReadIndex = "readIndex"
        compFuncName WriteField = "writeField"
        compFuncName WriteIndex = "writeIndex"
        compFuncName AllocAddress = "allocAddress"

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

        runCompFunc ReadField [ReducedRecord r, Name f] = return $ initConf $ readField r (ibsToString f)
        runCompFunc ReadIndex [ReducedRecord r, i]      = return $ initConf $ readField r (toString i)

        runCompFunc WriteField [ReducedRecord r, Name f, val] = return $ initConf $ writeField r (ibsToString f) val
        runCompFunc WriteIndex [ReducedRecord r, i,      val] = return $ initConf $ writeField r (toString i)    val

        runCompFunc AllocAddress [] = initConf <$> NumConst <$> ConstInt <$> generateHeapAddress

        runCompFunc AbsCompute [GStar _, _]    = return $ initConf ValStar
        runCompFunc AbsCompute [_, GStar _]    = return $ initConf ValStar
        runCompFunc AbsCompute [GStar _, _, _] = return $ initConf ValStar
        runCompFunc AbsCompute [_, GStar _, _] = return $ initConf ValStar
        runCompFunc AbsCompute [_, _, GStar _] = return $ initConf ValStar

        runCompFunc AbsReadField [GStar _, _] = return $ initConf ValStar
        runCompFunc AbsReadField [_, GStar _] = return $ initConf ValStar

        runCompFunc AbsReadIndex [GStar _, _] = return $ initConf ValStar
        runCompFunc AbsReadIndex [_, GStar _] = return $ initConf ValStar

        runCompFunc AbsAllocAddress [] = return $ initConf ValStar

instance Hashable (CompFunc MITScript)

instance ValueIrrelevance (CompFunc MITScript) where
    valueIrrelevance Compute     = AbsCompute
    valueIrrelevance ReadIndex = AbsReadIndex
    valueIrrelevance ReadField = AbsReadField
    valueIrrelevance AllocAddress = AbsAllocAddress

    valueIrrelevance AbsCompute = AbsCompute
    valueIrrelevance AbsReadField = AbsReadField
    valueIrrelevance AbsReadIndex = AbsReadIndex
    valueIrrelevance AbsAllocAddress = AbsAllocAddress

instance Lang MITScript where
    signature = mitScriptSig
    rules = mitScriptRules

    initConf t = Conf t (EmptySimpEnv, EmptySimpEnv)

conf :: Term MITScript -> (MetaVar, MetaVar) -> Configuration MITScript
conf t (stack, heap) = Conf t (WholeSimpEnv stack, WholeSimpEnv heap)

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
    mkRule0 $
        StepTo (conf (Block NilStmt) env) (Build (conf NilStmt env))

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
    mkRule4 $ \var val mu h ->
        let (mvar, vval) = (mv var, vv val) in
            StepTo (Conf (Var mvar) (AssocOneVal mu mvar vval, WholeSimpEnv h))
            (Build $ Conf vval (AssocOneVal mu mvar vval, WholeSimpEnv h))

    -- Write
    , name "generic-assn-cong" $
    mkPairRule2 $ \env env' ->
    mkRule3 $ \var e e' ->
        let (mvar, te, me') = (mv var, tv e, mv e') in
            StepTo (conf (Assign mvar te) env)
            (LetStepTo (conf me' env') (conf te env)
            (Build $ conf (Assign mvar me') env'))

    , name "stack-assn-eval" $
    mkRule4 $ \var val mu h ->
        let (mvar, vval) = (mv var, vv val) in
            StepTo (conf (Assign (Var mvar) vval) (mu, h))
            (Build $ Conf NilStmt (AssocOneVal mu mvar vval, WholeSimpEnv h))

    , name "field-assn-cong" $
    mkPairRule2 $ \env env' ->
    mkRule4 $ \field val re re'  ->
        let (mfield, vval, tre, mre) = (mv field, vv val, tv re, mv re') in
            StepTo (conf (Assign (FieldAccess tre mfield) vval) env)
            (LetStepTo (conf mre env') (conf tre env)
            (Build $ conf (Assign (FieldAccess mre mfield) vval) env'))

    , name "field-assn-eval" $
    mkRule7 $ \val field ref mu h re re'->
        let (vval, vref, mfield, vre, vre') = (vv val, vv ref, mv field, vv re, vv re') in
            StepTo (Conf (Assign (FieldAccess (ReferenceVal vref) mfield) vval) (WholeSimpEnv mu, AssocOneVal h vref vre))
            (LetComputation (initConf vre') (ExtComp WriteField [vre, mfield, vval])
            (Build $ Conf NilStmt (WholeSimpEnv mu, AssocOneVal h vref vre')))

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
        let (vval, vref, mindex, vre, vre') = (vv val, vv ref, mv index, vv re, vv re') in
            StepTo (Conf (Assign (Index (ReferenceVal vref) mindex) vval) (WholeSimpEnv mu, AssocOneVal h vref vre))
            (LetComputation (initConf vre') (ExtComp WriteIndex [vre, mindex, vval])
            (Build $ Conf NilStmt (WholeSimpEnv mu, AssocOneVal h vref vre')))

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
            (LetComputation (initConf $ ValVar v') (ExtComp Compute [mop, vv1])
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
            (LetComputation (initConf $ ValVar v') (ExtComp Compute [mop, vv1, vv2])
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
    mkRule4 $ \h mu val addr ->
        let (vval, vaddr) = (vv val, vv addr) in
            StepTo (conf (HeapAlloc vval) (mu, h))
            (LetComputation (initConf vaddr) (ExtComp AllocAddress [])
            (Build $ Conf (ReferenceVal vaddr) (WholeSimpEnv mu, AssocOneVal h vaddr vval)))

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
        let (vr, vi, v', vref) = (vv r, vv i, vv v, vv ref) in
            StepTo (Conf (Index (ReferenceVal vr) vi) (WholeSimpEnv mu, AssocOneVal h vr vref))
            (LetComputation (initConf v') (ExtComp ReadIndex [vref, vi])
            (Build $ Conf v' (WholeSimpEnv mu, AssocOneVal h vr vref)))

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
        let (vr, mf, vref, v') = (vv r, mv f, vv v, vv ref) in
            StepTo (Conf (FieldAccess (ReferenceVal vr) mf) (WholeSimpEnv mu, AssocOneVal h vr vref))
            (LetComputation (initConf v') (ExtComp ReadField [vref, mf])
            (Build $ Conf v' (WholeSimpEnv mu, AssocOneVal h vr vref)))
    ]

ibsToString :: InternedByteString -> String
ibsToString = BS.unpack . unintern

stringToIbs :: String -> InternedByteString
stringToIbs = intern . BS.pack

readField :: Term MITScript -> String ->  Term MITScript
readField (ReducedRecordCons (ReducedRecordPair (Name k) v) rps) field = if ibsToString k == field then v else readField rps field
readField _ _ = None

writeField' :: Term MITScript -> String -> Term MITScript -> Term MITScript
writeField' (ReducedRecordCons (ReducedRecordPair (Name k) v) rps) field val =
    if ibsToString k == field then ReducedRecordCons (ReducedRecordPair (Name k) val) rps
                              else ReducedRecordCons (ReducedRecordPair (Name k) v) (writeField' rps field val)
writeField' ReducedRecordNil field val = ReducedRecordCons (ReducedRecordPair (Name (stringToIbs field)) val) ReducedRecordNil

writeField :: Term MITScript -> String -> Term MITScript -> Term MITScript
writeField x y z = ReducedRecord (writeField' x y z)


toMetaBool :: Term MITScript -> Bool
toMetaBool True = Prelude.True
toMetaBool False = Prelude.False

fromMetaBool :: Bool -> Term MITScript
fromMetaBool Prelude.True = True
fromMetaBool Prelude.False = False

removeQuotes :: String -> String
removeQuotes s = take (length s - 2) $ drop 1 s

toString :: Term MITScript -> String
toString (BConst b) = show $ toMetaBool b
toString (NumConst (ConstInt n1)) = show n1
toString (Str (ConstStr s)) = ibsToString s
toString None = "None"
toString (ReducedRecord rs) = "{" ++ toString rs ++ "}"
toString (ReducedRecordCons rp rps) = toString rp ++ toString rps
toString ReducedRecordNil = ""
toString (ReducedRecordPair (Name n) v) = ibsToString n ++ ":" ++ toString v ++ " "

returnInt :: Monad m => Integer -> m (GConfiguration (RedState MITScript) MITScript)
returnInt x = return $ initConf $ NumConst $ ConstInt x

returnBool :: Monad m => Bool -> m (GConfiguration (RedState MITScript) MITScript)
returnBool x = return $ initConf $ BConst $ fromMetaBool x

returnString :: Monad m => String -> m (GConfiguration (RedState MITScript) MITScript)
returnString x = return $ initConf $ Str $ ConstStr $ fromString x

heapAddressGenerator :: IORef Integer
heapAddressGenerator = unsafePerformIO (newIORef 0)
{-# NOINLINE heapAddressGenerator #-}

generateHeapAddress :: MatchEffect Integer
generateHeapAddress = MatchEffect $ atomicModifyIORef heapAddressGenerator (\x -> (x+1, x))