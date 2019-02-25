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

instance Hashable (CompFunc Tiger)

instance Irrelevance (CompFunc Tiger) where
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