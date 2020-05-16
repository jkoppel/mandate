{-# LANGUAGE PatternSynonyms #-}

module Languages.MITScript.Analyze (
    --constPropFramework
  ) where

import           Data.Map ( Map, (!) )
import qualified Data.Map.Strict as Map
import           Data.Set ( Set )

import Data.Interned.ByteString ( InternedByteString )


import CfgGenRuntime
import Term

import Languages.Analysis.ConstProp
import Languages.Analysis.Monotone
import Languages.MITScript.Signature

-------------------------------------------------------------------
------------------------ Const Prop, language specific ------------
-------------------------------------------------------------------

{-
Assign
BinExp
UnExp
NumConst
Var
-}

{-

transferMITScriptConst :: Map (GraphNode MITScript) ConstPropState -> Term MITScript -> ConstPropState -> ConstPropState
transferMITScriptConst m (v := _)                   s = ConstPropState Bottom (Map.insert v (constProp_val s) (constProp_vars s))
transferMITScriptConst m (VarExp (Var (VarName v))) s = ConstPropState (constProp_vars s ! v) (constProp_vars s)
transferMITScriptConst m (EVal (Const n))           s = ConstPropState (Known n) (constProp_vars s)
transferMITScriptConst m (Plus a b)                 s = ConstPropState (constValBinop (+) (abstractOutput m a) (abstractOutput m b))
                                                                 (constProp_vars s)
transferMITScriptConst m _                          s = ConstPropState Bottom (constProp_vars s)

constPropFramework :: Set InternedByteString -> MonotoneFramework ConstPropState MITScript
constPropFramework vars = MonotoneFramework { bottom = ConstPropState Bottom $ Map.fromSet (const Bottom) vars
                                            , join = joinConstPropState
                                            , transfer = transferMITScriptConst
                                            }
-}