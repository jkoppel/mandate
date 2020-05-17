{-# LANGUAGE PatternSynonyms #-}

module Languages.MITScript.Analyze (
    constPropFramework
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

-----
-- Ideally, this would be dependent on a symbol table / escape analysis,
-- and so could soundly overapproximate the effect of function calls
-- on the abstract state. But I don't have either, and need
-- neither to fulfill the goal of showing that we can do something
-- with Mandate's CFGs.


interpBinop :: Term MITScript -> (ConstVal -> ConstVal -> ConstVal)
interpBinop PLUS  = constValBinop (+)
interpBinop MINUS = constValBinop (-)
interpBinop TIMES = constValBinop (*)
interpBinop DIV   = constValBinop div
interpBinop _     = \_ _ -> Bottom


interpUnop :: Term MITScript -> (ConstVal -> ConstVal)
interpUnop UMINUS = constValUnop (0 -)
interpUnop _      = \_ -> Bottom

transferMITScriptConst :: Map (GraphNode MITScript) ConstPropState -> Term MITScript -> ConstPropState -> ConstPropState
transferMITScriptConst m (Assign (LVar (Name v)) a) s = ConstPropState Bottom (Map.insert v (abstractOutput m a) (constProp_vars s))
transferMITScriptConst m (Var (Name v))             s = ConstPropState (constProp_vars s ! v) (constProp_vars s)
transferMITScriptConst m (NumConst (ConstInt n))    s = ConstPropState (Known n) (constProp_vars s)
transferMITScriptConst m (BinExp a op b)            s = ConstPropState (interpBinop op
                                                                                    (abstractOutput m a)
                                                                                    (abstractOutput m b))
                                                                       (constProp_vars s)
transferMITScriptConst m (UnExp op a)               s = ConstPropState (interpUnop op (abstractOutput m a))
                                                                       (constProp_vars s)
transferMITScriptConst m _                          s = ConstPropState Bottom (constProp_vars s)


constPropFramework :: Set InternedByteString -> MonotoneFramework ConstPropState MITScript
constPropFramework vars = MonotoneFramework { bottom   = constPropBottom vars
                                            , sourceSt = constPropBottom vars
                                            , join     = joinConstPropState
                                            , transfer = transferMITScriptConst
                                            }
