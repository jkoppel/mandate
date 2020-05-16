{-# LANGUAGE PatternSynonyms #-}

module Languages.Tiger.Analyze (
    constPropFramework
  ) where

import           Data.Map ( Map, (!) )
import qualified Data.Map.Strict as Map
import           Data.Set ( Set )

import Data.Interned.ByteString ( InternedByteString )


import CfgGenRuntime
import Term hiding ( Symbol )

import Languages.Analysis.ConstProp
import Languages.Analysis.Monotone
import Languages.Tiger.Signature

-------------------------------------------------------------------
------------------------ Const Prop, language specific ------------
-------------------------------------------------------------------

-----
-- Ideally, this would be dependent on a symbol table / escape analysis,
-- and so could soundly overapproximate the effect of function calls
-- on the abstract state. But I don't have either, and need
-- neither to fulfill the goal of showing that we can do something
-- with Mandate's CFGs.
--

-- Oh yeah: and we totally don't handle shadowing. (Would also
-- be handle by a symbol table that gives each var ref a unique ID.)

interpBinop :: Term Tiger -> (ConstVal -> ConstVal -> ConstVal)
interpBinop PlusOp   = constValBinop (+)
interpBinop MinusOp  = constValBinop (-)
interpBinop TimesOp  = constValBinop (*)
interpBinop DivideOp = constValBinop div
interpBinop _     = \_ _ -> Bottom


-- Not handled: ForExp. (Variables bound in ForExp should always be Top; instead, they are Botttom. But
--                       they'll become Top anyway)


-- Exists because tiger VarExp's are not congruent; therefore, less straightforward to look up var
-- for a node
abstractOutput' :: Map (GraphNode Tiger) ConstPropState -> Term Tiger -> ConstVal
abstractOutput' m (VarExp s) = abstractOutput m s
abstractOutput' m a          = abstractOutput m a


transferMITScriptConst :: Map (GraphNode Tiger) ConstPropState -> Term Tiger -> ConstPropState -> ConstPropState
transferMITScriptConst m (AssignExp (LSimpleVar (Symbol v)) a) s = ConstPropState Bottom (Map.insert v (abstractOutput m a) (constProp_vars s))
transferMITScriptConst m (VarDecDec (VarDec (Symbol v)) _ a)   s = ConstPropState Bottom (Map.insert v (abstractOutput m a) (constProp_vars s))
transferMITScriptConst m (SimpleVar (Symbol v))                s = ConstPropState (constProp_vars s ! v) (constProp_vars s)
transferMITScriptConst m (IntExp (ConstInt n))                 s = ConstPropState (Known n) (constProp_vars s)
transferMITScriptConst m (OpExp a op b)                        s = ConstPropState (interpBinop op
                                                                                    (abstractOutput' m a)
                                                                                    (abstractOutput' m b))
                                                                       (constProp_vars s)
transferMITScriptConst m _                                     s = ConstPropState Bottom (constProp_vars s)


constPropFramework :: Set InternedByteString -> MonotoneFramework ConstPropState Tiger
constPropFramework vars = MonotoneFramework { bottom = ConstPropState Bottom $ Map.fromSet (const Bottom) vars
                                            , join = joinConstPropState
                                            , transfer = transferMITScriptConst
                                            }
