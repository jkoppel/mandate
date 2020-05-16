{-# LANGUAGE PatternSynonyms #-}

module Languages.Imp.Analyze (
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
import Languages.Imp.CfgGen
import Languages.Imp.Imp

-------------------------------------------------------------------
------------------------ Const Prop, language specific ------------
-------------------------------------------------------------------

transferImpConst :: Map (GraphNode ImpLang) ConstPropState -> Term ImpLang -> ConstPropState -> ConstPropState
transferImpConst m (v := _)                   s = ConstPropState Bottom (Map.insert v (constProp_val s) (constProp_vars s))
transferImpConst m (VarExp (Var (VarName v))) s = ConstPropState (constProp_vars s ! v) (constProp_vars s)
transferImpConst m (EVal (Const n))           s = ConstPropState (Known n) (constProp_vars s)
transferImpConst m (Plus a b)                 s = ConstPropState (constValBinop (+) (abstractOutput m a) (abstractOutput m b))
                                                                 (constProp_vars s)
transferImpConst m _                          s = ConstPropState Bottom (constProp_vars s)

constPropFramework :: Set InternedByteString -> MonotoneFramework ConstPropState ImpLang
constPropFramework vars = MonotoneFramework { bottom = ConstPropState Bottom $ Map.fromSet (const Bottom) vars
                                            , join = joinConstPropState
                                            , transfer = transferImpConst
                                            }
