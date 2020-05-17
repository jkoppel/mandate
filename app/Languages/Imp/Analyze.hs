{-# LANGUAGE PatternSynonyms #-}

module Languages.Imp.Analyze (
    constPropFramework

  , analyzeParenBalance
  ) where

import           Data.Map ( Map, (!) )
import qualified Data.Map.Strict as Map
import           Data.Set ( Set )

import Data.Interned.ByteString ( InternedByteString )


import CfgGenRuntime
import Graph
import Semantics.Abstraction
import Term

import Languages.Analysis.ConstProp
import Languages.Analysis.Monotone
import Languages.Imp.CfgGen
import Languages.Imp.Imp
import Languages.Translation

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
constPropFramework vars = MonotoneFramework { bottom   = constPropBottom vars
                                            , sourceSt = constPropBottom vars
                                            , join     = joinConstPropState
                                            , transfer = transferImpConst
                                            }

-------------------------------------------------------------------
------------------------ Paren balancing --------------------------
-------------------------------------------------------------------


data ParenBalanceState = ParenExcess Int |  ParenBottom | ParenTop
  deriving (Eq, Ord, Show)


joinParenBalance :: ParenBalanceState -> ParenBalanceState -> ParenBalanceState
joinParenBalance ParenBottom b           = b
joinParenBalance a           ParenBottom = a
joinParenBalance a b = if a == b then a else ParenTop

parenBalanceAdd :: Int -> ParenBalanceState -> ParenBalanceState
parenBalanceAdd n ParenBottom     = ParenBottom
parenBalanceAdd n ParenTop        = ParenTop
parenBalanceAdd n (ParenExcess m) = if n + m >= 0 then
                                      ParenExcess (n + m)
                                    else
                                      ParenTop


addParens' :: String -> ParenBalanceState -> ParenBalanceState
addParens' s ParenTop = ParenTop
addParens' []      t = t
addParens' ('(':s) t = addParens' s (parenBalanceAdd   1  t)
addParens' (')':s) t = addParens' s (parenBalanceAdd (-1) t)
addParens'   (_:s) t = addParens' s t


addParens :: InternedByteString -> ParenBalanceState -> ParenBalanceState
addParens ibs = addParens' (ibsToString ibs)


transferParen :: Map (GraphNode ImpLang) ParenBalanceState -> Term ImpLang -> ParenBalanceState -> ParenBalanceState
transferParen _ (Write (EVal (StrConst str))) s = addParens str s
transferParen _ (Write _)                     s = ParenTop
transferParen _ _                             s = s

parenBalanceFramework :: Term ImpLang -> MonotoneFramework ParenBalanceState ImpLang
parenBalanceFramework startTerm = MonotoneFramework { bottom   = ParenBottom
                                                    , sourceSt = ParenExcess 0
                                                    , join     = joinParenBalance
                                                    , transfer = transferParen
                                                    }

analyzeParenBalance :: InternedByteString -> Term ImpLang -> IO (Map (GraphNode ImpLang) ParenBalanceState)
analyzeParenBalance trackingVar t = do
  let absStartTerm = irrelevance (VarNotIrr trackingVar) t
  let fram = parenBalanceFramework absStartTerm
  g <- makePathSensitiveCfg t trackingVar
  let sourceNode = nodeForTerm (nodeList g) absStartTerm EnterNode
  return $ chaoticIteration fram g sourceNode
