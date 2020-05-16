{-# LANGUAGE PatternSynonyms, ScopedTypeVariables, TupleSections #-}

module Languages.Imp.Analyze (

) where

import Control.Arrow ( (***) )
import           Data.Map ( Map, (!) )
import qualified Data.Map.Strict as Map
import           Data.Set ( Set )
import qualified Data.Set as Set

import Data.Interned.ByteString ( InternedByteString )


import CfgGenRuntime
import Graph as Graph
import Term

import Languages.Imp.CfgGen
import Languages.Imp.Imp


------------------ Utils --------------------------------

forMap :: (Ord k) => Map k v -> (k -> v -> v) -> Map k v
forMap = flip Map.mapWithKey


-- Look ma! So inefficient
nodeForTerm :: [GraphNode l] -> Term l -> NodeType -> GraphNode l
nodeForTerm nodes t nt = head $ filter matchesTerm nodes
  where
    matchesTerm n = graphNode_type n == nt && graphNode_term n == t

---------------- Framework defn -------------------------


data MonotoneFramework s l = MonotoneFramework {
    bottom :: s
  , join :: s -> s -> s
  , transfer :: Map (GraphNode l) s -> Term l -> s -> s
  }


iterateToFixpoint :: (Eq a) => (a -> a) -> a -> a
iterateToFixpoint f x = let next = f x in
                        if next == x then x else iterateToFixpoint f next

chaoticIteration :: forall s l. (Eq s) => MonotoneFramework s l -> Graph (GraphNode l) -> Map (GraphNode l) s
chaoticIteration fram g = iterateToFixpoint update startState
  where
    startState = foldr (\n s -> Map.insert n (bottom fram) s) Map.empty (nodeList g)

    doTransfer :: GraphNode l -> Map (GraphNode l) s -> GraphNode l -> s
    doTransfer cur st n = case graphNode_type cur of
                            EnterNode -> st ! n
                            ExitNode  -> transfer fram st (graphNode_term cur) (st ! n)

    update :: Map (GraphNode l) s -> Map (GraphNode l) s
    update oldSt = forMap oldSt $ \n oldVal ->
                     foldr (join fram) oldVal $
                       map (doTransfer n oldSt) (Graph.preds g n)

-------------------------------------------------------------------
------------------------ Const Prop, language general -------------
-------------------------------------------------------------------


data ConstVal = Top | Bottom | Known Integer
  deriving ( Eq, Ord, Show )

data ConstPropState = ConstPropState {
      constProp_val  :: ConstVal
    , constProp_vars :: Map InternedByteString ConstVal
    }
  deriving (Eq, Ord, Show)

joinConstVal :: ConstVal -> ConstVal -> ConstVal
joinConstVal Top       b         = Top
joinConstVal a         Top       = Top
joinConstVal Bottom    b         = b
joinConstVal a         Bottom    = a
joinConstVal (Known a) (Known b) = if a == b then Known a else Top

joinConstMap :: (Ord k) => Map k ConstVal -> Map k ConstVal -> Map k ConstVal
joinConstMap = Map.unionWith joinConstVal

joinConstPropState :: ConstPropState -> ConstPropState -> ConstPropState
joinConstPropState (ConstPropState val1 vars1)
                   (ConstPropState val2 vars2) = ConstPropState (joinConstVal val1 val2) (joinConstMap vars1 vars2)

abstractOutput :: Map (GraphNode l) ConstPropState -> Term l -> ConstVal
abstractOutput st t = constProp_val $ st ! (nodeForTerm (Map.keys st) t ExitNode)


constValBinop :: (Integer -> Integer -> Integer) -> ConstVal -> ConstVal -> ConstVal
constValBinop op (Known a) (Known b) = Known (a `op` b)
constValBinop _  Bottom    _         = Bottom
constValBinop _  _         Bottom    = Bottom
constValBinop _  Top       _         = Top
constValBinop _  _         Top       = Top

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


------------------------------------------------------------------
--------------------------- Driver -------------------------------
------------------------------------------------------------------

analyzeConstProp :: Term ImpLang -> Map (GraphNode ImpLang) ConstPropState
analyzeConstProp t = chaoticIteration fram g
  where
    fram = constPropFramework (getImpVars t)
    g = makeExpCfg t