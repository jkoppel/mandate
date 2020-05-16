{-# LANGUAGE PatternSynonyms, ScopedTypeVariables, TupleSections #-}

module Languages.Imp.Analyze (

) where

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


data ConstVal = Top | Bottom | Known Integer
  deriving ( Eq, Ord, Show )

type ConstPropState = Map InternedByteString ConstVal

data MonotoneFramework s l = MonotoneFramework {
    bottom :: s
  , join :: s -> s -> s
  , transfer :: Term l -> s -> s
  }


joinConstVal :: ConstVal -> ConstVal -> ConstVal
joinConstVal Top       b         = Top
joinConstVal a         Top       = Top
joinConstVal Bottom    b         = b
joinConstVal a         Bottom    = a
joinConstVal (Known a) (Known b) = if a == b then Known a else Top

joinConstMap :: (Ord k) => Map k ConstVal -> Map k ConstVal -> Map k ConstVal
joinConstMap = Map.unionWith joinConstVal


transferImpConst :: Term ImpLang -> ConstPropState -> ConstPropState
transferImpConst (v := (EVal (Const n))) s = Map.insert v (Known n) s
transferImpConst (v := _)                s = Map.insert v Top s
transferImpConst _                                    s = s

constPropFramework :: Set InternedByteString -> MonotoneFramework ConstPropState ImpLang
constPropFramework vars = MonotoneFramework { bottom = Map.fromList $ map (,Bottom) $ Set.toList vars
                                            , join = joinConstMap
                                            , transfer = transferImpConst
                                            }

forMap :: (Ord k) => Map k v -> (k -> v -> v) -> Map k v
forMap = flip Map.mapWithKey

iterateToFixpoint :: (Eq a) => (a -> a) -> a -> a
iterateToFixpoint f x = let next = f x in
                        if next == x then x else iterateToFixpoint f next

chaoticIteration :: forall s l. (Eq s) => MonotoneFramework s l -> Graph (GraphNode l) -> Map (GraphNode l) s
chaoticIteration fram g = iterateToFixpoint update startState
  where
    startState = foldr (\n s -> Map.insert n (bottom fram) s) Map.empty (nodeList g)

    doTransfer :: Map (GraphNode l) s -> GraphNode l -> s
    doTransfer st n = case graphNode_type n of
                        EnterNode -> transfer fram (graphNode_term n) (st ! n)
                        ExitNode  -> st ! n

    update :: Map (GraphNode l) s -> Map (GraphNode l) s
    update oldSt = forMap oldSt $ \n oldVal ->
                     foldr (join fram) oldVal $
                       map (doTransfer oldSt) (Graph.preds g n)

--------------------------------

analyzeConstProp :: Term ImpLang -> Map (GraphNode ImpLang) ConstPropState
analyzeConstProp t = chaoticIteration fram g
  where
    fram = constPropFramework (getImpVars t)
    g = makeExpCfg t