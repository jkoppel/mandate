module TigerFlow
  (
    FlowGraph(..)
  , instrs2graph
  , FNode
  )
  where

import qualified TigerGraph as Gr
import qualified Data.Map as Map
import TigerAssem
import TigerTemp
import Control.Monad.State
import Prelude hiding (succ)

data FlowGraph = FGRAPH { control :: Gr.Graph Instr ()
                        , def :: Map.Map (Gr.UNode) [Temp]
                        , use :: Map.Map (Gr.UNode) [Temp]
                        , ismove :: Map.Map (Gr.UNode) Bool
                        }

instrs2graph :: [Instr] -> (FlowGraph, [FNode])
instrs2graph instrs = let (f, d, u) 
                            = execState (instrs2graph' instrs)
                                        (Gr.emptyGraph, Map.empty, Map.empty)
                          nodes = Gr.nodes f

                          insertIsMove (i, MOV _ _ _) m = Map.insert i True m
                          insertIsMove (_, _) m = m

                          im = foldr insertIsMove Map.empty nodes
                      in (FGRAPH f d u im, nodes)

type DefMap = Map.Map (Gr.UNode) [Temp]
type UseMap = Map.Map (Gr.UNode) [Temp]
type FNode = Gr.Node Instr
type GraphCon = State (Gr.Graph Instr (), DefMap, UseMap)

getGraph :: GraphCon (Gr.Graph Instr ())
getGraph = do (g, _, _) <- get
              return g

putGraph :: Gr.Graph Instr () -> GraphCon ()
putGraph g = do (_, d, u) <- get
                put (g, d, u)

getDefMap :: GraphCon DefMap
getDefMap = do (_, d, _) <- get
               return d

putDefMap :: DefMap -> GraphCon ()
putDefMap d = do (g, _, u) <- get
                 put (g, d, u)

getUseMap :: GraphCon UseMap
getUseMap = do (_, _, u) <- get
               return u

putUseMap :: UseMap -> GraphCon ()
putUseMap u = do (g, d, _) <- get
                 put (g, d, u)

newNode :: Instr -> GraphCon (FNode)
newNode assem = do g <- getGraph
                   let (n, g') = Gr.newNode g assem
                   putGraph g'
                   return n

newEdge :: FNode -> FNode -> GraphCon ()
newEdge n1 n2 = do g <- getGraph
                   let g' = Gr.mkEdge1 n1 n2 g
                   putGraph g'

setDefs :: FNode -> GraphCon ()
setDefs node = do let instr = snd node
                  let ds = defs instr
                  d <- getDefMap
                  putDefMap $ Map.insert (Gr.unlabeledNode node) ds d

setUses :: FNode -> GraphCon ()
setUses node = do let instr = snd node
                  let us = uses instr
                  d <- getUseMap
                  putUseMap $ Map.insert (Gr.unlabeledNode node) us d
                 

allEdges :: [FNode] -> [(FNode, FNode)]
allEdges nodes = allEdges' nodes nodes []

allEdges' :: [FNode] -> [FNode]
                             -> [(FNode, FNode)] 
                             -> [(FNode, FNode)]
allEdges' _ [] edges = edges
allEdges' ns [n] edges = let e = jumpEdges ns n
                         in  e ++ edges
allEdges' ns (n1:n2:ns') edges = let e1 = (n1, n2)
                                     jes = jumpEdges ns n1
                                 in  allEdges' ns (n2:ns') $ jes ++ (e1:edges)
jumpEdges :: [FNode] -> FNode -> [(FNode, FNode)]
jumpEdges ns n@(_, OPER _ _ _ labs) = 
  let 
      filterhelper :: Lab -> FNode -> Bool
      filterhelper lab1 (_, LABEL lab2) = lab1 == lab2
      filterhelper _ _                  = False

      succ lab = filter (\node -> filterhelper lab node) ns
      succs ls = concatMap succ ls
  in
    case labs of
      Nothing -> []
      Just labs' -> map (\s -> (n, s)) $ succs labs'
jumpEdges _ _ = []


instrs2graph' :: [Instr] -> GraphCon ()
instrs2graph' assems = do nodes <- mapM newNode assems
                          mapM_ setDefs nodes
                          mapM_ setUses nodes
                          let edges = allEdges nodes
                          mapM_ (uncurry newEdge) edges
