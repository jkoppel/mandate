module TigerInterference
  (
    IGraph(..)
  , interferenceGraph
  )
  where

import qualified TigerFlow as F
import qualified TigerGraph as Gr
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.State
import TigerTemp
import Debug.Trace (trace)
import Data.List (nubBy)
import Data.Tuple


type InterGraph = Gr.Graph Temp ()
type INode = Gr.Node Temp

data IGraph = IGRAPH { graph :: InterGraph
                     , tnode :: Map.Map Temp Gr.UNode
                     , gtemp :: Map.Map Gr.UNode Temp
                     , moves :: [(INode, INode)] }

interferenceGraph :: F.FlowGraph -> (IGraph, F.FNode -> [Temp])
interferenceGraph f@(F.FGRAPH g dfs _ _) =
  let livemap = liveinout f
      fnodes  = Gr.nodes g
      ulfnodes = map Gr.unlabeledNode fnodes

      getdefs n = case Map.lookup n dfs of
                    Just xs -> xs
                    Nothing -> trace ("34: Can't find defs for " ++ show n) []
      -- getuses n = fromJust $ Map.lookup n us
      getliveout n = case Map.lookup n livemap of
                       Nothing -> []
                       Just s  -> Set.toList s

      defs = map getdefs ulfnodes
      liveouts = map getliveout ulfnodes
      alltemps = Set.toList $ Set.fromList $ concat defs ++ concat liveouts

      makeEdges definedTemps liveOuts = [(d, l) | d <- definedTemps, l <- liveOuts]
      alledges = concat $ zipWith makeEdges defs liveouts
      
      createNodeHelper temp (nodes, gr) = let (n, g') = Gr.newNode gr temp
                                         in  (n:nodes, g')

      (internodes, intergraph) = foldr createNodeHelper ([], Gr.emptyGraph) alltemps

      tgmaphelper (i, t) (tmap, gmap) = let tmap' = Map.insert t i tmap
                                            gmap' = Map.insert i t gmap
                                        in  (tmap', gmap')
      (tn, gt) = foldr tgmaphelper (Map.empty, Map.empty) internodes

      edgetrans (t1, t2) = let n1 = fromJust $ Map.lookup t1 tn
                               n2 = fromJust $ Map.lookup t2 tn
                           in  ((n1, t1), (n2, t2))
    
      alledges1 = filter (\(a1, a2) -> a1 /= a2) $ nubBy (\m1 m2 -> m1 == m2 || swap m1 == m2) $ map edgetrans alledges

      intergraph1 = foldr (uncurry Gr.mkEdge1) intergraph alledges1

  in  (IGRAPH intergraph1 tn gt [], \(i, _) -> getliveout i)

liveinout :: F.FlowGraph -> Map.Map Gr.UNode (Set.Set Temp)
liveinout (F.FGRAPH g dfs us _) =
  let flowgraphnodes = Gr.nodes g

      getdefs n = Set.fromList $ case Map.lookup n dfs of
                                   Just xs -> xs
                                   Nothing -> trace ("72: Can't find defs for " ++ show n) []
      getuses n = Set.fromList $ case Map.lookup n us of
                                   Just xs -> xs
                                   Nothing -> trace ("75: Can't find uses for " ++ show n) []

      (\\) = (Set.\\)

      liveness n = do let n' = Gr.unlabeledNode n
                      outs' <- getOutForNode n'
                      let ins = Set.union (getuses n') (outs' \\ getdefs n')
                      let successors = Gr.succ n g
                      succins <- mapM getInForNode (map Gr.unlabeledNode successors)
                      let outs = Set.unions succins
                      setInForNode n' ins
                      setOutForNode n' outs

      loop = do m <- get
                mapM_ liveness flowgraphnodes
                m' <- get
                if m == m'
                   then return $ Map.map snd m
                   else loop

  in evalState loop Map.empty

type LiveInOut = State (Map.Map Gr.UNode (Set.Set Temp, Set.Set Temp))

setInForNode :: Gr.UNode -> Set.Set Temp -> LiveInOut ()
setInForNode n liveins = do m <- get
                            case Map.lookup n m of
                              Nothing -> put $ Map.insert n (liveins, Set.empty) m
                              Just (_, outs) -> put $ Map.insert n (liveins, outs) m

setOutForNode :: Gr.UNode -> Set.Set Temp -> LiveInOut ()
setOutForNode n liveouts = do m <- get
                              case Map.lookup n m of
                                Nothing -> put $ Map.insert n (Set.empty, liveouts) m
                                Just (ins, _) -> put $ Map.insert n (ins, liveouts) m

getInForNode :: Gr.UNode -> LiveInOut (Set.Set Temp)
getInForNode n = do m <- get
                    case Map.lookup n m of
                      Nothing -> return Set.empty
                      Just (ins, _) -> return ins

getOutForNode :: Gr.UNode -> LiveInOut (Set.Set Temp)
getOutForNode n = do m <- get
                     case Map.lookup n m of
                       Nothing -> return Set.empty
                       Just (_, outs) -> return outs
