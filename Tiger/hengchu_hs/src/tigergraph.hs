module TigerGraph
  (
    Graph
  , Node
  , UNode
  , Edge
  , nodes
  , succ
  , pred
  , adj
  , emptyGraph
  , newNode
  , newNode1
  , rmNode
  , mkEdge
  , mkEdge1
  , rmEdge
  , graph2dotfile
  , unlabeledNode
  , labeledNode
  )
  where

import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.Graph.Inductive.PatriciaTree as GrImpl
import Data.List
import Prelude hiding (succ, pred)
import Control.Monad.State
import Data.Maybe

type Graph a b = GrImpl.Gr a b
type Node a = Gr.LNode a
type UNode = Gr.Node
type Edge b = Gr.LEdge b

unlabeledNode :: Node a -> UNode
unlabeledNode = fst

labeledNode :: UNode -> Graph a b -> Maybe (Node a)
labeledNode n g = let l = Gr.lab g n
                  in  case l of
                        Nothing -> Nothing
                        Just l' -> Just (n, l')

nodes :: Graph a b -> [Node a]
nodes g = let ns = Gr.nodes g
              labs = map (fromJust . Gr.lab g) ns
          in  zip ns labs

succ :: Node a -> Graph a b -> [Node a]
succ n g = let ns = map fst $ Gr.lsuc g (fst n)
               labs = map (fromJust . Gr.lab g) ns
           in  zip ns labs

pred :: Node a -> Graph a b -> [Node a]
pred n g = let ns = map fst $ Gr.lpre g (fst n)
               labs = map (fromJust . Gr.lab g) ns
           in  zip ns labs

adj :: (Eq a) => Node a -> Graph a b -> [Node a]
adj n g = nub $ succ n g ++ pred n g

emptyGraph :: Graph a b
emptyGraph = Gr.empty

newNode :: Graph a b -> a -> (Node a, Graph a b)
newNode g lab = let n = head $ Gr.newNodes 1 g
                    g' = Gr.insNode (n, lab) g
                in  ((n,lab), g')

newNode1 :: Graph () b -> (Node (), Graph () b)
newNode1 g = newNode g ()

rmNode :: UNode -> Graph a b -> Graph a b
rmNode n g = Gr.delNode n g

mkEdge :: Node a -> Node a -> b -> Graph a b -> Graph a b
mkEdge n1 n2 lab g = let edge = (fst n1, fst n2, lab)
                     in  Gr.insEdge edge g

mkEdge1 :: Node a -> Node a -> Graph a () -> Graph a ()
mkEdge1 n1 n2 g = mkEdge n1 n2 () g

rmEdge :: Node a -> Node a -> Graph a b -> Graph a b
rmEdge n1 n2 g = let edge = (fst n1, fst n2)
                 in  Gr.delEdge edge g

type PrintGraph = State String

printgraph :: (Show a, Show b) => String -> Graph a b -> Bool -> PrintGraph ()
printgraph name g isdirected = 
  let
    emit :: String -> PrintGraph ()
    emit str = do s <- get
                  put $ s ++ str

    arrow :: PrintGraph ()
    arrow = if isdirected then emit " -> " else emit " -- "

    newln :: PrintGraph ()
    newln = emit "\n"

    semicolon :: PrintGraph ()
    semicolon = emit ";"

    endstmt :: PrintGraph ()
    endstmt = semicolon >> newln

    emitnode :: (Show a) => Node a -> PrintGraph ()
    emitnode n = emit $ show $ fst n

    printnode n = do let succs = succ n g
                     if length succs > 0
                        then mapM_ (\n' -> emitnode n >> arrow >> emitnode n' >> endstmt) succs
                        else emitnode n >> endstmt

    printnodelab :: (Show a) => Node a -> PrintGraph ()
    printnodelab n = let n' = show $ fst n
                         lab = show $ snd n
                     in do emitnode n
                           emit " ["
                           emit $ "label=\"(" ++ n' ++ ", " ++ lab ++ ")\""
                           emit "]"
                           endstmt

  in do let ns = nodes g
        let title = if isdirected then "digraph" else "graph"
        emit $ title ++ " " ++ name ++ " {"
        newln
        mapM_ printnodelab ns
        mapM_ printnode ns
        emit "}"

graph2dotfile :: (Show a, Show b) => String -> Graph a b -> Bool -> String
graph2dotfile grname gr directed = execState (printgraph grname gr directed) ""
