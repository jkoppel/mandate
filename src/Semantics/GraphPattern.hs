{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeApplications #-}


module Semantics.GraphPattern (
    HasTopState(..)
  , abstractGraphPattern
  , makeGraphPatterns
  , graphPatternsToCode
  ) where

import Prelude hiding ( (<>) )

import Control.Monad ( forM, (=<<) )
import Data.Foldable ( foldMap )
import Data.Functor ( (<&>) )
import Data.Hashable ( Hashable )
import Data.HashMap.Strict ( HashMap, (!) )
import qualified Data.HashMap.Strict as HM
import Data.List ( concat, intercalate )
import Data.Map ( Map )
import qualified Data.Map as Map

import Language.Haskell.TH ( nameBase )

import Text.Printf ( printf )
import Text.PrettyPrint ( Doc, (<>), (<+>), ($$), punctuate, brackets, hcat, vcat, text)

import CfgGenRuntime
import Configuration
import Graph
import Lang
import Lattice
import Semantics.Abstraction
import Semantics.AbstractMachine
import Semantics.Context
import Term
import TransitionSystem
import Var

-- This can be different from (UpperBound (RedState l)).
-- Main difference: top for environments is [AllStar -> AllStar], but
-- you may use environments which only store values, and hence top would be [ValStar -> ValStar]
class (Lang l) => HasTopState l where
  topRedState :: RedState l

-- | This is useful because the Configuration value holds
-- some useful instances
swapState :: Configuration l -> RedState l -> Configuration l
swapState (Conf t _) s = Conf t s

conf :: (Lang l) => Term l -> RedState l -> Configuration l
conf t s = swapState (initConf t) s

abstractGraphPattern :: forall l. (HasTopState l) => Abstraction (CompFunc l) -> Abstraction (AMState l) -> NamedAMRules l -> Term l -> IO (Graph (AMState l))
abstractGraphPattern absFunc abs rules t = do
    initState <- abs <$> AMState (conf t (topRedState @l)) <$> KVar <$> nextVar
    genTransitionGraph step initState
  where
    step :: AMState l -> IO [(AMState l, EdgeType)]
    -- IDEA: If we used Top/Halt instead of a KVar for the graph pattern top, wouldn't need these next two cases
    step (AMState (Conf ValStar _)    (KVar _)) = return []
    step (AMState (Conf (ValVar _) _) (KVar _)) = return []
    step as@(AMState (Conf  NonvalStar   _) k) = return [(AMState (Conf ValStar (topRedState @l)) k, TransitiveEdge)]
    step as@(AMState (Conf (NonvalVar _) _) k) = nextVar >>= \v -> return [(AMState (Conf (ValVar v) (topRedState @l)) k, TransitiveEdge)]
    step as = map (,NormalEdge) <$> map abs <$> stepAmNarrowing (map (abstractCompFuncs absFunc) rules) as


--FIXME: The way this works (namely, instantiating children as nonval nodes) means that
--things like "Var" (distinct form VarExp) which do not take place in computation must be marked as
--Node/nonval, but will erroneously have code generated for them.
--
-- Doing proper two-level nondeterminism would enable us to use All vars in place of nonval vars,
-- and would fix this problem
makeGraphPatterns :: (HasTopState l) => Abstraction (CompFunc l) -> Abstraction (AMState l)
                                     -> NamedAMRules l -> Signature l
                                     -> IO (Map Symbol (Graph (AMState l)))
makeGraphPatterns absFunc abs rules sig = flip foldMap nodeSigs $ \n ->
                                                   Map.singleton (sigNodeSymbol n)
                                                             <$> (makePattern =<< canonicalElt n)
  where
    isNode (NodeSig _ _ _) = True
    isNode _               = False

    nodeSigs = filter isNode sigNodes
      where sigNodes = case sig of (Signature s) -> s

    canonicalElt :: SigNode -> IO (Term l)
    canonicalElt (NodeSig sym children _) = Node sym <$> mapM (const (NonvalVar <$> nextVar)) children
    --canonicalElt (NodeSig sym children _) = return $ Node sym (map (const NonvalStar) children)

    makePattern = abstractGraphPattern absFunc abs rules




---------------------------------------------------------------------------------------------------


graphPatternsToCode :: (Lang l) => Map Symbol (Graph (AMState l)) -> Doc
graphPatternsToCode pats = (vcat $ map (\(k,v) -> graphPatternToCode k v) (Map.assocs pats)) $$ valCase

-- Three things: metavars, variables in the program, am states
-- maps: cfg node variable names <---> am states
--     , metavars <---> variables in the program

-- |
-- Strategy to implement algo:
--
-- V1 (works for muladd):
----- Hardcoded thing for Val
-----
----- makeInOut on the root term
---------- Create a program variable for each metaVar. genCfg on each
---------- Create map from AMState to program variable representing CFG node
---------- One wire statement per edge
-----
-- Recognizes 3 kinds of states: init state, a metavar, a ValStar. If a a state found not in thisi set.


{-
 - Example output for + node
 -
 - genCfg t@(Node "+" [a, b]) = do
 -   (tIn, tOut) <- makeInOut t
 -   (aIn, aOut) <- genCfg t1
 -   (bIn, bOut) <- genCfg t2
 -   wire tIn aIn
 -   wire aOut bIn
 -   wire bOut tOut
 -   return (tIn, tOut)

-}

get :: (Hashable a, Eq a) => HashMap a b -> a -> String -> b
get m a msg = case HM.lookup a m of
  Just b  -> b
  Nothing -> error msg

graphPatternToCode :: forall l. (Lang l) => Symbol -> Graph (AMState l) -> Doc
graphPatternToCode sym graphPat = dec <+> body
  where
    [startState] = filter isStartState (nodeList graphPat)
      where
        isStartState (AMState (Conf (Node s _) _) (KVar _))
                                               | s == sym = True
        isStartState _                                    = False

    (mvars, stackVar) = case startState of
                          AMState (Conf (Node _ vs) _)   k -> (vs, k)

    progVars :: HashMap MetaVar String
    progVars = go infNameList mvars HM.empty
      where
        go (nm:nms) ((NonvalVar v):vs) mp = go nms vs (HM.insert v nm mp)
        go nms      (_:vs)             mp = go nms vs mp
        go _        []                 mp = mp

        infNameList = map (:[]) ['a'..'z'] ++ map (++ "'") infNameList

    outVars :: HashMap MetaVar String
    outVars = foldr addV HM.empty $ transitiveEdgeList graphPat
      where
        addV (AMState (Conf (NonvalVar v) _) _, AMState (Conf (ValVar v') _) _) = HM.insert v' (progVars ! v)

    tName = "t"
    dec = text "genCfg " <> text tName <> text "@(Node \"" <> text (show sym)
                         <> text "\"" <+> brackets (hcat $ punctuate (text ", ") (map (text.argToVar) mvars)) <> text ") = do"
      where
        argToVar (NonvalVar v) = progVars ! v
        argToVar _             = "_"

    body = makeNodes $$ recursiveCalls $$ doWire $$ doReturn

    mkInNode  s = s ++ "In"
    mkOutNode s = s ++ "Out"

    inNode  = mkInNode  tName
    outNode = mkOutNode tName

    stateToVarNm :: AMState l -> String
    stateToVarNm = (stateMap !)
      where
        nodeVarForState' :: Term l -> Context l -> String
        nodeVarForState' (Node s _)   (KVar _) | s == sym = inNode
        nodeVarForState' ValStar      (KVar _)            = outNode
        nodeVarForState' (NonvalVar v) _                  = mkInNode  (get progVars v ("Progvar not found: " ++ show v))
        nodeVarForState' (ValVar v)    _                  = mkOutNode (get outVars  v ("Outvar not found: "  ++ show v))
        nodeVarForState' t k =
          error (printf "Graph pattern has state not yet supported by codegen: <%s | %s>" (show t) (show k))

        nodeVarForState :: AMState l -> String
        nodeVarForState (AMState (Conf n _) k) = nodeVarForState' n k

        stateMap :: HashMap (AMState l) String
        stateMap = foldl (\mp st -> HM.insert st (nodeVarForState st) mp)
                         HM.empty
                         (nodeList graphPat)

    finalNodes = map stateToVarNm $ sinks graphPat


    makeNodes = text (printf "(%s, %s) <- %s %s" inNode outNode (nameBase 'makeInOut) tName)

    recursiveCalls = vcat $ map recCall (HM.elems progVars)
      where
        recCall v = text (printf "(%s, %s) <- %s %s" (mkInNode v) (mkOutNode v) ("genCfg" :: String) v)

    doWire = vcat $ normalEdgeList graphPat <&> \(a, b) ->
                                             text (nameBase 'connect) <+> text (stateToVarNm a) <+> text (stateToVarNm b)

    doReturn = text (printf "return (%s [%s], %s [%s])" (nameBase 'inNodes) inNode (nameBase 'outNodes) (intercalate "," finalNodes))




valCase :: Doc
valCase = text ("genCfg t@(Val _ _) = do (a, b) <- %s t" `printf` nameBase 'makeInOut)
       $$ text ("                        %s a b"         `printf` nameBase 'connect)
       $$ text ("                        return (a, b)")