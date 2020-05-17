{-# LANGUAGE FlexibleContexts, OverloadedStrings, PatternSynonyms #-}

module Languages.Imp.CfgGen (
  makeExpCfg
, makePathSensitiveCfg
) where


import Control.Monad ( (=<<) )
import Control.Monad.State ( State, gets, modify, evalState )

import qualified Data.Map as TreeMap
import           Data.HashMap.Strict ( HashMap, (!) )
import qualified Data.HashMap.Strict as Map

import System.IO.Unsafe ( unsafePerformIO )

import Data.Interned.ByteString ( InternedByteString(..) )

import CfgGenRuntime
import Configuration
import Graph
import Lang
import Semantics.Abstraction
import Semantics.AbstractMachine
import Semantics.Context
import Semantics.Conversion
import Semantics.SOS
import Term

import           Languages.Imp.Imp hiding ( True, False )
import qualified Languages.Imp.Imp as Imp

-----------------------------------------

import Debug.Trace


----------------------------------------------------------------
---------------------- Path-sensitive CFG ----------------------
----------------------------------------------------------------


----- The AM machinery does not track provenance of each abstract state (i.e.:
----- does not track the original term that each abstract term corresponds to).
-----
----- The generated CFG has the right shape, but each node stores only exactly
----- the data needed to distinguish among program points as defined by the theory.
----- To add back the other information I want for analysis, and get it in the format
----- used by my already-written static analysis micro-framework, I could:
-----
----- 1) Redo the rewriting system to track provenance
----- 2) Use a hack to keep the information need for a specific analysis, and
-----    transform the graph into the desired format
----- 3) Use a hack to keep the information need for a specific analysis, and
-----    write new static analysis code on top of the "abstract AM transition graph"
-----    format output by the interpreted-mode CFG generator.
-----
-----
----- Option 1 would only make sense if I intended to turn Mandate into a framework
----- for actually building useful things / expected to make more future use of
----- the interpreted-mode CFGs.
-----
----- I chose option 2, because I, probably mistakenly, believed it to be easier than option 3.
-----
----- (Observation if ever decide to try #1: I think it's sufficient to just
-----  associate abstract states to subtrees of the original program by annotating
-----  the head of each term; the head doesn't change until execution happens.)

{-# NOINLINE amRules #-}
amRules :: NamedAMRules ImpLang
amRules = unsafePerformIO $ do
  rules <- rules :: IO (NamedRules ImpLang)
  pamRules <- sosToPam rules
  pamToAM pamRules

pathSensitiveAmCfg :: Term ImpLang -> InternedByteString -> IO (Graph (AMState ImpLang))
pathSensitiveAmCfg t trackingVar = abstractAmCfg (irrelevance (VarNotIrr trackingVar)) (irrelevance (VarNotIrr trackingVar)) amRules t



data ConvertCfgState = ConvertCfgState {
    counter :: Int

    -- (value of tracking var, term, context)
  , oneNodeTermNodes :: HashMap (Maybe Bool, Term ImpLang, Context ImpLang) (GraphNode ImpLang)

    -- (Enter or exit, value of tracking var, context)
  , nodesByKont :: HashMap (NodeType, Maybe Bool, Context ImpLang) (GraphNode ImpLang)
  }

startState :: ConvertCfgState
startState = ConvertCfgState 0 Map.empty Map.empty

nextCounter :: State ConvertCfgState Int
nextCounter = do i <- gets counter
                 modify (\s -> s {counter = counter s + 1})
                 return i


onlyOneNode :: Term ImpLang -> Bool
onlyOneNode (If _ _ _) = True
onlyOneNode (Seq _ _)  = True
onlyOneNode _          = False


getTrackingVal :: InternedByteString -> RedState ImpLang -> Maybe Bool
getTrackingVal trackingVar (JustSimpMap (SimpEnvMap s)) =
  case TreeMap.lookup (VarName trackingVar) s of
    Just Imp.True  -> Just Prelude.True
    Just Imp.False -> Just Prelude.False
    Nothing        -> Nothing

getNode :: HashMap (Maybe Bool, Context ImpLang) (Term ImpLang) -> InternedByteString -> AMState ImpLang -> State ConvertCfgState (GraphNode ImpLang)
getNode kontMap trackingVar (AMState (Conf t st) k) =
  if onlyOneNode t then do
    m <- gets oneNodeTermNodes
    case Map.lookup (tvVal, t, k) m of
      Just n -> return n
      Nothing -> do i <- nextCounter
                    let n = GraphNode i typ t
                    modify (\s -> s {oneNodeTermNodes = Map.insert (tvVal, t, k) n m})
                    return $ GraphNode i typ t
  else do
    m <- gets nodesByKont
    case Map.lookup (typ, tvVal, k) m of
      Just n -> return n
      Nothing -> do i <- nextCounter
                    let n = GraphNode i typ (termForKont (tvVal, k))
                    modify (\s -> s {nodesByKont = Map.insert (typ, tvVal, k) n m})
                    return n
  where
    termForKont :: (Maybe Bool, Context ImpLang) -> Term ImpLang
    termForKont (v, k) = case Map.lookup (v, k) kontMap of
                           Just t  -> t
                           Nothing -> case Map.lookup (Nothing, k) kontMap of
                             Just t -> t          -- This case fires on an ExitNode, where the split
                                                  -- happened between it and the corresponding EnterNode
                             Nothing -> Skip  -- This case fires because Skip nodes are values and have no Enter node.


    tvVal = getTrackingVal trackingVar st

    typ = case matchTypeForTerm t of
            ValueOnly -> ExitNode
            _         -> EnterNode


-- Converter between CFG formats. (Mostly) doesn't change the shape of the graph,
-- but does change the annotations on each node.
--
---Notes:
--  *  Will not generate nodes for values already in the program
-- (as the syntax-directed one is based on a modified semantics where there are no
-- such things).
--
-- * Will erroneously merge nodes after a split but before the assignment to a variable. Should
--   be pretty harmless.
--
-- * BIG PROBLEM: Remember how the exit nodes of two branches of an if statement tend to get merged?
--                Well, that's a problem. I think the lesson is that terms should really be annotating
--                the edges, not the nodes. Yikes. Only saved because, in the main program of interest,
--                the only conditionals in question split on the tracking var
--
amCfgToStaticCfg :: InternedByteString -> Graph (AMState ImpLang) -> Graph (GraphNode ImpLang)
amCfgToStaticCfg trackingVar amGraph = evalState (go (edgeList amGraph) Graph.empty) startState
 where
   assocKont :: AMState ImpLang -> HashMap (Maybe Bool, Context ImpLang) (Term ImpLang)
   assocKont (AMState (Conf t s) k) = if onlyOneNode t then
                                        Map.empty
                                      else
                                        case matchTypeForTerm t of
                                          NonvalOnly -> Map.singleton (getTrackingVal trackingVar s, k) t
                                          _          -> Map.empty

   kontMap ::HashMap (Maybe Bool, Context ImpLang) (Term ImpLang)
   kontMap = foldMap assocKont $ nodeList amGraph

   getNode' = getNode kontMap trackingVar

   go :: [(AMState ImpLang, EdgeType, AMState ImpLang)]
      -> Graph (GraphNode ImpLang)
      -> State ConvertCfgState (Graph (GraphNode ImpLang))
   go []               g = return g
   go ((s1, _, s2):es) g = go es =<< (insert <$> getNode' s1 <*> pure NormalEdge <*> getNode' s2 <*> pure g)


makePathSensitiveCfg :: Term ImpLang -> InternedByteString -> IO (Graph (GraphNode ImpLang))
makePathSensitiveCfg t v = amCfgToStaticCfg v <$> pathSensitiveAmCfg t v

----------------------------------------------------------------
----------------------- Exp CFG --------------------------------
----------------------------------------------------------------

makeExpCfg :: Term ImpLang -> Graph (GraphNode ImpLang)
makeExpCfg t = withoutIsolatedNodes $ runGraphGen $ genCfg t

----- Verbatim output of the following commands:
--
-- > :script ghci-scripts/run-imp
-- > gs <- makeGraphPatterns (irrelevance ValueIrr) (irrelevance ValueIrr) amRules signature
-- > graphPatternsToCode gs

genCfg t@(Node "Write" [a]) = do (tIn, tOut) <- makeInOut t
                                 (aIn, aOut) <- genCfg a
                                 connect tIn aIn
                                 connect aOut tOut
                                 return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "WriteInt" [a]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    connect tIn aIn
                                    connect aOut tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "If" [a, b, c]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    (cIn, cOut) <- genCfg c
                                    connect tIn aIn
                                    connect aOut cIn
                                    connect aOut bIn
                                    return (inNodes [tIn], outNodes [cOut,bOut])
genCfg t@(Node ":=" [a, b]) = do (tIn, tOut) <- makeInOut t
                                 (aIn, aOut) <- genCfg a
                                 (bIn, bOut) <- genCfg b
                                 connect bOut tOut
                                 connect tIn bIn
                                 return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Var" [a]) = do (tIn, tOut) <- makeInOut t
                               (aIn, aOut) <- genCfg a
                               return (inNodes [tIn], outNodes [tIn])
genCfg t@(Node "ReadInt" []) = do (tIn, tOut) <- makeInOut t
                                  connect tIn tOut
                                  return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "Seq" [a, b]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  (bIn, bOut) <- genCfg b
                                  connect tIn aIn
                                  connect aOut bIn
                                  return (inNodes [tIn], outNodes [bOut])
genCfg t@(Node "VarExp" [a]) = do (tIn, tOut) <- makeInOut t
                                  (aIn, aOut) <- genCfg a
                                  connect tIn tOut
                                  return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "+" [a, b]) = do (tIn, tOut) <- makeInOut t
                                (aIn, aOut) <- genCfg a
                                (bIn, bOut) <- genCfg b
                                connect aOut bIn
                                connect bOut tOut
                                connect tIn aIn
                                return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "<" [a, b]) = do (tIn, tOut) <- makeInOut t
                                (aIn, aOut) <- genCfg a
                                (bIn, bOut) <- genCfg b
                                connect aOut bIn
                                connect tIn aIn
                                connect bOut tOut
                                return (inNodes [tIn], outNodes [tOut])
genCfg t@(Node "While" [a, b]) = do (tIn, tOut) <- makeInOut t
                                    (aIn, aOut) <- genCfg a
                                    (bIn, bOut) <- genCfg b
                                    connect bOut tIn
                                    connect tIn aIn
                                    connect aOut bIn
                                    connect aOut tOut
                                    return (inNodes [tIn], outNodes [tOut])
genCfg t@(Val _ _) = do (a, b) <- makeInOut t
                        connect a b
                        return (a, b)
genCfg t@(StrNode _ _) = makeInOut t
genCfg t@(IntNode _ _) = makeInOut t