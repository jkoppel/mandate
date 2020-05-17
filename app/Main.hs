{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Main where

import System.Environment
import System.Exit

import Control.DeepSeq
import Configuration
import Data.Map ( Map )
import Data.Text.Lazy as Lazy

import Data.GraphViz as GraphViz
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Printing

import Data.Interned.ByteString ( InternedByteString(..) )

import CfgGenRuntime
import Graph
import Rose
import Semantics.AbstractMachine
import Semantics.Abstraction
import Semantics.Conversion
import Semantics.PAM
import Semantics.GeneralMachine as GM
import Semantics.GraphPattern
import Term

import Lang
import Semantics.SOS

import Languages.Translation

import Languages.AddMul

import Languages.Analysis.ConstProp
import Languages.Analysis.Monotone
import Languages.LockStep

import Languages.MITScript.Analyze   as MIT
import Languages.MITScript.CfgGen    as MIT
import Languages.MITScript.Parse     as MIT
import Languages.MITScript.Semantics as MIT
import Languages.MITScript.Signature as MIT
import Languages.MITScript.Syntax    as MIT
import Languages.MITScript.Translate as MIT

import Languages.Imp.Analyze as Imp
import Languages.Imp.CfgGen  as Imp
import Languages.Imp.Imp     as Imp


import Languages.Tiger.Analyze   as Tiger
import Languages.Tiger.CfgGen    as Tiger
import Languages.Tiger.Parse     as Tiger
import Languages.Tiger.Semantics as Tiger
import Languages.Tiger.Signature as Tiger
import Languages.Tiger.Translate as Tiger

---------------------------------------------------------------------------------------------------------

analyzeConstPropImp :: Term ImpLang -> Map (GraphNode ImpLang) ConstPropState
analyzeConstPropImp t = chaoticIteration fram g sourceNode
  where
    fram = Imp.constPropFramework (getImpVars t)
    g    = Imp.makeExpCfg t
    sourceNode = nodeForTerm (nodeList g) t EnterNode

analyzeConstPropMIT :: Term MITScript -> Map (GraphNode MITScript) ConstPropState
analyzeConstPropMIT t = chaoticIteration fram g sourceNode
  where
    fram = MIT.constPropFramework (getMITScriptVars t)
    g    = MIT.makeExpCfg t
    sourceNode = nodeForTerm (nodeList g) t EnterNode

analyzeConstPropTiger :: Term Tiger -> Map (GraphNode Tiger) ConstPropState
analyzeConstPropTiger t = chaoticIteration fram g sourceNode
  where
    fram = Tiger.constPropFramework (getTigerVars t)
    g    = Tiger.makeExpCfg t
    sourceNode = nodeForTerm (nodeList g) t EnterNode


---------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  validate args (Prelude.length args) >>= putStrLn

usage = "Usage: derive-cfg [language name] [path to source]"

validate :: [String] -> Int -> IO String
validate args 2 = makeGraph (Prelude.head args) (Prelude.last args)
validate _ _ = return usage

makeGraph :: String -> String -> IO String
makeGraph lang fs = case lang of
  "mitscript" -> graphToString <$> convertMITScript fs 
  "tiger"     -> graphToString <$> convertTiger fs
  "imp"       -> graphToString <$> convertImp fs
  _           -> return "Unsupported language"

convertTiger fs = do
  x <- Tiger.parseFile fs
  tigerRules <- (rules :: IO (NamedRules Tiger))
  pamRules <- sosToPam tigerRules
  amRules <- pamToAM pamRules
  absCfg <- abstractAmCfg (irrelevance ValueIrr) (irrelevance ValueIrr) amRules (toGeneric x :: Term Tiger)
  return absCfg

convertMITScript fs = do
  x <- MIT.parseFile fs
  mitScriptRules <- (rules :: IO (NamedRules MITScript))
  pamRules <- sosToPam mitScriptRules
  amRules <- pamToAM pamRules
  absCfg <- abstractAmCfg (irrelevance (SortIrr "Exp")) (irrelevance (SortIrr "Exp")) amRules (toGeneric x :: Term MITScript)
  return absCfg

convertImp fs = do
  let x = case fs of
            "term1"             -> Imp.term1
            "term3"             -> Imp.term3
            "term4"             -> Imp.term4
            "termBalanceParens" -> Imp.termBalanceParens

  impRules <- (rules :: IO (NamedRules ImpLang))
  pamRules <- sosToPam impRules
  amRules <- pamToAM pamRules
  absCfg <- abstractAmCfg (irrelevance (VarNotIrr "b")) (irrelevance (VarNotIrr "b")) amRules x
  return absCfg

graphToString graph = unpack $ renderDot $ toDot $ graphToDot (nonClusteredParams { fmtNode = \(n,l) -> [toLabel l, shape BoxShape], fmtEdge = \(n1,n2,l) -> [toLabel l, shape BoxShape]}) (toRealConvGraph @Gr graph id)

getTerm :: AMState l -> Term l
getTerm (AMState (Conf t _) _) = t