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
import Languages.MITScript.Parse as MITParse
import Languages.MITScript.Semantics
import Languages.MITScript.Signature
import Languages.MITScript.Syntax
import Languages.MITScript.Translate

import Languages.Imp.Analyze as Imp
import Languages.Imp.CfgGen  as Imp
import Languages.Imp.Imp     as Imp

import Languages.Tiger.Parse as TigerParse
import Languages.Tiger.Semantics
import Languages.Tiger.Signature
import Languages.Tiger.Translate

---------------------------------------------------------------------------------------------------------


analyzeConstProp :: Term ImpLang -> Map (GraphNode ImpLang) ConstPropState
analyzeConstProp t = chaoticIteration fram g
  where
    fram = Imp.constPropFramework (getImpVars t)
    g    = Imp.makeExpCfg t

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
  x <- TigerParse.parseFile fs
  tigerRules <- (rules :: IO (NamedRules Tiger))
  pamRules <- sosToPam tigerRules
  amRules <- pamToAM pamRules
  absCfg <- abstractAmCfg (irrelevance ValueIrr) (irrelevance ValueIrr) amRules (toGeneric x :: Term Tiger)
  return absCfg

convertMITScript fs = do
  x <- MITParse.parseFile fs
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