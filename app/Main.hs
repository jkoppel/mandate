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
  case args of
    ["interpreted-cfg", language, file] -> putStrLn =<< makeGraphInterpreted language file
    ["compiled-cfg",    language, file] -> putStrLn =<< makeGraphCompiled    language file
    ["const-prop",      language, file] -> putStrLn =<< doConstProp          language file
    _                                   -> putStrLn usage

usage =    "Usage: derive-cfg <command> <language name> <path to source> \n"
        ++ "Valid commands: interpreted-cfg, compiled-cfg, const-prop \n"
	++ "Valid languages: tiger, mitscript, imp \n"
	++ "\n"
	++ "The abstraction to be used in each CFG is hardcoded. Check source for details."
	++ "\n"
	++ "For imp: Instead of file path, write either: term1, term3, term4, termBalanceParens"

makeGraphInterpreted :: String -> String -> IO String
makeGraphInterpreted lang fs = case lang of
  "mitscript" -> graphToString <$> convertMITScript fs
  "tiger"     -> graphToString <$> convertTiger fs
  "imp"       -> graphToString <$> convertImp fs
  _           -> return "Unsupported language"

makeGraphCompiled :: String -> String -> IO String
makeGraphCompiled lang fs = case lang of
  "mitscript" -> graphToString <$> MIT.makeExpCfg   <$> toGeneric <$> MIT.parseFile   fs
  "tiger"     -> graphToString <$> Tiger.makeExpCfg <$> toGeneric <$> Tiger.parseFile fs
  "imp"       -> return $ graphToString $ Imp.makeExpCfg $ impGetTermByName fs
  _           -> return "Unsupported language"

doConstProp :: String -> String -> IO String
doConstProp lang fs = case lang of
  "mitscript" -> show <$> analyzeConstPropMIT   <$> toGeneric <$> MIT.parseFile   fs
  "tiger"     -> show <$> analyzeConstPropTiger <$> toGeneric <$> Tiger.parseFile fs
  "imp"       -> show <$> analyzeConstPropImp   <$> return (impGetTermByName fs)
  _           -> return "Unsupported language"


convertTiger fs = do
  x <- Tiger.parseFile fs
  tigerRules <- (rules :: IO (NamedRules Tiger))
  pamRules <- sosToPam tigerRules
  amRules <- pamToAM pamRules
  absCfg <- abstractAmCfg (irrelevance ValueIrr) (Tiger.irrSkippingFunScope ValueIrr) amRules (toGeneric x :: Term Tiger)
  return absCfg

convertMITScript fs = do
  x <- MIT.parseFile fs
  mitScriptRules <- (rules :: IO (NamedRules MITScript))
  pamRules <- sosToPam mitScriptRules
  amRules <- pamToAM pamRules
  absCfg <- abstractAmCfg (irrelevance (SortIrr "Exp")) (MIT.irrSkippingScope (SortIrr "Exp")) amRules (toGeneric x :: Term MITScript)
  return absCfg

impGetTermByName :: String -> Term ImpLang
impGetTermByName "term1"             = Imp.term1
impGetTermByName "term3"             = Imp.term3
impGetTermByName "term4"             = Imp.term4
impGetTermByName "termBalanceParens" = Imp.termBalanceParens
impGetTermByName x                   = error ("Unrecognized Imp term: " ++ x)

convertImp fs = do
  let x = impGetTermByName fs

  impRules <- (rules :: IO (NamedRules ImpLang))
  pamRules <- sosToPam impRules
  amRules <- pamToAM pamRules
  absCfg <- abstractAmCfg (irrelevance (VarNotIrr "b")) (irrelevance (VarNotIrr "b")) amRules x
  return absCfg

graphToString graph = unpack $ renderDot $ toDot $ graphToDot (nonClusteredParams { fmtNode = \(n,l) -> [toLabel l, shape BoxShape], fmtEdge = \(n1,n2,l) -> [toLabel l, shape BoxShape]}) (toRealConvGraph @Gr graph id)

getTerm :: AMState l -> Term l
getTerm (AMState (Conf t _) _) = t