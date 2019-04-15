{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment
import System.Exit

import Control.DeepSeq
import Configuration
import Data.Text.Lazy as Lazy
import Data.GraphViz as GraphViz
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Printing
import Graph
import Rose
import Semantics.AbstractMachine
import Semantics.Abstraction
import Semantics.Conversion
import Semantics.PAM
import Term

import Lang
import Semantics.SOS

import Languages.Translation

import Languages.AddMul

import Languages.LockStep
import Languages.MITScript.Parse as MITParse
import Languages.MITScript.Semantics
import Languages.MITScript.Signature
import Languages.MITScript.Syntax
import Languages.MITScript.Translate

import Languages.Imp

import Languages.Tiger.Parse as TigerParse
import Languages.Tiger.Semantics
import Languages.Tiger.Signature
import Languages.Tiger.Translate

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
  _           -> return "Unsupported language"

convertMITScript fs = do
  x <- MITParse.parseFile fs
  mitScriptRules <- (rules :: IO (NamedRules MITScript))
  pamRules <- sosToPam mitScriptRules
  amRules <- pamToAM pamRules
  absCfg <- abstractAmCfg (irrelevance ValueIrr) (irrelevance ValueIrr) amRules (toGeneric x :: Term MITScript)
  return absCfg

graphToString graph = unpack $ renderDot $ toDot $ graphToDot (nonClusteredParams { fmtNode = \(n,l) -> [toLabel "", shape BoxShape], fmtEdge = \(n1,n2,l) -> [toLabel "", shape BoxShape]}) (toRealGraph @Gr graph)
