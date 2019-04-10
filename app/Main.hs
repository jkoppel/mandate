{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment
import System.Exit

import Control.DeepSeq
import Configuration
-- import Data.GraphViz
import Data.Graph.Inductive.Example
-- import Data.GraphViz.Printing
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
  validate args (length args) >>= putStrLn

usage = "Usage: derive-cfg [language name] [path to source]"

validate :: [String] -> Int -> IO String
validate args 2 = makeGraph (head args) (last args)
validate _ _ = return usage

makeGraph :: String -> String -> IO String
makeGraph lang fs = case lang of
  "mitscript" -> return (show (parse fs)) 
  _           -> return "Unsupported language"

parse fs = do
  x <- MITParse.parseFile fs
  mitScriptRules <- (rules :: IO (NamedRules MITScript))
  pamRules <- sosToPam mitScriptRules
  amRules <- pamToAM pamRules
  absCfg <- abstractAmCfg (irrelevance ValueIrr) (irrelevance ValueIrr) amRules (toGeneric x :: Term MITScript)
  return (toRealGraph absCfg)

{- makeCfg term = do
  mitrules <- rules term
  pamRules <- sosToPam mitrules
  amRules <- pamToAM pamRules
  toRealGraph $ abstractAmCfg (irrelevance term) (irrelevance term) amRules term
-} 
