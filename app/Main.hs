{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment
import System.Exit

import Control.DeepSeq
import Configuration
-- import Data.GraphViz
-- import Data.Graph.Inductive.Example
-- import Data.GraphViz.Printing
-- import Data.Graph.Inductive.PatriciaTree
import Graph
import Rose
import Semantics.AbstractMachine
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
  validate args (length args)

usage = "Usage: derive-cfg [language name] [path to source]"

validate :: [String] -> Int -> IO ()
validate args 2 = makeGraph (head args) (last args)
validate _ _ = return usage

makeGraph :: String -> String -> IO ()
makeGraph lang fs = case lang of
  "mitscript" -> makeCfg (parse fs)
  _           -> return "Unsupported language"

parse fs = toGeneric (MITParse.parseFile fs) :: Term MITScript
makeCfg term = do
  mitrules <- rules term
  pamRules <- sosToPam mitrules
  amRules <- pamToAM pamRules
  abstractAmCfg (irrelevance term) (irrelevance term) amRules term
