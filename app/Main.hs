module Main where

import Data.List

import Semantics

import Languages.Add
import Languages.Imp

main :: IO ()
main = print =<< impLangRules
