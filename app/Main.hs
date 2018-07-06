module Main where

import Data.List

import Languages.Add
import Languages.Imp

main :: IO ()
main = print =<< impLangRules
