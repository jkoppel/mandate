{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List

import Lang
import Semantics

import Languages.Add
import Languages.Imp

main :: IO ()
main = print =<< rules @ImpLang
