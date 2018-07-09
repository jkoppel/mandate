{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List

import Lang
import Semantics.SOS

import Languages.Add
import Languages.Lockstep
import Languages.MITScript.Syntax
import Languages.Imp

main :: IO ()
main = print =<< rules @ImpLang
