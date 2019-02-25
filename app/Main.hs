{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List

import Lang
import Semantics.SOS

import Languages.Translation

import Languages.AddMul

import Languages.Lockstep
--import Languages.MITScript.Parse
import Languages.MITScript.Semantics
import Languages.MITScript.Signature
import Languages.MITScript.Syntax
import Languages.MITScript.Translate

import Languages.Imp

import Languages.Tiger.Parse
import Languages.Tiger.Semantics
import Languages.Tiger.Signature
import Languages.Tiger.Translate

main :: IO ()
main = print =<< rules @ImpLang
