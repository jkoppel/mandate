module Languages.MITScript.Parse (
    parseFile
  ) where

import System.Process ( readProcess )

import Languages.MITScript.Syntax

parseFile :: FilePath -> IO Stmt
parseFile path = do
  fileStr <- readFile path
  treeStr <- readProcess "./MITScript/a.out" [] fileStr
  return $ read treeStr