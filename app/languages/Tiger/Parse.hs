module Languages.Tiger.Parse (
    parseFile
  ) where

import TigerAbsyn
import qualified TigerLexer as TL
import qualified TigerParser as TP

parseFile :: FilePath -> IO Program
parseFile path = do
  fileStr <- readFile path
  case TL.scanner fileStr of
    Left e     -> fail ("Lexer error: " ++ show e)
    Right toks -> do let ptoks = map TP.token2ptoken toks
                     case TP.runParser TP.parser path ptoks of
                       Left e       -> fail ("Parse error: " ++ show e)
                       Right (p, _) -> return p