module Languages.Tiger.Parse (
    emptyPosn
  , toDumbSymbol
  , parseFile
  ) where

import Data.Generics (everywhere, mkT)

import TigerAbsyn
import TigerSymbol
import qualified TigerLexer as TL
import qualified TigerParser as TP

-----------------------------------------------------------------------------

emptyPosn :: TL.AlexPosn
emptyPosn = TL.AlexPn 0 0 0

toDumbSymbol :: String -> Symbol
toDumbSymbol s = (s, 0)

stripExtraInf :: Program -> Program
stripExtraInf = everywhere (mkT stripPosInf . mkT stripVarEscape . mkT stripSymId . mkT normalizeElse)
  where
    stripPosInf :: TL.AlexPosn -> TL.AlexPosn
    stripPosInf _ = emptyPosn

    stripVarEscape :: Bool -> Bool
    stripVarEscape _ = False

    stripSymId :: Symbol -> Symbol
    stripSymId (s, _) = toDumbSymbol s

    --TODO: I'm not sure that I actually want this
    normalizeElse :: Exp -> Exp
    normalizeElse (IfExp e s1 Nothing p) = IfExp e s1 (Just (NilExp p)) p
    normalizeElse x = x

parseFile :: FilePath -> IO Program
parseFile path = do
  fileStr <- readFile path
  case TL.scanner fileStr of
    Left e     -> fail ("Lexer error: " ++ show e)
    Right toks -> do let ptoks = map TP.token2ptoken toks
                     case TP.runParser TP.parser path ptoks of
                       Left e       -> fail ("Parse error: " ++ show e)
                       Right (p, _) -> return $ stripExtraInf p