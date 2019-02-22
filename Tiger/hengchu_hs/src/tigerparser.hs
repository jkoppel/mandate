{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TigerParser
  (
    token2ptoken
  , parser
  , Position(..)
  , TigerParser.runParser
  )
  where

import TigerLexer
import TigerAbsyn
import Prelude hiding (EQ, LT, GT)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Expr
import TigerSymbol
import qualified TigerGenSymLabTmp as TGSLT
import Control.Applicative ((<|>), (*>))
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Trans.Class

class Position a where
  extractPosition :: a -> AlexPosn

instance Position PToken where
  extractPosition (PToken pos _) = pos

instance Position Var where
  extractPosition (SimpleVar(_, pos))       = pos
  extractPosition (FieldVar(_, _, pos))     = pos
  extractPosition (SubscriptVar(_, _, pos)) = pos

instance Position Exp where
  extractPosition (VarExp v) = extractPosition v
  extractPosition (NilExp pos) = pos
  extractPosition (IntExp (_, pos)) = pos
  extractPosition (StringExp (_, pos)) = pos
  extractPosition (SeqExp []) = error "Can't access position of empty sequence"
  extractPosition (SeqExp ((_, pos):_)) = pos
  extractPosition (AppExp {appPos=pos}) = pos
  extractPosition (OpExp {opPos=pos}) = pos
  extractPosition (RecordExp {recordPos=pos}) = pos
  extractPosition (AssignExp {assignPos=pos}) = pos
  extractPosition (IfExp {ifPos=pos}) = pos
  extractPosition (WhileExp {whilePos=pos}) = pos
  extractPosition (ForExp {forPos=pos}) = pos
  extractPosition (BreakExp {breakPos=pos}) = pos
  extractPosition (LetExp {letPos=pos}) = pos
  extractPosition (ArrayExp {arrayPos=pos}) = pos

instance Position Ty where
  extractPosition (NameTy (_, pos)) = pos
  extractPosition (RecordTy []) = error "Can't access position of empty record types"
  extractPosition (RecordTy (t:_)) = extractPosition t
  extractPosition (ArrayTy (_, pos)) = pos

instance Position Tfield where
  extractPosition (Tfield{tfieldPos=pos}) = pos

instance Position Typedec where
  extractPosition (Typedec{typedecPos=pos}) = pos

instance Position Fundec where
  extractPosition (Fundec{fundecPos=pos}) = pos

instance Position Dec where
  extractPosition (FunctionDec []) = error "Can't access position of empty function decs"
  extractPosition (FunctionDec (f:_)) = extractPosition f
  extractPosition (VarDec{varDecPos=pos}) = pos
  extractPosition (TypeDec []) = error "Can't access position of empty type decs"
  extractPosition (TypeDec (t:_)) = extractPosition t
  

-- helper function for generating symbols
symbol = lift . TGSLT.symbol

-- Simplified token type for the parser.
data PToken = PToken AlexPosn TokenClass
  deriving(Show, Eq)

type Parser = ParsecT [PToken] () (TGSLT.GenSymLabTmp Identity)

token2ptoken :: Token -> PToken
token2ptoken (Token pos tc _ ) = PToken pos tc

updatePos :: SourcePos -> PToken -> [PToken] -> SourcePos
updatePos pos (PToken (AlexPn _ line col) _) _ = 
  setSourceLine (setSourceColumn pos col) line

parseSimpleToken :: TokenClass -> Parser PToken
parseSimpleToken tc = tokenPrim show updatePos acceptTok
  where acceptTok t@(PToken _ c) | tc == c = Just t
                                 | otherwise = Nothing

parseId :: Parser (PToken, String)
parseId = tokenPrim show updatePos acceptTok
  where acceptTok idtok@(PToken _ (Id name)) = Just (idtok, name)
        acceptTok _                       = Nothing

parseNumber :: Parser (PToken, Int)
parseNumber = tokenPrim show updatePos acceptTok
  where acceptTok num@(PToken _ (Number val)) = Just (num, val)
        acceptTok _                           = Nothing

parseString :: Parser (PToken, String)
parseString = tokenPrim show updatePos acceptTok
  where acceptTok str@(PToken _ (Str s)) = Just (str, s)
        acceptTok _                      = Nothing

negateOp pos a = OpExp { opLeft = IntExp (0, pos)
                       , opOper = MinusOp
                       , opRight = a
                       , opPos = pos }

binaryOp op pos a b = OpExp { opLeft = a
                            , opOper = op
                            , opRight = b
                            , opPos = pos }

prefix parseOp fun  = Prefix (try $ do {p <- parseOp; return $ fun $ extractPosition p})
--postfix parseOp fun = Postfix (try $ do {p <- parseOp; return $ fun $ extractPosition p})
binary parseOp fun  = Infix (try $ do {p <- parseOp; return $ fun $ extractPosition p})

expr = buildExpressionParser table pExpTerm

table = [
          [ prefix (parseSimpleToken MINUS)  negateOp ]
        , [ binary (parseSimpleToken MULT)  (binaryOp TimesOp) AssocLeft
          , binary (parseSimpleToken DIV)   (binaryOp DivideOp) AssocLeft ]
        , [ binary (parseSimpleToken PLUS)  (binaryOp PlusOp) AssocLeft
          , binary (parseSimpleToken MINUS) (binaryOp MinusOp) AssocLeft ]
        , [ binary (parseSimpleToken GEQ)   (binaryOp GeOp) AssocNone
          , binary (parseSimpleToken LEQ)   (binaryOp LeOp) AssocNone
          , binary (parseSimpleToken EQ)    (binaryOp EqOp) AssocNone
          , binary (parseSimpleToken NEQ)   (binaryOp NeqOp) AssocNone
          , binary (parseSimpleToken LT)    (binaryOp LtOp) AssocNone
          , binary (parseSimpleToken GT)    (binaryOp GtOp) AssocNone ]
        , [ binary (parseSimpleToken AMPERSAND) (binaryOp AndOp) AssocLeft ]
        , [ binary (parseSimpleToken BAR)   (binaryOp OrOp) AssocLeft ]
        ]

pExpTerm =  try pSeqExp
        <|> try pLet
        <|> try pArrayCreation
        <|> try pRecordCreation
        <|> try pAssignment
        <|> try pFunctionCall
        <|> try pLValue
        <|> try pIf
        <|> try pWhile
        <|> try pBreak
        <|> try pFor
        <|> try pStr
        <|> try pNum
        <|> try pNil
        <|> between (parseSimpleToken LPAREN) (parseSimpleToken RPAREN) expr

pNum = do (p, num) <- parseNumber
          return $ IntExp (num, extractPosition p)

pNil = do p <- parseSimpleToken NIL
          return $ NilExp $ extractPosition p

pStr = do (p, str) <- parseString
          return $ StringExp (str, extractPosition p)

pArrayCreation =
  do (p, typeid) <- parseId
     szexp <- between (parseSimpleToken LBRAK) (parseSimpleToken RBRAK) expr
     parseSimpleToken OF
     initexp <- expr
     typeidsymbol <- symbol typeid
     return $ ArrayExp { arrayTyp = typeidsymbol
                       , arraySize = szexp
                       , arrayInit = initexp
                       , arrayPos = extractPosition p }

pRecordCreation = 
  do (p, typeid) <- parseId
     efields <- between (parseSimpleToken LBRAC) (parseSimpleToken RBRAC) pEfields
     typeidsym <- symbol typeid
     return $ RecordExp { recordTyp = typeidsym
                        , recordFields = efields
                        , recordPos = extractPosition p
                        }
  where pEfields = pEfield `sepBy1` (parseSimpleToken COMMA)
        pEfield  = do (p, fieldid) <- parseId
                      parseSimpleToken EQ
                      e <- expr
                      fieldidsym <- symbol fieldid
                      return (fieldidsym, e, extractPosition p)

data LValueTail = Dot Symbol AlexPosn
                | Bracket Exp AlexPosn

pLValue = liftM VarExp pLValue1

pLValue1 =
  do (p, varid) <- parseId
     maybetail  <- optionMaybe $ pLValue'
     varidsym <- symbol varid
     let simplevar = SimpleVar (varidsym, extractPosition p)
     return $ case maybetail of
                Nothing   -> simplevar
                Just ltail -> attachTail simplevar ltail
  where pLValue' = try option1 <|> try option2 <|> try pDotId <|> pBracketExp
        pDotId = do dottok <- parseSimpleToken DOT
                    (_, fieldid) <- parseId
                    fieldidsym <- symbol fieldid
                    return [Dot fieldidsym (extractPosition dottok)]
        pBracketExp = do lbraktok <- parseSimpleToken LBRAK
                         e <- expr
                         parseSimpleToken RBRAK
                         return [Bracket e $ extractPosition lbraktok]
        option1 = do dotid <- pDotId
                     rest <- pLValue'
                     return $ dotid ++ rest
        option2 = do brackete <- pBracketExp
                     rest <- pLValue'
                     return $ brackete ++ rest
        attachTail :: Var -> [LValueTail] -> Var
        attachTail var [] = var
        attachTail var (t:ts) =
          case t of
            Dot fieldid pos -> attachTail (FieldVar (var, fieldid, pos)) ts
            Bracket e pos   -> attachTail (SubscriptVar (var, e, pos)) ts

pFunctionCall =
  do (p, funcid) <- parseId
     args <- between (parseSimpleToken LPAREN) (parseSimpleToken RPAREN)
                     (expr `sepBy` (parseSimpleToken COMMA))
     funcidsym <- symbol funcid
     return $ AppExp { appFunc = funcidsym
                     , appArgs = args
                     , appPos = extractPosition p }

pAssignment = 
  do var <- pLValue1
     assigntok <- parseSimpleToken ASSIGN
     e <- expr
     return $ AssignExp { assignVar = var
                        , assignExp = e
                        , assignPos = extractPosition assigntok
                        }

pIf = 
  do iftok <- parseSimpleToken IF
     teste <- expr
     parseSimpleToken THEN
     thene <- expr
     maybeelse <- optionMaybe pElse
     let ifexp = IfExp { ifTest = teste
                       , ifThen = thene
                       , ifElse = Nothing
                       , ifPos = extractPosition iftok }
     return $ case maybeelse of
                Just elsee -> ifexp{ifElse=Just elsee}
                Nothing -> ifexp
  where pElse = do parseSimpleToken ELSE
                   expr

pWhile =
  do whiletok <- parseSimpleToken WHILE
     teste <- expr
     parseSimpleToken DO
     bodye <- expr
     return $ WhileExp { whileTest=teste
                       , whileBody=bodye
                       , whilePos=extractPosition whiletok
                       }

pFor =
  do fortok <- parseSimpleToken FOR
     (_, iterid) <- parseId
     parseSimpleToken ASSIGN
     iterlowexp <- expr
     parseSimpleToken TO
     iterhighexp <- expr
     parseSimpleToken DO
     bodye <- expr
     iteridsym <- symbol iterid
     return $ ForExp { forVar = Vardec { vardecName=iteridsym, vardecEscape=False }
                     , forLo = iterlowexp
                     , forHi = iterhighexp
                     , forBody = bodye
                     , forPos = extractPosition fortok }

pBreak =
  do breaktok <- parseSimpleToken BREAK
     return BreakExp { breakPos = extractPosition breaktok }

pSeqExp =
  do es <- between (parseSimpleToken LPAREN) (parseSimpleToken RPAREN) $ expr `sepBy` (parseSimpleToken SEMICOLON)
     return $ SeqExp $ zip es $ map extractPosition es

pVardec = 
  do vartok <- parseSimpleToken VAR
     (_, varid) <- parseId
     maybetypeid <- optionMaybe (parseSimpleToken COLON *> parseId)
     parseSimpleToken ASSIGN
     e <- expr
     varidsym <- symbol varid
     let vardec = Vardec { vardecName = varidsym
                         , vardecEscape = False }
     let vardec2 = VarDec { varDecVar = vardec
                          , varDecTyp = Nothing
                          , varDecInit = e
                          , varDecPos = extractPosition vartok }
     case maybetypeid of
       Nothing -> return vardec2
       Just (p, typeid) -> do typeidsym <- symbol typeid
                              return vardec2{varDecTyp=Just(typeidsym,
                                                            extractPosition p)}

pLet =
  do lettok <- parseSimpleToken LET
     decls <- pDecs
     parseSimpleToken IN
     es <- expr `sepBy` (parseSimpleToken SEMICOLON)
     let poses = map extractPosition es
     parseSimpleToken END
     return $ LetExp { letDecs=decls
                     , letBody= SeqExp $ zip es poses
                     , letPos=extractPosition lettok }

pDecs = many pDec

pDec =  try pVardec
    <|> try pFunDecs
    <|> pTypeDecs

pFunDecs = liftM FunctionDec $ many1 pFunDec

pFunDec =
  do functok <- parseSimpleToken FUNCTION
     (_, funcid) <- parseId
     tfields <- between (parseSimpleToken LPAREN) (parseSimpleToken RPAREN) pTyfields
     maybetypeid <- optionMaybe (parseSimpleToken COLON *> parseId)
     parseSimpleToken EQ
     bodye <- expr
     funcidsym <- symbol funcid
     let fundec = Fundec { fundecName = funcidsym
                         , fundecParams = tfields
                         , fundecResult = Nothing
                         , fundecBody = bodye
                         , fundecPos = extractPosition functok }
     case maybetypeid of
       Nothing -> return fundec
       Just (p, typeid) -> do typeidsym <- symbol typeid
                              return $ fundec{fundecResult=Just(typeidsym, extractPosition p)}

pTypeDecs = liftM TypeDec (many1 pTypeDec)

pTypeDec =
  do typetok <- parseSimpleToken TYPE
     (_, typeid) <- parseId
     parseSimpleToken EQ
     -- (_, tyid) <- parseId
     typeidsym <- symbol typeid
     ty <- pTy
     return Typedec { typedecName=typeidsym
                    , typedecTy=ty
                    , typedecPos=extractPosition typetok }

pTyfields = pTyfield `sepBy` (parseSimpleToken COMMA)

pTyfield =
  do (p, nameid) <- parseId
     parseSimpleToken COLON
     (_, typeid) <- parseId
     nameidsym <- symbol nameid
     typeidsym <- symbol typeid
     return $ Tfield { tfieldName = nameidsym
                     , tfieldTyp = typeidsym
                     , tfieldPos = extractPosition p }

pTy = try pRecordDef <|> try pArrayDec <|> pTypeIdDef
  where pRecordDef = do _ <- parseSimpleToken LBRAC
                        tfields <- pTyfields
                        parseSimpleToken RBRAC
                        return $ RecordTy tfields
        pArrayDec = do arraytok <- parseSimpleToken ARRAY
                       parseSimpleToken OF
                       (_, typeid) <- parseId
                       typeidsym <- symbol typeid
                       return $ ArrayTy (typeidsym, extractPosition arraytok)
        pTypeIdDef = do (p, typeid) <- parseId
                        typeidsym <- symbol typeid
                        return $ NameTy (typeidsym, extractPosition p)

expr2 = do e <- expr
           parseSimpleToken EOF
           return $ Pexp e

decs = do ds <- pDecs
          parseSimpleToken EOF
          return $ Pdecs ds

parser = try expr2 <|> decs

runParser :: Parser a -> String 
                      -> [PToken] 
                      -> Either ParseError (a, TGSLT.GenSymLabTmpState)
runParser p filename ptokens =
  let tgsltmonad = runPT p () filename ptokens
      identitymonad = TGSLT.runGSLT TGSLT.initialGSLTState tgsltmonad
      (result, gsltstate) = runIdentity identitymonad
  in case result of
       Left err -> Left err
       Right a  -> Right (a, gsltstate)
