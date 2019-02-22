{

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- But alex still generates some code that causes the "lazy unlifted bindings"
-- warning, and old compilers don't know about it so we can't easily turn
-- it off, so for now we use the sledge hammer:
{-# OPTIONS_GHC -w #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- Lexer module for Tiger
-- Part of this source file are drawn from the reference here:
-- https://github.com/simonmar/alex/blob/master/examples/tiger.x
-- Hengchu Zhang, 2015
module TigerLexer
  (
    Token(..)
  , TokenClass(..)
  , AlexPosn(..)
  , scanner
  ) where

import Prelude hiding (GT, LT, EQ)
import Numeric (readDec, readOct, readHex)
import Data.Char (chr)
import Control.Monad
import Data.Maybe (isJust, fromJust)
}

%wrapper "monadUserState"

$digit      = 0-9
$alpha      = [a-zA-Z]
$whitespace = [\ \t]
$eol        = [\n\r]

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*

rules:-

<0>$whitespace           ;
<0>$eol                  { skip }

<0>"array"     { simpleToken ARRAY     }
<0>"if"        { simpleToken IF        }
<0>"then"      { simpleToken THEN      }
<0>"else"      { simpleToken ELSE      }
<0>"while"     { simpleToken WHILE     }
<0>"for"       { simpleToken FOR       }
<0>"to"        { simpleToken TO        }
<0>"do"        { simpleToken DO        }
<0>"let"       { simpleToken LET       }
<0>"in"        { simpleToken IN        }
<0>"end"       { simpleToken END       }
<0>"of"        { simpleToken OF        }
<0>"break"     { simpleToken BREAK     }
<0>"nil"       { simpleToken NIL       }
<0>"function"  { simpleToken FUNCTION  }
<0>"var"       { simpleToken VAR       }
<0>"type"      { simpleToken TYPE      }
<0>"import"    { simpleToken IMPORT    }
<0>"primitive" { simpleToken PRIMITIVE }

<0>","  { simpleToken COMMA     }
<0>":"  { simpleToken COLON     }
<0>";"  { simpleToken SEMICOLON }
<0>"("  { simpleToken LPAREN    }
<0>")"  { simpleToken RPAREN    }
<0>"["  { simpleToken LBRAK     }
<0>"]"  { simpleToken RBRAK     }
<0>"{"  { simpleToken LBRAC     }
<0>"}"  { simpleToken RBRAC     }
<0>"."  { simpleToken DOT       }
<0>"+"  { simpleToken PLUS      }
<0>"-"  { simpleToken MINUS     }
<0>"*"  { simpleToken MULT      }
<0>"/"  { simpleToken DIV       }
<0>"="  { simpleToken EQ        }
<0>"<>" { simpleToken NEQ       }
<0>"<"  { simpleToken LT        }
<0>"<=" { simpleToken LEQ       }
<0>">"  { simpleToken GT        }
<0>">=" { simpleToken GEQ       }
<0>"&"  { simpleToken AMPERSAND }
<0>"|"  { simpleToken BAR       }
<0>":=" { simpleToken ASSIGN    }

<0>@identifier { getVariable }
<0>@number     { getInteger  }

<0>\" { enterNewString `andBegin` state_string }
<state_string> \\n                  { addCharToLexerString '\n' }
<state_string> \\t                  { addCharToLexerString '\t' }
<state_string> \\a                  { addCharToLexerString '\a' }
<state_string> \\b                  { addCharToLexerString '\b' }
<state_string> \\f                  { addCharToLexerString '\f' }
<state_string> \\r                  { addCharToLexerString '\r' }
<state_string> \\v                  { addCharToLexerString '\v' }
<state_string> \\$digit$digit$digit { addOctalToLexerString     }
<state_string> \\\"                 { addCharToLexerString '"'  }
<state_string> \\\\                 { addCharToLexerString '\\' }
<state_string> \\[\ \n\t\f\r\b\v]\\ ;
<state_string> \\                   { \_ _ -> lexerError "Illegal escape sequence" }
<state_string> \"                   { leaveString `andBegin` state_initial }
<state_string> .                    { addCurrentCharToString }
<state_string> \n                   { \_ _ -> lexerError "Illegal newline in string" }

<0>"/*" { enterNewComment `andBegin` state_comment }
<state_comment>"/*"   { embedComment   }
<state_comment>"*/"   { unembedComment }
<state_comment>.      ;
<state_comment>\n     { skip           }

{

data Token = Token AlexPosn TokenClass (Maybe String)
             deriving (Show, Eq)

data TokenClass = ARRAY
                | IF
                | THEN
                | ELSE
                | WHILE
                | FOR
                | TO
                | DO
                | LET
                | IN
                | END
                | OF
                | BREAK
                | NIL
                | FUNCTION
                | VAR
                | TYPE
                | IMPORT
                | PRIMITIVE
                | Id          String
                | Number      Int
                | Str         String
                | COMMA
                | COLON
                | SEMICOLON
                | LPAREN
                | RPAREN
                | LBRAK
                | RBRAK
                | LBRAC
                | RBRAC
                | DOT
                | PLUS
                | MINUS
                | MULT
                | DIV
                | EQ
                | NEQ
                | LT
                | LEQ
                | GT
                | GEQ
                | AMPERSAND
                | BAR
                | ASSIGN
                | EOF
             deriving (Show, Eq)

-- Token action should have type
-- AlexInput -> Int -> Alex Token

state_initial :: Int
state_initial = 0

data AlexUserState = AlexUserState
                     {
                       lexerCommentDepth :: Int
                     , lexerInString     :: Bool
                     , lexerStringValue  :: String
                     }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                    {
                      lexerCommentDepth = 0
                    , lexerInString     = False
                    , lexerStringValue  = ""
                    }

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return (Token p EOF Nothing)

getLexerCommentDepth :: Alex Int
getLexerCommentDepth =
  Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss =
  Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

getLexerStringValue :: Alex String
getLexerStringValue =
  Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue str =
  Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=str}}, ())

addCharToLexerString :: Char -> AlexInput -> Int -> Alex Token
addCharToLexerString c _ _ = do s <- getLexerStringValue
                                setLexerStringValue (c:s)
                                alexMonadScan

getLexerInString :: Alex Bool
getLexerInString =
  Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerInString ust)

setLexerInString :: Bool -> Alex ()
setLexerInString b =
  Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerInString=b}}, ())

getVariable :: AlexInput -> Int -> Alex Token
getVariable (p, _, _, str) len = return (Token p (Id s) (Just s))
  where s = take len str

getInteger :: AlexInput -> Int -> Alex Token
getInteger (p, _, _, str) len = return (Token p (Number d) (Just s))
  where s = take len str
        d = fst $ head $ readDec s

enterNewString :: AlexInput -> Int -> Alex Token
enterNewString _ _ = do
  setLexerInString True
  setLexerStringValue ""
  alexMonadScan

enterNewComment :: AlexInput -> Int -> Alex Token
enterNewComment _ _ = do
  setLexerCommentDepth 1
  alexMonadScan

embedComment _ _ = do
  cd <- getLexerCommentDepth
  setLexerCommentDepth (cd+1)
  alexMonadScan

unembedComment _ _ = do
  cd <- getLexerCommentDepth
  setLexerCommentDepth (cd-1)
  when (cd==1) (alexSetStartCode state_initial)
  alexMonadScan

addOctalToLexerString :: AlexInput -> Int -> Alex Token
addOctalToLexerString i@(_, _, _, input) len = if (v < 256)
                                                 then addCharToLexerString c i len
                                                 else lexerError ("Invalid ascii value : " ++ input)
  where
    s = if (len == 4)
          then drop 1 input
          else error "Invalid call to 'addOctalToLexerString'"
    r = readOct s
    v = if (length r == 1)
          then fst $ head r
          else error "Invalid call to 'addOctalToLexerString'"
    c = chr v
            

addCurrentCharToString :: AlexInput -> Int -> Alex Token
addCurrentCharToString i@(p, _, _, str) len = addCharToLexerString c i len
  where
    c = if (len == 1)
          then head str
          else error "Invalid call to 'addCurrentCharToString'"

leaveString (p, _, _, str) len = do
  s <- getLexerStringValue
  setLexerInString False
  setLexerStringValue ""
  return (Token p (Str $ reverse s) (Just (take len str)))

simpleToken :: TokenClass -> AlexInput -> Int -> Alex Token
simpleToken t (p, _, _, str) len = return (Token p t (Just (take len str)))

scannerComplementError :: Alex a -> Alex (a, Maybe String)
scannerComplementError (Alex al) = Alex (\s -> case al s of
                                                 Right (s', x) -> Right (s', (x, Nothing))
                                                 Left  msg  -> Right (s, (undefined, Just msg)))

scanner :: String -> Either String [Token]
scanner str = let loop = do (t, m) <- scannerComplementError alexMonadScan
                            when (isJust m) (lexerError (fromJust m))
                            let tok@(Token _ cl _) = t
                            if (cl == EOF)
                              then do f1 <- getLexerInString
                                      f2 <- getLexerCommentDepth
                                      if ((not f1) && (f2 == 0))
                                        then return [tok]
                                        else if (f1)
                                             then alexError "Open string at the end of file"
                                             else alexError "Comment not closed at the end of file"
                              else do toks <- loop
                                      return (tok:toks)
              in runAlex str loop

lexerError :: String -> Alex a
lexerError msg =
  do (p, c, _, inp) <- alexGetInput
     let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
     let inp2 = if (length inp1 > 30)
                   then trim (take 30 inp1)
                   else trim inp1
     let disp = if (null inp)
                   then " at end of file"
                   else if (null inp2)
                           then " before end of file"
                           else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
     let disp3 = if (null msg)
                    then "Lexer error"
                    else trim msg
     alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    -- removes space at head and tail
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col
}
