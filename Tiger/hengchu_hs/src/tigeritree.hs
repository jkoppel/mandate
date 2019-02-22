module TigerITree
       ( Stm(..)
       , Exp(..)
       , Test(..)
       , Binop(..)
       , Relop(..)
       , Cvtop(..)
       , isExpPtr
       , IsPtr
       , prettyprintstm
       )
       where

import qualified TigerTemp as Temp
import Control.Monad.State

type Size = Int

isExpPtr :: Exp -> IsPtr
isExpPtr (BINOP _ isptr)   = isptr
isExpPtr (CVTOP _)         = False
isExpPtr (MEM _ isptr)     = isptr
isExpPtr (TEMP _ isptr)    = isptr
isExpPtr (ESEQ (_, expr)) = isExpPtr expr
isExpPtr (NAME _)          = False
isExpPtr (CONST _ isptr)   = isptr
isExpPtr (CONSTF _)        = False
isExpPtr (CALL _ isptr _)  = isptr

data Stm = SEQ   (Stm, Stm)
         | LABEL (Temp.Label)
         | JUMP  (Exp, [Temp.Label])
         | CJUMP (Test, Temp.Label, Temp.Label)
         | MOVE  (Exp, Exp)
         | EXP   Exp
         deriving (Show, Eq)

type IsPtr = Bool

data Exp = BINOP  (Binop, Exp, Exp) IsPtr
         | CVTOP  (Cvtop, Exp, Size, Size)
         | MEM    (Exp, Size) IsPtr
         | TEMP   Temp.Temp IsPtr
         | ESEQ   (Stm, Exp)
         | NAME   Temp.Label
         | CONST  Int IsPtr
         | CONSTF Float
         | CALL   (Exp, [Exp]) IsPtr Temp.RetLabel
         deriving (Show, Eq)

data Test = TEST (Relop, Exp, Exp)
         deriving (Show, Eq)

data Binop = FPLUS | FMINUS | FDIV | FMUL
           | PLUS  | MINUS  | MUL  | DIV
           | AND   | OR     | LSHIFT | RSHIFT | ARSHIFT | XOR
         deriving (Show, Eq)

data Relop = EQ | NE | LT | GT | LE | GE
           | ULT | ULE | UGT | UGE
           | FEQ | FNE | FLT | FLE | FGT | FGE
         deriving (Show, Eq)

data Cvtop = CVTSU | CVTSS | CVTSF | CVTUU
           | CVTUS | CVTFS | CVTFF
         deriving (Show, Eq)

-- Pretty printer
prettyprintstm :: Stm -> String
prettyprintstm s = execState (prettyprintstm' s) ""

prettyprintstm' :: Stm -> State String () 
prettyprintstm' s =
  let
    say stuff = do out <- get
                   put $ out ++ stuff
    sayln stuff = say stuff >> say "\n"

    indent :: Int -> State String ()
    indent 0 = return ()
    indent i = say " " >> indent (i-1)

    stm :: Stm -> Int -> State String ()
    stm (SEQ(a, b)) d = indent d >> sayln "SEQ(" >> stm a (d+1)
                                 >> sayln " " >> stm b (d+1) >> say ")"
    stm (LABEL lab) d = if d == 0
                           then say (fst lab ++ ":")
                           else indent d >> say (fst lab)
    stm (JUMP(e, _)) d = indent d >> sayln "JUMP(" >> expr e (d+1) >> say ")"
    stm (CJUMP(TEST(r, a, b), t, f)) d = do indent d
                                            sayln "CJUMP("
                                            indent $ d+1
                                            say $ show r
                                            sayln ","
                                            expr a $ d+1
                                            sayln ","
                                            expr b $ d+1
                                            sayln ","
                                            indent $ d+1
                                            say $ fst t
                                            say ","
                                            say $ fst f
                                            say ")"
    stm (MOVE(a, b)) d = indent d >> sayln "MOVE(" >> expr a (d+1)
                                  >> sayln "," >> expr b (d+1)
                                  >> say ")"
    stm (EXP e) d = indent d >> sayln "EXP(" >> expr e (d+1) >> say ")"
                                                 

    expr :: Exp -> Int -> State String ()
    expr (BINOP(p, a, b) _) d = indent d >> say "BINOP(" >> say (show p) >> sayln ","
                                      >> expr a (d+1) >> sayln "," >> expr b (d+1)
                                      >> say ")"
    expr (CVTOP(p, e, s1, s2)) d = indent d >> say "CVTOP[" >> say (show s1)
                                           >> say "," >> say (show s2) >> say "]("
                                           >> say (show p) >> sayln ","
                                           >> expr e (d+1) >> say ")"
    expr (MEM(e, stmt) _) d = indent d >> say "MEM[" >> say (show stmt)
                                 >> sayln "](" >> expr e (d+1) >> say ")"
    expr (TEMP t _) d = indent d >> say "TEMP " >> say (show t)
    expr (ESEQ(stmt, e)) d = indent d >> sayln "ESEQ(" >> stm stmt (d+1)
                                  >> sayln "," >> expr e (d+1) >> say ")"
    expr (NAME lab) d = indent d >> say "NAME " >> say (fst lab)
    expr (CONST i _) d = indent d >> say "CONST " >> say (show i)
    expr (CONSTF r) d = indent d >> say "CONSTF" >> say (show r)
    expr (CALL(e, el) _ _) d = indent d >> sayln "CALL(" >> expr e (d+1)
                                   >> mapM_ (\a -> sayln "," >> expr a (d+2)) el
                                   >> say ")"

  in
    stm s 0

