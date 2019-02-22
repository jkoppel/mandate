module TigerTranslate
  (
    Level
  , Access
  , Frag
  , Gexp
  , newLevel
  , outerMost
  , allocInFrame
  , eqStr
  , notEqStr
  , strLessThan
  , strLessThanOrEq
  , eqCmp
  , notEqCmp
  , lessThan
  , lessThanOrEq
  , arithmetic
  , intExp
  , stringExp
  , constructEseq
  , letExpression
  , assign
  , createRecord
  , createArray
  , field
  , subscript
  , simpleVar
  , ifThen
  , ifThenElse
  , whileLoop
  , forLoop
  , TigerTranslate.break
  , callFunction
  , createProcFrag
  , createMainFrag
  , reset
  , getResult
  , nilGexp
  ) where

import TigerITree
import TigerSemTr
import qualified TigerTemp as Tmp
import qualified TigerFrame as Frame
import qualified TigerAbsyn as Absyn
import qualified TigerRegisters as Reg
import Control.Monad.State
import Data.Bits
import Data.IORef
import Prelude hiding (EQ, LT, GT)

data Gexp = Ex Exp
          | Nx Stm
          | Cx (Tmp.Label -> Tmp.Label -> Stm)
             -- True label   False label

outerMost :: Level
outerMost = TOP

nilGexp :: Gexp
nilGexp = Ex $ CONST 0 False

newLevel :: Level -> [a] -> SemTr (Level, [(a, Access)])
newLevel parent formals =
  do let numformals = length formals
     (frame, (slOffset:offsets)) <- liftIO $ Frame.newFrame (numformals+1)
     lvlUniq <- genUniq
     let lvl = LEVEL { levelFrame=frame
                     , staticLinkOffset=slOffset + Reg.parambaseoffset
                     , levelParent=parent
                     , levelUniq=lvlUniq }
     let offsets' = map (\off -> (lvl, off+Reg.parambaseoffset)) offsets
     let formalsAndOffsets = zip formals offsets'
     return (lvl, formalsAndOffsets)

allocInFrame :: Bool -> Level -> IO Access
allocInFrame isptr lvl@(LEVEL { levelFrame=lvlframe }) =
  do offset <- Frame.allocLocalInFrame isptr lvlframe
     return (lvl, offset + Reg.localbaseoffset) 
allocInFrame _ TOP = error "Compiler error: cannot alloc local in TOP Level frame"

seqcon :: [Stm] -> Stm
seqcon (x:[]) = x
seqcon (x:xs) = SEQ(x, seqcon xs)
seqcon []     = error "Compiler error: Impossible usage of seqcon"

unEx :: Gexp -> SemTr (Exp)
unEx (Ex e)      = return e
unEx (Cx genstm) = do let isrptr = False
                      r <- newTemp isrptr
                      t <- newLabel
                      f <- newLabel
                      return $ ESEQ(seqcon
                                       [ MOVE(TEMP r isrptr, CONST 1 False)
                                       , genstm t f
                                       , LABEL f
                                       , MOVE(TEMP r isrptr, CONST 0 False)
                                       , LABEL t
                                       ], TEMP r isrptr)
unEx (Nx s)      = return $ ESEQ(s, CONST 0 False)

unCx :: Gexp -> SemTr (Tmp.Label -> Tmp.Label -> Stm)
unCx (Ex (CONST 1 _)) = return $ (\t -> \_ -> JUMP(NAME(t), [t]))
unCx (Ex (CONST 0 _)) = return $ (\_ -> \f -> JUMP(NAME(f), [f]))
unCx (Ex e) = return $ (\t -> \f -> CJUMP(TEST(NE, e, CONST 0 False), t, f))
unCx (Cx genstm) = return genstm
unCx (Nx _) = error "Compiler error: Impossible usage of unCx"

unNx :: Gexp -> SemTr (Stm)
unNx (Ex e) = return $ EXP e
unNx (Nx stm) = return stm
unNx c = do e <- unEx c
            unNx $ Ex e



-- Cx-constructed expression comparing two strings for equality
eqStr :: Gexp -> Gexp -> SemTr Gexp
eqStr str1 str2 = 
  do str1' <- unEx str1
     str2' <- unEx str2
     funclabel <- namedLabel "stringEqual"
     retlab <- newRetLabel
     return $ Ex $ CALL (NAME funclabel, [str1', str2']) False retlab

notEqStr :: Gexp -> Gexp -> SemTr Gexp
notEqStr str1 str2 = 
  do (Ex eqstr) <- eqStr str1 str2
     return $ Cx $ \t -> 
                   \f -> CJUMP(TEST (EQ, eqstr, CONST 0 False), t, f)
                        

strLessThan :: Gexp -> Gexp -> SemTr Gexp
strLessThan str1 str2 = 
  do str1' <- unEx str1
     str2' <- unEx str2
     funclabel <- namedLabel "stringLessThan"
     retlab <- newRetLabel
     return $ Ex $ CALL (NAME funclabel, [str1', str2']) False retlab

strLessThanOrEq :: Gexp -> Gexp -> SemTr Gexp
strLessThanOrEq str1 str2 = 
  do lab <- newLabel
     Ex eq <- eqStr str1 str2
     Ex lt <- strLessThan str1 str2
     return $ Cx $ \t ->
                   \f -> seqcon [ CJUMP (TEST (EQ, eq, CONST 0 False), lab, t)
                                , LABEL lab
                                , CJUMP (TEST (EQ, lt, CONST 0 False), f, t)]
                               


-- Comparing non-string values
eqCmp :: Gexp -> Gexp -> SemTr Gexp
eqCmp g1 g2 = 
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> CJUMP(TEST (EQ, g1', g2'), t, f)
 
notEqCmp :: Gexp -> Gexp -> SemTr Gexp
notEqCmp g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> CJUMP(TEST (NE, g1', g2'), t, f)

lessThan :: Gexp -> Gexp -> SemTr Gexp
lessThan g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> CJUMP(TEST (LT, g1', g2'), t, f)

lessThanOrEq :: Gexp -> Gexp -> SemTr Gexp
lessThanOrEq g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> CJUMP(TEST (LE, g1', g2'), t, f)

-- Arithmetic
arithmetic :: Absyn.Oper -> Gexp -> Gexp -> SemTr Gexp
arithmetic op g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Ex $ BINOP (transop op, g1', g2') (isExpPtr g1' `xor` isExpPtr g2')
  where transop Absyn.PlusOp   = PLUS
        transop Absyn.MinusOp  = MINUS
        transop Absyn.TimesOp  = MUL
        transop Absyn.DivideOp = DIV
        transop Absyn.AndOp    = AND
        transop Absyn.OrOp     = OR
        transop o              = error $ "Compiler error : " ++ show o ++ "not implemented yet"

-- Literal
intExp :: Int -> SemTr Gexp
intExp val = return $ Ex $ CONST val False

stringExp :: String -> SemTr Gexp
stringExp str =
  do lab <- newLabel
     let frag = Frame.DATA lab str
     createDataFrag frag
     return $ Ex $ NAME lab


-- Helper sequence function
constructEseq :: Gexp -> Gexp -> SemTr Gexp
constructEseq stm e =
  do stm' <- unNx stm
     exp' <- unEx e
     return $ Ex $ ESEQ (stm', exp')

letExpression :: [Gexp] -> Gexp -> SemTr Gexp
letExpression []   body = return body
letExpression decs body =
  do decs' <- mapM unNx decs
     body' <- unEx body
     return $ Ex $ ESEQ (seqcon decs', body')

-- Assignment
assign :: Gexp -> Gexp -> SemTr Gexp
assign var assgnval =
  do var' <- unEx var
     assgnval' <- unEx assgnval
     return $ Nx $ MOVE (var', assgnval')

-- Record and Array creation
createRecord :: [(Gexp,Bool)] -> SemTr Gexp
createRecord fieldvarsAndIsPtrs =
  do let (fieldvars, isptrs) = unzip fieldvarsAndIsPtrs

     let bool2descriptor True = 'P'
         bool2descriptor False = 'N'

     let descriptors = map bool2descriptor isptrs
     descriptorlab <- newLabel
     let descriptorFrag = Frame.GCDESCREC descriptorlab descriptors
     createDescFrag descriptorFrag

     let isaptr = True
     address <- newTemp isaptr
     allocfunlab <- namedLabel "allocRecord"
     retlab <- newRetLabel

     let alloc = MOVE(TEMP address isaptr, CALL (NAME allocfunlab, [CONST (4 * length fieldvars) False, NAME descriptorlab]) True retlab)
     let idxs  = [1..length fieldvars]
     instrs <- mapM (uncurry $ initfield address isptrs) $ zip fieldvars idxs
     return $ Ex $ ESEQ(seqcon $ alloc:instrs, TEMP address isaptr)
  where initfield address isptrs fieldvar idx = do 
          fieldvar' <- unEx fieldvar
          let isbaseptr = True
          let baseaddr = TEMP address isbaseptr
          let addr = MEM (BINOP(PLUS, baseaddr, CONST (idx * 4) False) True, 4) (isptrs!!idx)
          return $ MOVE (addr, fieldvar')

createArray :: Gexp -> Gexp -> SemTr Gexp
createArray sizexp initexp =
  do sizexp'  <- unEx sizexp
     initexp' <- unEx initexp
     allocarrfun <- namedLabel "allocArray"

     retlab <- newRetLabel

     desclab <- newLabel
     let descriptors = Frame.GCDESCARR desclab $ isExpPtr initexp'
     createDescFrag descriptors

     return $ Ex $ CALL (NAME allocarrfun, [sizexp', initexp', NAME desclab]) True retlab

-- Variable access
field :: Gexp -> Int -> Bool -> SemTr Gexp
field recordge fieldnum isptr =
  do fieldfunlab <- namedLabel "field"
     recordge'   <- unEx recordge
     retlab <- newRetLabel
     return $ Ex $ MEM(CALL(NAME fieldfunlab, [recordge', CONST (4*fieldnum) False]) True retlab, 4) isptr 

subscript :: Gexp -> Gexp -> Bool -> SemTr Gexp
subscript arrge idxge isptr =
  do arrge' <- unEx arrge
     idxge' <- unEx idxge
     subscriptfunlab <- namedLabel "subscript"
     retlab <- newRetLabel
     return $ Ex $ MEM(CALL(NAME subscriptfunlab, [arrge', idxge']) True retlab, 4) isptr

simpleVar :: Access -> Level -> Bool -> SemTr Gexp
simpleVar (varlevel, offset) fromLevel isptr =
  return $ Ex $ accessFrameOff offset (frameAtLevel varlevel fromLevel $ TEMP (Tmp.Named Reg.EBP) False) isptr

frameAtLevel :: Level -> Level -> Exp -> Exp
frameAtLevel destlvl startlvl startlvlptr =
  if destlvl == startlvl
     then startlvlptr
     else case startlvl of
            TOP -> error "Functions from TOP level should not access static links"
            LEVEL{staticLinkOffset=offset, levelParent=parent} -> frameAtLevel destlvl parent $ accessFrameOff offset startlvlptr False

accessFrameOff :: Int -> Exp -> Bool -> Exp
accessFrameOff offset frameptr = MEM(BINOP (PLUS, frameptr, CONST offset False) True, 4)

-- Conditional and loops
ifThen :: Gexp -> Gexp -> SemTr Gexp
ifThen testge thenge =
  do testge' <- unCx testge
     thenge' <- unNx thenge
     t <- newLabel
     f <- newLabel
     return $ Nx $ seqcon [(testge' t f), LABEL t, thenge', LABEL f]

ifThenElse :: Gexp -> Gexp -> Gexp -> Bool -> SemTr Gexp
ifThenElse testge (Nx thenstm) (Nx elsestm) _ =
  do testge' <- unCx testge
     t <- newLabel
     f <- newLabel
     j <- newLabel
     return $ Nx $ seqcon [ testge' t f
                          , LABEL t
                          , thenstm
                          , JUMP (NAME j, [j])
                          , LABEL f
                          , elsestm
                          , LABEL j]
ifThenElse testge thenge elsege ispointer =
  do testge' <- unCx testge
     t <- newLabel
     f <- newLabel
     j <- newLabel
     r <- newTemp ispointer
     thenge' <- unEx thenge
     elsege' <- unEx elsege
     return $ Ex $ ESEQ (  seqcon [
                             testge' t f
                           , LABEL t
                           , MOVE (TEMP r ispointer, thenge')
                           , JUMP (NAME j, [j])
                           , LABEL f
                           , MOVE (TEMP r ispointer, elsege')
                           , LABEL j
                           ]
                         , TEMP r ispointer)

whileLoop :: Gexp -> Gexp -> Tmp.Label -> SemTr Gexp
whileLoop testge bodyge donelab =
  do testge' <- unCx testge
     bodyge' <- unNx bodyge
     testlab <- newLabel
     bodylab <- newLabel
     return $ Nx $ seqcon [ LABEL testlab
                          , testge' bodylab donelab
                          , LABEL bodylab
                          , bodyge'
                          , JUMP (NAME testlab, [testlab])
                          , LABEL donelab ]

forLoop :: Gexp -> Gexp -> Gexp -> Tmp.Label -> Gexp -> SemTr Gexp
forLoop loge hige bodyge donelab iteratorge =
  do loge' <- unEx loge
     hige' <- unEx hige
     bodyge' <- unNx bodyge
     iteratorge' <- unEx iteratorge
     limit <- newTemp False
     bodylab <- newLabel
     inclab  <- newLabel
     return $ Nx $ seqcon [ MOVE(iteratorge', loge')
                          , MOVE(TEMP limit False, hige')
                          , CJUMP (TEST (LE, iteratorge', TEMP limit False), bodylab, donelab)
                          , LABEL bodylab
                          , bodyge'
                          , CJUMP (TEST(LT, iteratorge', TEMP limit False), inclab, donelab)
                          , LABEL inclab
                          , MOVE(iteratorge', BINOP(PLUS, iteratorge', CONST 1 False) False)
                          , JUMP(NAME bodylab, [bodylab])
                          , LABEL donelab
                          ]

break :: Tmp.Label -> SemTr Gexp
break lab =
  return $ Nx $ JUMP(NAME lab, [lab])

updateMaxArgs :: Level -> Int -> SemTr ()
updateMaxArgs callerLvl numArgs =
  case callerLvl of
    TOP -> error "Compiler error: updateMaxArgs called with TOP level."
    LEVEL lvlframe _ _ _ -> case lvlframe of
                              Frame.Frame {Frame.frameMaxArgs=maxargref} -> do 
                                maxargs <- liftIO $ readIORef maxargref
                                when (numArgs > maxargs) $ liftIO $ writeIORef maxargref numArgs

callFunction :: Tmp.Label -> Level -> Level -> [Gexp] -> Bool -> SemTr Gexp
callFunction funclab callerlvl calleelvl argsge isptr =
  do argsge' <- mapM unEx argsge
     if calleelvl == TOP
        then do updateMaxArgs callerlvl $ length argsge'
                retlab <- newRetLabel
                return $ Ex $ CALL (NAME funclab, argsge') isptr retlab
        else do updateMaxArgs callerlvl $ 1+length argsge'
                let calleeparent = levelParent calleelvl
                let staticlinkexp = frameAtLevel calleeparent callerlvl $ TEMP (Tmp.Named Reg.EBP) False
                retlab <- newRetLabel
                return $ Ex $ CALL (NAME funclab, staticlinkexp:argsge') isptr retlab

wrapFuncBody :: Stm -> Bool -> SemTr Stm
wrapFuncBody (EXP bodyexp) isptr =
  do temp <- newTemp isptr
     return $ seqcon [ MOVE (TEMP temp isptr, bodyexp)
                     , MOVE (TEMP (Tmp.Named Reg.EAX) isptr, TEMP temp isptr) ]
wrapFuncBody body _ = return body

createProcFrag :: Tmp.Label -> Level -> Gexp -> Bool -> SemTr ()
createProcFrag proclab level bodyge returnsptr =
  do bodyge' <- unNx bodyge
     wrappedbody <- wrapFuncBody bodyge' returnsptr
     let procfrag = Frame.PROC { Frame.procName  = proclab
                               , Frame.procBody  = wrappedbody
                               , Frame.procFrame = levelFrame level }
     frags <- getFragList
     putFragList $ procfrag:frags

createMainFrag :: Level -> Gexp -> SemTr ()
createMainFrag lvl bodyge = 
  do mainlab <- namedLabel "tigermain"
     createProcFrag mainlab lvl bodyge False

createDataFrag :: Frag -> SemTr ()
createDataFrag f@(Frame.DATA _ _) = do
  frags <- getFragList
  putFragList $ f:frags
createDataFrag _ = error "Compiler error: createDataFrag called with non-DATA fragment."

createDescFrag :: Frag -> SemTr ()
createDescFrag f@(Frame.GCDESCREC _ _) = do
  frags <- getFragList
  putFragList $ f:frags

createDescFrag f@(Frame.GCDESCARR _ _) = do
  frags <- getFragList
  putFragList $ f:frags

createDescFrag _ = error "Compiler error: createDescFrag called with non-DESC fragment."
  
reset :: SemTr ()
reset = putFragList []

getResult :: SemTr [Frag]
getResult = getFragList
