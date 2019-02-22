module TigerCodeGen
  (
    codegen
  , stringdata
  , recorddescriptordata
  , arraydescriptordata
  , procEntryExit
  )
  where

import TigerAssem
import qualified TigerTemp as Tmp
import TigerFrame
import TigerRegisters
import TigerITree
import qualified TigerGenSymLabTmp as TGSLT
import Prelude hiding (EQ, LT, GT)
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Arrow (first)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List
import Data.IORef

import Data.Word
import Data.Bits

import Numeric


named :: Register -> Tmp.Temp
named = Tmp.Named

codegen :: Stm -> TGSLT.GenSymLabTmpState -> ([Instr], TGSLT.GenSymLabTmpState)
codegen stm st = let monad = codegen' stm
                     gsltmonad = execStateT monad []
                     (instrs, st') = (runIdentity . TGSLT.runGSLT st) gsltmonad
                 in  (reverse instrs, st')

stringdata :: Tmp.Label -> String -> [Instr]
stringdata lab str = [
                       DIRECTIVE ".data"
                     , TigerAssem.LABEL (TGSLT.name lab)
                     , DIRECTIVE $ ".4byte " ++ show (length str)
                     , DIRECTIVE $ ".string " ++ show str
                     ]

recordmagicnumber :: Word32
recordmagicnumber = 0xDEAFCACA

recorddescriptordata :: Tmp.Label -> String -> [Instr]
recorddescriptordata lab str = [
                                 DIRECTIVE ".data"
                               , TigerAssem.LABEL (TGSLT.name lab)
                               , DIRECTIVE $ ".4byte 0x" ++ showHex recordmagicnumber ""
                               , DIRECTIVE $ ".4byte " ++ show (length str)
                               , DIRECTIVE $ ".string " ++ show str
                               ]

arraymagicnumber :: Word32
arraymagicnumber = 0xDEABCACA

arraydescriptordata :: Tmp.Label -> Bool -> [Instr]
arraydescriptordata lab isptr = [
                                  DIRECTIVE ".data"
                                , TigerAssem.LABEL (TGSLT.name lab)
                                , DIRECTIVE $ ".4byte 0x" ++ showHex arraymagicnumber ""
                                , DIRECTIVE $ ".4byte " ++ if isptr then "1" else "0"
                                ]


-- Codegen Monad
type Codegen = StateT [Instr] (TGSLT.GenSymLabTmp Identity)

newTemp :: Bool -> Codegen Tmp.Temp
newTemp = lift . TGSLT.newTemp

emit :: Instr -> Codegen ()
emit instr = do instrs <- get
                put $ instr:instrs

comment :: String -> Instr
comment str = CMT $ COMMENT str

codegen' :: Stm -> Codegen ()
codegen' s0 =
  let
    op2jmp :: Relop -> String -> Assem
    op2jmp EQ = JE
    op2jmp NE = JNE
    op2jmp LT = JL
    op2jmp GT = JG
    op2jmp LE = JLE
    op2jmp GE = JGE
    op2jmp op = error $ "Compiler error: Jump operator " ++ show op ++ " not yet implemented."

    genCall :: Tmp.Label -> Tmp.RetLabel -> [Exp] -> Bool -> Codegen ()
    genCall f retlab args shouldsavecaller =
      do let calldefs = map named [EAX, ECX, EDX]
         when shouldsavecaller saveCallerSaves
         emit $ comment "Entering GC"
         emit $ OPER PUSHA (map named [EAX, EBX, ECX, EDX, EDI, ESI, EBP, ESP]) [named ESP] Nothing
         emit $ OPER (PUSH $ named ESP) [named ESP] [named ESP] Nothing
         emit $ OPER (LEAL (TGSLT.name retlab) (named EAX)) [] [named EAX] Nothing
         emit $ OPER (PUSH $ named EAX) [named EAX] [named ESP] Nothing
         emit $ OPER (CALLL "gcentry" dummyRetLab) [named ESP] (map named [ESP, EBP]) Nothing
         emit $ OPER (ADDCR 8 $ named ESP) [named ESP] [named ESP] Nothing
         emit $ OPER POPA [named ESP] (map named [EAX, EBX, ECX, EDX, EDI, ESI, EBP, ESP]) Nothing
         emit $ comment "Exiting GC"
         emit $ comment "Pushing arguments on stack in reverse order"
         mapM_ munchArg $ reverse args
         emit $ comment "Done pushing arguments"
         emit $ OPER (CALLL (TGSLT.name f) retlab) [named ESP] (calldefs ++ map named [ESP, EBP])
                Nothing
         emit $ TigerAssem.LABEL $ TGSLT.name retlab
         unless (null args) $
           do emit $ comment "Free arguments from stack"
              emit $ OPER (ADDCR (length args * 4) (named ESP))
                          [named ESP] (map named [ESP]) Nothing
              emit $ comment "Done freeing arguments from stack"
         when shouldsavecaller restoreCallerSaves

    saveCallerSaves :: Codegen ()
    saveCallerSaves =
      do emit $ comment "Pushing caller save registers on stack"
         emit $ OPER (PUSH $ named EAX) (map named [EAX, ESP]) [named ESP] Nothing
         emit $ OPER (PUSH $ named ECX) (map named [ECX, ESP]) [named ESP] Nothing
         emit $ OPER (PUSH $ named EDX) (map named [EDX, ESP]) [named ESP] Nothing
         emit $ OPER (PUSH $ named EBX) (map named [EBX, ESP]) [named ESP] Nothing
         emit $ OPER (PUSH $ named EDI) (map named [EDI, ESP]) [named ESP] Nothing
         emit $ OPER (PUSH $ named ESI) (map named [ESI, ESP]) [named ESP] Nothing
         emit $ comment "Done pushing caller save registers on stack"
         

    restoreCallerSaves :: Codegen ()
    restoreCallerSaves =
      do emit $ comment "Restoring caller save registers"
         emit $ OPER (POP $ named ESI) [named ESP] (map named [ESI, ESP]) Nothing
         emit $ OPER (POP $ named EDI) [named ESP] (map named [EDI, ESP]) Nothing
         emit $ OPER (POP $ named EBX) [named ESP] (map named [EBX, ESP]) Nothing
         emit $ OPER (POP $ named EDX) [named ESP] (map named [EDX, ESP]) Nothing
         emit $ OPER (POP $ named ECX) [named ESP] (map named [ECX, ESP]) Nothing
         emit $ OPER (POP $ named EAX) [named ESP] (map named [EAX, ESP]) Nothing
         emit $ comment "Done restoring caller save registers"

    munchStm :: Stm -> Codegen ()
    munchStm (SEQ(_, _)) = error "Compiler error: ITree is not canonicalized."
    munchStm (MOVE(TEMP t _, CALL(NAME f, args) _ retlab)) =
      do saveCallerSaves
         genCall f retlab args False
         emit $ comment $ "saving RV into " ++ show t
         emit $ MOV (MOVRR (named EAX) (Tmp.DST 0)) (named EAX) t
         restoreCallerSaves

    munchStm (MOVE(TEMP t _, e)) =
      do src <- munchExp e
         emit $ MOV (MOVRR (Tmp.SRC 0) (Tmp.DST 0)) src t

    munchStm (MOVE(MEM(TEMP tmp _, _) _, e)) =
      do src <- munchExp e
         emit $ OPER (MOVRM (Tmp.SRC 1) $ mkaddr (Tmp.SRC 0) 0) [tmp, src] [] Nothing

    munchStm (MOVE(MEM(BINOP(PLUS, e1, CONST i _) _, _) _, e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRM (Tmp.SRC 1) $ mkaddr (Tmp.SRC 0) i) [src1, src2] [] Nothing

    munchStm (MOVE(MEM(BINOP(PLUS, CONST i isiptr, e1) isbptr, sz) ismemptr, e2)) =
      munchStm (MOVE(MEM(BINOP(PLUS, e1, CONST i isiptr) isbptr, sz) ismemptr, e2))

    munchStm (MOVE(MEM(BINOP(MINUS, e1, CONST i _) _, _) _, e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRM (Tmp.SRC 1) $ mkaddr (Tmp.SRC 0) $ -i) [src1, src2] [] Nothing

    munchStm (MOVE(MEM(BINOP(MINUS, CONST i _, e1) _, _) _, e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (NEGR (Tmp.SRC 0)) [src1] [] Nothing
         emit $ OPER (MOVRM (Tmp.SRC 1) $ mkaddr (Tmp.SRC 0) i) [src1, src2] [] Nothing

    munchStm (MOVE(MEM(e1, _) _, MEM(e2, _) ismem2ptr)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         t <- newTemp ismem2ptr
         emit $ OPER (MOVMR (mkaddr (Tmp.SRC 0) 0) (Tmp.DST 0)) [src2] [t] Nothing
         emit $ OPER (MOVRM (Tmp.SRC 1) (mkaddr (Tmp.SRC 0) 0)) [src1, t] [] Nothing

    munchStm (MOVE(MEM(e1, _) _, e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRM (Tmp.SRC 1) (mkaddr (Tmp.SRC 0) 0)) [src1, src2] [] Nothing

    munchStm (MOVE(e1, e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRR (Tmp.SRC 0) (Tmp.DST 0)) [src2] [src1] Nothing

    munchStm (JUMP(NAME lab, lablist)) =
      emit $ OPER (JMP $ TGSLT.name lab) [] [] (Just $ map TGSLT.name lablist)

    munchStm (CJUMP(TEST(relop, e1, e2), lab1, lab2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         let jmp = op2jmp relop
         emit $ OPER (CMPRR (Tmp.SRC 1) (Tmp.SRC 0)) [src1, src2] [] Nothing
         emit $ OPER (jmp $ TGSLT.name lab1) [] [] (Just $ map TGSLT.name [lab1, lab2])

    munchStm (TigerITree.LABEL lab) =
      emit $ TigerAssem.LABEL (TGSLT.name lab)

    munchStm (EXP(CALL(NAME f, args) _ retlab)) =
      genCall f retlab args True

    munchStm (EXP e) =
      void $ munchExp e

    munchStm _ = error "Compiler error: Impossible pattern in munchStm."

    munchExp :: Exp -> Codegen Tmp.Temp
    munchExp (ESEQ(_, _)) = error "Compiler error: ITree is not canonicalized."

    munchExp (TEMP t _) = return t

    munchExp (MEM(CONST i _, _) _) =
      do r <- newTemp True
         emit $ OPER (MOVMR (mkaddr (named ZERO) i) (Tmp.DST 0)) [] [r] Nothing
         return r

    munchExp (MEM(BINOP(PLUS, e, CONST i _) _, _) _) =
      do t <- munchExp e
         r <- newTemp True
         emit $ OPER (MOVMR (mkaddr (Tmp.SRC 0) i) (Tmp.DST 0)) [t] [r] Nothing
         return r

    munchExp (MEM(BINOP(PLUS, CONST i isiptr, e) isbptr, sz) ismemptr) =
      munchExp (MEM(BINOP(PLUS, e, CONST i isiptr) isbptr, sz) ismemptr)

    munchExp (MEM(BINOP(MINUS, e, CONST i _) _, _) _) =
      do t <- munchExp e
         r <- newTemp True
         emit $ OPER (MOVMR (mkaddr (Tmp.SRC 0) (-i)) (Tmp.DST 0)) [t] [r] Nothing
         return r

    munchExp (MEM(e, _) ismemptr) =
      do t <- munchExp e
         r <- newTemp ismemptr
         emit $ OPER (MOVMR (mkaddr (Tmp.SRC 0) 0) (Tmp.DST 0)) [t] [r] Nothing
         return r

    munchExp (BINOP(MUL, e1, e2) isbptr) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         r <- newTemp isbptr
         emit $ OPER (MOVRR (Tmp.SRC 0) (Tmp.DST 0)) [t1] [r] Nothing
         emit $ OPER (IMULRR (Tmp.SRC 0) (Tmp.DST 0)) [t2, r] [r, named EDX] Nothing
         return r

    munchExp (BINOP(DIV, e1, e2) isbptr) =
      do r <- newTemp isbptr

         t1 <- munchExp e1
         emit $ OPER (MOVRR (Tmp.SRC 0) (named EAX)) [t1] [named EAX] Nothing

         emit $ OPER (PUSH $ named EAX) (map named [ESP, EAX]) [named ESP] Nothing

         t2 <- munchExp e2
         emit $ OPER (MOVRR (Tmp.SRC 0) (Tmp.DST 0)) [t2] [r] Nothing

         emit $ OPER (POP $ named EAX) [named ESP] [named EAX, named ESP] Nothing
         emit $ OPER CDQ [] [] Nothing
         emit $ OPER (IDIVR (Tmp.SRC 0)) [r, named EAX, named EDX] (map named [EAX, EDX])
                     Nothing
         emit $ OPER (MOVRR (named EAX) (Tmp.DST 0)) [named EAX] [r] Nothing
         return r

    munchExp (BINOP(PLUS, CONST i _, e) isbptr) =
      do t <- munchExp e
         r <- newTemp isbptr
         emit $ MOV (MOVRR (Tmp.SRC 0) (Tmp.DST 0)) t r
         emit $ OPER (ADDCR i (Tmp.SRC 0)) [r] [r] Nothing
         return r

    munchExp (BINOP(PLUS, e, CONST i isiptr) isbptr) =
      munchExp (BINOP(PLUS, CONST i isiptr, e) isbptr)

    munchExp (BINOP(PLUS, e1, e2) _) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (ADDRR (Tmp.SRC 0) (Tmp.DST 0)) [t1, t2] [t2] Nothing
         return t2

    munchExp (BINOP(MINUS, e1, e2) _) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (SUBRR (Tmp.SRC 1) (Tmp.DST 0)) [t1, t2] [t1] Nothing
         return t1

    munchExp (BINOP(AND, e1, e2) _) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (ANDRR (Tmp.SRC 0) (Tmp.DST 0)) [t1, t2] [t2] Nothing
         return t2

    munchExp (BINOP(OR, e1, e2) _) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (ORRR (Tmp.SRC 0) (Tmp.DST 0)) [t1, t2] [t2] Nothing
         return t2

    munchExp (CONST i isiptr) =
      do r <- newTemp isiptr
         emit $ OPER (MOVCR i (Tmp.DST 0)) [] [r] Nothing
         return r

    munchExp (NAME lab) =
      do r <- newTemp False
         emit $ OPER (LEAL (TGSLT.name lab) (Tmp.DST 0)) [] [r] Nothing
         return r

    munchExp e = error $ "Compiler error: Impossible pattern: " ++ show e ++ "."

    munchArg :: Exp -> Codegen ()
    munchArg (CONST i _) =
      emit $ OPER (PUSHC i) [named ESP] [named ESP] Nothing

    munchArg (MEM(BINOP(PLUS, e, CONST i _) _, _) _) =
      do t <- munchExp e
         emit $ OPER (PUSHM (mkaddr (Tmp.SRC 0) i)) [t, named ESP] [named ESP] Nothing

    munchArg (MEM(BINOP(PLUS, CONST i isiptr, e) isbptr, sz) ismemptr) =
      munchArg (MEM(BINOP(PLUS, e, CONST i isiptr) isbptr, sz) ismemptr)

    munchArg (MEM(BINOP(MINUS, e, CONST i _) _, _) _) =
      do t <- munchExp e
         emit $ OPER (PUSHM (mkaddr (Tmp.SRC 0) (-i))) [t, named ESP] [named ESP] Nothing

    munchArg (MEM(e, _) _) =
      do t <- munchExp e
         emit $ OPER (PUSHM $ mkaddr (Tmp.SRC 0) 0) [t, named ESP] [named ESP] Nothing

    munchArg e =
      do t <- munchExp e
         emit $ OPER (PUSH (Tmp.SRC 0)) [t, named ESP] [named ESP] Nothing
      
  in
    munchStm s0


type ColorResult = Map.Map Tmp.Temp Register
type TMap = Map.Map Tmp.Temp Bool
type RMap = Map.Map Register Bool
type SMap = Map.Map Int Bool

tmap2rmap :: [Tmp.Temp] -> TMap -> ColorResult -> RMap
tmap2rmap tmps tmap color =
  let focusedtmap = Map.fromList $ filter (\(t, _) -> t `elem` tmps) $ Map.toList tmap
      rmap = Map.mapKeys (\t -> fromMaybe (error $ show t ++ " not found in color map!") $ Map.lookup t color) 
             focusedtmap
  in  rmap

dummyRetLab :: Tmp.RetLabel
dummyRetLab = ("", 0)

iscallassem :: Assem -> Bool
iscallassem (CALLL _ r) = r /= dummyRetLab
iscallassem (CALLR _ r) = r /= dummyRetLab
iscallassem _ = False

iscallinstr :: Instr -> Bool
iscallinstr OPER{opAssem=assem} = iscallassem assem
iscallinstr _ = False

findAllCalls :: [(Instr, [Tmp.Temp])] -> [(Instr, [Tmp.Temp])]
findAllCalls = filter (\(i, _) -> iscallinstr i)

extractRetLabAssem :: Assem -> Tmp.RetLabel
extractRetLabAssem (CALLL _ retlab) = retlab
extractRetLabAssem (CALLR _ retlab) = retlab
extractRetLabAssem _ = error "Compiler Error: extractRetLab called with non-CALL Assem!"

extractRetLabInstr :: Instr -> Tmp.RetLabel
extractRetLabInstr OPER{opAssem=assem} = extractRetLabAssem assem
extractRetLabInstr _ = error "Compiler Error: extractRetLab called with non-CALL Instr!"

makeRMaps :: [(Instr, [Tmp.Temp])] -> TMap -> ColorResult -> Map.Map Tmp.RetLabel RMap
makeRMaps instrs tmap color =
  let callinstrs = findAllCalls instrs
      retlivetemps = map (first extractRetLabInstr) callinstrs
      retrmaps = map (\(rl, ts) -> (rl, tmap2rmap ts tmap color)) retlivetemps
  in  Map.fromList retrmaps

makeRegDirective :: RMap -> Word32
makeRegDirective rmap =
  let 
     usage = map inuse [EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP]
  in regbit usage 0 0
  where inuse r = case Map.lookup r rmap of
                    Just True -> True
                    _         -> False
        regbit :: [Bool] -> Int -> Word32 -> Word32
        regbit [] _ res        = res
        regbit (True:ts) i res = regbit ts (i+1) (bit i .|. res)
        regbit (_:ts) i res    = regbit ts (i+1) res

ispseudo :: Register -> Bool
ispseudo (PSEUDO _) = True
ispseudo _ = False

makePseudoRegDirective :: RMap -> String
makePseudoRegDirective rmap =
  let focusedrmap = Map.filterWithKey (\k _ -> ispseudo k) rmap
      focusedpmap = Map.filter id focusedrmap
      focusedlist = Map.toList focusedpmap
      directives  = map (\(PSEUDO d, _) -> ".4byte " ++ show (-4*d)) focusedlist
  in  List.intercalate "\n" directives

makeStackDirective :: SMap -> String
makeStackDirective smap =
  let focusedsmap = Map.filter id smap
      focusedslist = Map.toList focusedsmap
      directives = map (\(v, _) -> ".4byte " ++ show v) focusedslist
  in List.intercalate "\n" directives

makePtrMap :: (Tmp.RetLabel, RMap) -> SMap -> [Instr]
makePtrMap (retlab, rmap) stackmap =
  let regbits = makeRegDirective rmap
      prev = TGSLT.prevRetLabel retlab
      retptrlab = TGSLT.name retlab ++ "PTRMAP"
      prevptrlab = case prev of
                     Nothing -> "0"
                     Just p  -> p ++ "PTRMAP"
      lab    = TigerAssem.LABEL retptrlab
      dirstr =    ".4byte " ++ prevptrlab ++ "\n"
               ++ ".4byte " ++ TGSLT.name retlab ++ "\n"
               ++ ".4byte " ++ show regbits ++ "\n"
      stackdir = makeStackDirective stackmap
      pdir = makePseudoRegDirective rmap
      stackusagelab = retptrlab ++ "STACKUSAGE"
      stackusagedir = ".4byte " ++ stackusagelab ++ "\n" ++ stackusagelab ++ ":\n"
  in  [lab, DIRECTIVE $ dirstr++stackusagedir++stackdir++"\n"++pdir++"\n.4byte 0"]

makePtrMaps :: SMap -> Map.Map Tmp.RetLabel RMap -> [Instr]
makePtrMaps smap rmaps =
  let rmaplist = Map.toList rmaps
      instrs = map (`makePtrMap` smap) rmaplist
  in  concat instrs

procEntryExit :: Tmp.Label 
              -> [(Instr, [Tmp.Temp])]
              -> ColorResult
              -> [Tmp.Temp]
              -> Frame
              -> TMap
              -> IO [Instr]
procEntryExit name
              instrAndLiveTemps
              alloc
              _ 
              frame
              tempmap =
  do let instrs = map fst instrAndLiveTemps
     prologue <- newIORef []
     epilogue <- newIORef []
     let emitPro i = do pros <- readIORef prologue
                        writeIORef prologue (i:pros)
     let emitEpi i = do epi <- readIORef epilogue
                        writeIORef epilogue (i:epi)
     let labname = TGSLT.name name
     numlocals <- readIORef $ frameLocalCount frame
     stackmap <- readIORef $ frameLocalIsPtr frame

     let stackmapfixed = Map.fromList $ map (\(off, b) -> (off+localbaseoffset, b)) stackmap
     let rmaps = makeRMaps instrAndLiveTemps tempmap alloc
     
     emitPro $ DIRECTIVE $ ".text\n.global "++labname
     emitPro $ TigerAssem.LABEL labname
     emitPro $ comment "prolog begins"
     emitPro $ OPER (PUSH $ Tmp.Named EBP) [Tmp.Named EBP] [Tmp.Named ESP] Nothing
     emitPro $ MOV (MOVRR (Tmp.Named ESP) (Tmp.Named EBP)) (Tmp.Named ESP) (Tmp.Named EBP)
     emitPro $ OPER (PUSH $ Tmp.Named ESP) [Tmp.Named ESP] [Tmp.Named ESP] Nothing
     emitPro $ OPER (CALLL "update_top_stack" dummyRetLab) [] [] Nothing
     emitPro $ OPER (ADDCR 4 (Tmp.Named ESP)) [] [] Nothing
     emitPro $ comment "allocating space for locals"
     emitPro $ OPER (SUBCR (numlocals*4+npseudoregs*4) (Tmp.Named ESP)) [Tmp.Named ESP] [] Nothing
     emitPro $ OPER (PUSH $ Tmp.Named ESP) [Tmp.Named ESP] [Tmp.Named ESP] Nothing
     emitPro $ OPER (CALLL "update_bot_stack" dummyRetLab) [] [] Nothing
     emitPro $ OPER (ADDCR 4 (Tmp.Named ESP)) [] [] Nothing
     emitPro $ comment "prologue ends here"

     emitEpi $ comment "epilogue begins here"
     emitEpi $ MOV (MOVRR (Tmp.Named EBP) (Tmp.Named ESP)) (Tmp.Named EBP) (Tmp.Named ESP)
     emitEpi $ OPER (POP (Tmp.Named EBP)) [] [Tmp.Named EBP] Nothing
     emitEpi $ OPER RET [] [] Nothing
     mapM_ emitEpi (makePtrMaps stackmapfixed rmaps)
     emitEpi $ comment "epilogue ends here"

     let spilledInstrs = concatMap (genSpill alloc) instrs
     proInstrs <- readIORef prologue
     epiInstrs <- readIORef epilogue
     return $ reverse proInstrs ++ spilledInstrs ++ reverse epiInstrs


pseudoreg2addr :: Register -> Addr
pseudoreg2addr (PSEUDO d) = mkaddr (Tmp.Named EBP) $ (-4)*d
pseudoreg2addr _ = error "Compiler error: pseudoreg2addr called with machine register."

genSpill :: Map.Map Tmp.Temp Register -> Instr -> [Instr]
genSpill alloc instr =
  let

    availmregs = [ECX, EDX]

    loadfreg src@(PSEUDO _) mreg =
      ([ COMMENT ("loading src pseudoreg: "++show src++" into machine reg: "++show mreg)
       , MOVMR (pseudoreg2addr src) (Tmp.Named mreg)
       ], mreg)
    loadfreg src _ = ([], src)

    mkmov p@(PSEUDO _) r = MOVMR (pseudoreg2addr p) (Tmp.Named r)
    mkmov r p@(PSEUDO _) = MOVRM (Tmp.Named r) (pseudoreg2addr p)
    mkmov r1 r2 = MOVRR (Tmp.Named r1) (Tmp.Named r2)

    mapsrcs [] _ = ([], [])
    mapsrcs (src:srcs) (mreg:mregs) =
      let (loadinstrs, src') = loadfreg src  mreg
          (loadrest, srcs')  = mapsrcs  srcs mregs
      in (loadinstrs++loadrest, src':srcs')
    mapsrcs srcs [] =
      if any ispseudo srcs
        then error "Compiler error: not enough machine register to map pseudo sources."
        else ([], srcs)

    tmp2reg (Tmp.Named r) = r
    tmp2reg t = fromMaybe err $ Map.lookup t alloc
                  where err = error $ "Compiler error: genSpill encountered non-colored temp: "++show t++"."

    mapdsts [] _ _ = ([], [])
    mapdsts ds@(dst:dsts) srcs newsrcs =
      if any ispseudo ds
        then let found = List.elemIndex dst srcs
             in  case found of
                   Just idx -> let src = srcs !! idx
                                   mreg = newsrcs !! idx
                               in  if src /= mreg
                                      then ([ COMMENT ("storing machine reg: "++show mreg++" back into pseudoreg: " ++ show dst), 
                                              mkmov mreg dst
                                            ], mreg:dsts)
                                      else ([], dst:dsts)
                   Nothing -> case dst of
                                PSEUDO _ -> let mreg = head availmregs
                                            in  ([ COMMENT ("storing machine reg: "++show mreg++" back into pseudoreg: " ++ show dst)
                                                 , mkmov mreg dst
                                                 ], mreg:dsts)
                                _ -> ([], dst:dsts)
        else ([], dst:dsts)

    mksimpleop i = OPER i [] [] Nothing

  in case instr of
       OPER i srcs dsts jmp ->
         let (loadinstrs, newsrcs) = mapsrcs (map tmp2reg srcs) availmregs
             (storeinstrs, newdsts) = mapdsts (map tmp2reg dsts) (map tmp2reg srcs) newsrcs
         in if any ispseudo $ newsrcs ++ newdsts
               then error $ "Compiler error: " ++ show newsrcs ++ ", " ++ show newdsts
               else map mksimpleop loadinstrs ++ [OPER i (map Tmp.Named newsrcs) (map Tmp.Named newdsts) jmp] ++ map mksimpleop storeinstrs
       MOV i src dst ->
          let (loadinstrs, newsrcs) = mapsrcs [tmp2reg src] availmregs
              (storeinstrs, newdsts) = mapdsts [tmp2reg dst] [tmp2reg src] newsrcs
          in if any ispseudo $ newsrcs ++ newdsts
               then error $ "Compiler error: " ++ show newsrcs ++ ", " ++ show newdsts
               else map mksimpleop loadinstrs ++ [OPER i (map Tmp.Named newsrcs) (map Tmp.Named newdsts) Nothing] ++ map mksimpleop storeinstrs
       TigerAssem.LABEL _ -> [instr]
       CMT _ -> [instr]
       DIRECTIVE _ -> [instr]
