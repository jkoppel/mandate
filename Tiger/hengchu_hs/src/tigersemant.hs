module TigerSemant
  (
    transprog
  )
  where

import TigerSemTr
import TigerSemantTypes
import qualified TigerLexer       as TLex
import qualified TigerAbsyn       as TAbs
import qualified TigerSymbol      as TSym
import qualified TigerTemp        as TTmp
import qualified TigerTranslate   as TTra
import qualified Data.Map         as Map
import qualified TigerParser      as TPar
import Control.Monad.State
import Control.Monad.Except
import Data.IORef.MonadIO
import Data.List
import Data.Maybe

type GexpTy = (TTra.Gexp, Ty)

-- Functions used to report error
cyclicTypeError :: TLex.AlexPosn -> [String] -> SemantError
cyclicTypeError pos types = SE pos $ TypeLoop types

notCallableError :: TLex.AlexPosn -> String -> SemantError
notCallableError pos str = SE pos $ NotCallable str

typeMisMatchError :: TLex.AlexPosn -> String -> String -> SemantError
typeMisMatchError pos str1 str2 = SE pos $ TypeMismatch str1 str2

argumentNameError :: TLex.AlexPosn -> String -> String -> SemantError
argumentNameError pos str1 str2 = SE pos $ ArgumentName str1 str2

undefinedBinop :: TLex.AlexPosn -> String -> String -> SemantError
undefinedBinop pos str1 str2 = SE pos $ UndefinedBinop str1 str2

undefinedError :: TLex.AlexPosn -> String -> SemantError
undefinedError pos str = SE pos $ Undefined str

argumentCountError :: TLex.AlexPosn -> Int -> Int -> SemantError
argumentCountError pos c1 c2 = SE pos $ ArgumentCount c1 c2

breakOutOfLoop :: TLex.AlexPosn -> SemantError
breakOutOfLoop pos = SE pos $ BreakOutsideOfLoop

duplicateDefinition :: TLex.AlexPosn -> String -> SemantError
duplicateDefinition pos str = SE pos $ DuplicateDefinition str

notVariable :: TLex.AlexPosn -> String -> SemantError
notVariable pos str = SE pos $ NotVariable str

enterLoop :: SemTr ()
enterLoop = do l <- getLoopLevel
               putLoopLevel $ l+1

exitLoop :: SemTr ()
exitLoop = do l <- getLoopLevel
              putLoopLevel $ l-1

actualTy' :: TLex.AlexPosn -> Ty -> [Ty] -> SemTr Ty
actualTy' pos a@(Name (_, iorefmaybety)) searched =
  do if a `elem` searched
        then throwError $ cyclicTypeError pos $ map show searched
        else do maybety <- liftIO $ readIORef iorefmaybety
                case maybety of
                  (Just ty) -> actualTy' pos ty (a:searched)
                  Nothing   -> error "Compiler error: fatal error in cyclic type detection."
actualTy' pos a searched = do if a `elem` searched 
                                 then throwError $ cyclicTypeError pos $ map show searched
                                 else return a

actualTy :: TLex.AlexPosn -> Ty -> SemTr Ty
actualTy pos ty = actualTy' pos ty []

isPointer :: Ty -> SemTr Bool
isPointer String     = return False
isPointer INT        = return False
isPointer Unit       = return False
isPointer t@(Name _) = do
  aty <- actualTy (TLex.AlexPn 0 0 0) t
  isPointer aty
isPointer Nil        = return False
isPointer (Array _)  = return True
isPointer (Record _) = return True


withBinding :: Venv -> Tenv -> SemTr a -> SemTr a
withBinding v t checker = 
  do ov <- getVenv
     ot <- getTenv
     putVenv v
     putTenv t
     a <- checker
     putVenv ov
     putTenv ot
     return a

findFirstDiffInLists :: Eq a => [a] -> [a] -> Maybe Int
findFirstDiffInLists la lb | la == lb  = Nothing
                           | length la /= length lb = error "Compiler error: list a and list b must be the"
                                                            " same length in findFirstDiffInLists."
                           | otherwise = let equalities = zipWith (==) la lb
                                         in  (False) `elemIndex` equalities

zipWithM5 :: (Monad m) => (a -> b -> c -> d -> e -> m g) -> [a] -> [b] -> [c] -> [d] -> [e] -> m [g]
zipWithM5 f (a:args1) (b:args2) (c:args3) (d:args4) (e:args5)= do
  g <- f a b c d e
  gs <- zipWithM5 f args1 args2 args3 args4 args5
  return $ g:gs

zipWithM5 _ [] [] [] [] [] = return []
zipWithM5 _ _ _ _ _ _ = error $ "zipWithM5: Args 1..5 must have the same length."

nestedZip :: [[a]] -> [[b]] -> [[(a, b)]]
nestedZip as bs = zipWith zip as bs

insertList :: (Ord k) => Map.Map k v -> [k] -> [v] -> Map.Map k v
insertList m keys values = foldr (uncurry Map.insert) m (zip keys values)

sym2ty :: TSym.Symbol -> TLex.AlexPosn -> SemTr Ty
sym2ty sym pos = do 
  t <- getTenv
  case Map.lookup sym t of
    Nothing -> throwError $ undefinedError pos $ name sym
    Just ty -> return ty

addtypetobinding :: TSym.Symbol -> Ty -> SemTr ()
addtypetobinding sym ty = do 
  t <- getTenv
  let t' = Map.insert sym ty t
  putTenv t'

addfunctiontobinding :: TSym.Symbol -> TTra.Level -> TTmp.Label -> [(Ty, Access)] -> Ty -> SemTr ()
addfunctiontobinding sym lvl lab params result = do
  v <- getVenv
  let fentry = FunEntry lvl lab params result
  let v' = Map.insert sym fentry v
  putVenv v'

transdec :: TTra.Level -> Maybe TTmp.Label -> TAbs.Dec -> SemTr (Venv, Tenv, [TTra.Gexp])
transdec lvl lab dec =
  let g (TAbs.FunctionDec fdecs) = let fnamesyms    = map TAbs.fundecName fdecs
                                       fparams      = map TAbs.fundecParams fdecs

                                       ffieldnamess = fmap (map TAbs.tfieldName) fparams
                                       ffieldtypess = fmap (map TAbs.tfieldTyp)  fparams
                                       ffieldposess = fmap (map TAbs.tfieldPos)  fparams

                                       fbodyexps = map TAbs.fundecBody fdecs
                                       ftypes    = map TAbs.fundecResult fdecs

                                   in  do fparamtyss <- mapM (mapM (uncurry sym2ty)) (nestedZip ffieldtypess ffieldposess)
                                          (flevels, formalsWithOffsetss) <- fmap unzip $ mapM (TTra.newLevel lvl) fparamtyss
                                          fresulttys <- mapM ftype2ty ftypes
                                          flabs <- mapM (\_ -> newLabel) fdecs
                                          let fentries = zipWith4 FunEntry flevels flabs formalsWithOffsetss fresulttys
                                          v <- getVenv
                                          t <- getTenv
                                          let v' = insertList v fnamesyms fentries
                                          _ <- withBinding v' t $ zipWithM5 (checkbody lab) flevels flabs fresulttys 
                                                                            (zip3 ffieldnamess fparamtyss $ fmap (map snd) formalsWithOffsetss)
                                                                            fbodyexps
                                          return (v', t, [])
        where ftype2ty (Just (s, pos)) = sym2ty s pos
              ftype2ty Nothing         = return Unit

              checkbody newlab newlvl funlab decty (paramsyms, paramtys, paramaccesses) bodyexp = 
                do let varentries = zipWith3 VarEntry paramaccesses paramtys $ take (length paramsyms) $ repeat False
                   v <- getVenv
                   t <- getTenv
                   let v' = insertList v paramsyms varentries
                   (gexp, bodyty) <- withBinding v' t $ transexp newlvl newlab bodyexp
                   let bodyposition = TPar.extractPosition bodyexp
                   bodyty' <- actualTy bodyposition bodyty
                   decty' <- actualTy bodyposition decty
                   if bodyty' == decty'
                      then do retvalIsptr <- isPointer bodyty'
                              TTra.createProcFrag funlab newlvl gexp retvalIsptr
                      else throwError $ typeMisMatchError (TPar.extractPosition bodyexp) (show bodyty) (show decty)

      g (TAbs.VarDec {TAbs.varDecVar=vardec, TAbs.varDecTyp=typandpos, TAbs.varDecInit=initexp, TAbs.varDecPos=pos}) =
        do (initgexp, initty) <- transexp lvl lab initexp
           let varnamesym = TAbs.vardecName vardec
           isinitptr <- isPointer initty
           varaccess <- liftIO $ TTra.allocInFrame isinitptr lvl
           var <- TTra.simpleVar varaccess lvl isinitptr
           assigngexp <- TTra.assign var initgexp
           case typandpos of
             Just (typsym, typpos) -> do typty <- sym2ty typsym typpos
                                         initty' <- actualTy (TPar.extractPosition initexp) initty
                                         typty' <- actualTy typpos typty
                                         if initty' == typty'
                                            then do let varentry = VarEntry varaccess initty' False
                                                    v <- getVenv
                                                    t <- getTenv
                                                    let v' = Map.insert varnamesym varentry v
                                                    return (v', t, [assigngexp])
                                            else throwError $ typeMisMatchError pos (show initty) (show typty)
             Nothing -> do v <- getVenv
                           t <- getTenv
                           let varentry = VarEntry varaccess initty False
                           let v' = Map.insert varnamesym varentry v
                           return (v', t, [assigngexp])
      g (TAbs.TypeDec decs) =
        do v <- getVenv
           t <- getTenv
           let names = map TAbs.typedecName decs
           let poses = map TAbs.typedecPos decs
           mapM_ (uncurry (checkname t)) (zip poses names)
           refNothings <- mapM (\_ -> liftIO $ newIORef Nothing) decs
           let nametypes = zipWith (curry Name) names refNothings
           let t' = insertList t names nametypes
           let absyntys = map TAbs.typedecTy decs
           tys <- mapM (withBinding v t' . transty) absyntys
           mapM_ (\(ref, ty) -> liftIO $ writeIORef ref $ Just ty) (zip refNothings tys)
           return (v, t', [])
        where checkname t pos sym = do case Map.lookup sym t of
                                         Nothing -> return ()
                                         Just _  -> throwError $ duplicateDefinition pos $ name sym
  in g dec

transexp :: TTra.Level -> Maybe TTmp.Label -> TAbs.Exp -> SemTr GexpTy
transexp lvl lab absexp = 
  let g (TAbs.VarExp v)   = transvar lvl lab v
      g (TAbs.NilExp _)   = return (TTra.nilGexp, Nil)
      g (TAbs.IntExp (v, _)) = do gexp <- TTra.intExp v
                                  return (gexp, INT)
      g (TAbs.StringExp (v, _)) = do gexp <- TTra.stringExp v
                                     return (gexp, String)
      g (TAbs.SeqExp []) = error "Compiler error: fatal error in sequence"
                                 " expression type checking"
      g (TAbs.SeqExp (epos:[])) = g $ fst epos
      g (TAbs.SeqExp (epos:eposes)) = do (ge, _) <- g $ fst epos
                                         (ge', ty) <- g $ TAbs.SeqExp eposes
                                         ge'' <- TTra.constructEseq ge ge'
                                         return (ge'', ty)
      g (TAbs.AppExp {TAbs.appFunc=funcsym, TAbs.appArgs=funcargs, TAbs.appPos=pos}) = 
        do v <- getVenv
           case Map.lookup funcsym v of
             Just (FunEntry {funLevel=funlvl
                            ,funLabel=funlab
                            ,funFormals=funformals
                            ,funResult=funresult}) ->
               do (ges, tys) <- liftM unzip $ mapM g funcargs
                  let argposes = map TPar.extractPosition funcargs
                  argtys <- mapM (uncurry actualTy) (zip argposes tys)
                  formaltys <- mapM (actualTy pos) (map fst funformals)
                  case findFirstDiffInLists argtys formaltys of
                    Nothing -> do isresultptr <- isPointer funresult
                                  gexp <- TTra.callFunction funlab lvl funlvl ges isresultptr
                                  return (gexp, funresult)
                    Just idx -> throwError $ typeMisMatchError (TPar.extractPosition $ funcargs !! idx) (show $ argtys !! idx) (show $ formaltys !! idx)
             Just _ -> throwError $ notCallableError pos $ name funcsym
             Nothing -> throwError $ undefinedError pos $ name funcsym
      g (TAbs.OpExp {TAbs.opLeft=lexp, TAbs.opOper=op, TAbs.opRight=rexp, TAbs.opPos=pos}) =
        do (lgexp, lty) <- g lexp
           (rgexp, rty) <- g rexp
           lty' <- actualTy (TPar.extractPosition lexp) lty
           rty' <- actualTy (TPar.extractPosition rexp) rty
           if (lty' == rty')
              then case lty of
                     INT -> do { gexp <- op2fun op lgexp rgexp; return (gexp, INT) }
                     String -> do { gexp <- op2funstr op lgexp rgexp; return (gexp, INT) }
                     Nil -> throwError $ undefinedBinop pos (show Nil) (show rty)
                     Unit -> throwError $ undefinedBinop pos (show Unit) (show rty)
                     _ -> case op of
                            TAbs.EqOp -> do { gexp <- op2fun op lgexp rgexp; return (gexp, INT) }
                            TAbs.NeqOp -> do { gexp <- op2fun op lgexp rgexp; return (gexp, INT) }
                            _ -> throwError $ undefinedBinop pos (show lty) (show rty)
              else throwError $ typeMisMatchError pos (show lty) (show rty)
        where op2fun TAbs.EqOp = TTra.eqCmp
              op2fun TAbs.NeqOp = TTra.notEqCmp
              op2fun TAbs.LtOp  = TTra.lessThan
              op2fun TAbs.LeOp  = TTra.lessThanOrEq
              op2fun TAbs.GtOp  = flip TTra.lessThan
              op2fun TAbs.GeOp  = flip TTra.lessThanOrEq
              op2fun o          = TTra.arithmetic o
              op2funstr TAbs.EqOp = TTra.eqStr
              op2funstr TAbs.NeqOp = TTra.notEqStr
              op2funstr TAbs.LtOp = TTra.strLessThan
              op2funstr TAbs.LeOp = TTra.strLessThanOrEq
              op2funstr TAbs.GtOp = flip TTra.strLessThan
              op2funstr TAbs.GeOp = flip TTra.strLessThanOrEq
              op2funstr _         = error "Compiler error: impossible operator on string."
      g (TAbs.RecordExp {TAbs.recordFields=efields, TAbs.recordTyp=typsym, TAbs.recordPos=pos}) =
        let checkrecord ty = do let typefields = fst ty 
                                let (fieldnames, fieldtys) = unzip typefields
                                let (efieldnames, eexps, eposes) = unzip3 efields
                                if length efieldnames == length fieldnames
                                   then case findFirstDiffInLists efieldnames fieldnames of
                                          Nothing  -> do (gexps, exptys) <- liftM unzip $ mapM g eexps
                                                         exptys' <- mapM (uncurry actualTy) (zip eposes exptys)
                                                         fieldtys' <- mapM (actualTy pos) fieldtys
                                                         case findFirstDiffInLists exptys' fieldtys' of
                                                           Nothing  -> do isptrs <- mapM isPointer fieldtys'
                                                                          finalgexp <- TTra.createRecord $ zip gexps isptrs
                                                                          return (finalgexp, Record ty)
                                                           Just idx -> throwError $ typeMisMatchError
                                                                                    (TPar.extractPosition $ eexps !! idx)
                                                                                    (show $ exptys !! idx)
                                                                                    (show $ fieldtys !! idx)
                                          Just idx -> throwError $ argumentNameError (TPar.extractPosition $ eexps !! idx)
                                                                                     (name $ efieldnames !! idx)
                                                                                     (name $ fieldnames !! idx)
                                   else throwError $ argumentCountError pos (length efieldnames) (length fieldnames)
        in do t <- getTenv
              case Map.lookup typsym t of
                Nothing -> throwError $ undefinedError pos $ name typsym
                Just (Record ty) -> checkrecord ty
                Just typ@(Name _) -> do aty <- actualTy pos typ
                                        case aty of
                                          Record ty -> checkrecord ty
                                          _ -> throwError $ typeMisMatchError pos ("Record") (name typsym)
                Just _ -> throwError $ typeMisMatchError pos ("Record") (name typsym)
      g (TAbs.AssignExp {TAbs.assignVar=var, TAbs.assignExp=aexp, TAbs.assignPos=pos}) =
        do (vgexp, vty) <- transvar lvl lab var
           (agexp, aty) <- g aexp
           vty' <- actualTy pos vty
           aty' <- actualTy (TPar.extractPosition aexp) aty
           if vty' == aty'
              then do gexp <- TTra.assign vgexp agexp
                      return (gexp, Unit)
              else throwError $ typeMisMatchError pos (show vty) (show aty)
      g (TAbs.IfExp {TAbs.ifTest=testexp, TAbs.ifThen=thenexp, TAbs.ifElse=elseexp, TAbs.ifPos=pos}) =
        do (testgexp, testty) <- g testexp
           (thengexp, thenty) <- g thenexp
           thenty' <- actualTy (TPar.extractPosition thenexp) thenty
           if testty == INT
              then case elseexp of
                     Just elseexp' -> do (elsegexp, elsety) <- g elseexp'
                                         elsety' <- actualTy (TPar.extractPosition elseexp') elsety
                                         if (thenty' == elsety')
                                            then do isptr <- isPointer thenty'
                                                    gexp <- TTra.ifThenElse testgexp thengexp elsegexp isptr
                                                    return (gexp, elsety)
                                            else throwError $ typeMisMatchError pos (show thenty) (show elsety)
                     Nothing       -> if (thenty' == Unit)
                                         then do gexp <- TTra.ifThen testgexp thengexp
                                                 return (gexp, Unit)
                                         else throwError $ typeMisMatchError pos (show Unit) (show thenty)
              else throwError $ typeMisMatchError pos (show INT) (show testty)
      g (TAbs.WhileExp {TAbs.whileTest=testexp, TAbs.whileBody=bodyexp, TAbs.whilePos=pos}) =
        do (testgexp, testty) <- g testexp
           if (testty == INT)
              then do donelab <- newLabel
                      enterLoop
                      (bodygexp, bodyty) <- transexp lvl (Just donelab) bodyexp
                      exitLoop
                      bodyty' <- actualTy (TPar.extractPosition bodyexp) bodyty
                      if (bodyty' == Unit)
                         then do gexp <- TTra.whileLoop testgexp bodygexp donelab
                                 return (gexp, Unit)
                         else throwError $ typeMisMatchError pos (show Unit) (show bodyty)
              else throwError $ typeMisMatchError pos (show INT) (show testty)
      g (TAbs.ForExp {TAbs.forVar=vardec, TAbs.forLo=lowexp, TAbs.forHi=highexp, TAbs.forBody=bodyexp, TAbs.forPos=pos}) =
        do let itername = TAbs.vardecName vardec
           (lowgexp, lowty) <- g lowexp
           (highgexp, highty) <- g highexp
           if (lowty == highty)
              then if (lowty == INT)
                      then do v <- getVenv
                              t <- getTenv
                              iteraccess <- liftIO $ TTra.allocInFrame False lvl
                              let v' = Map.insert itername (VarEntry iteraccess INT True) v
                              itergexp<- TTra.simpleVar iteraccess lvl False
                              donelab <- newLabel
                              enterLoop
                              (bodygexp, bodyty) <- withBinding v' t (transexp lvl (Just donelab) bodyexp)
                              exitLoop
                              bodyty' <- actualTy (TPar.extractPosition bodyexp) bodyty
                              if bodyty' == Unit
                                 then do gexp <- TTra.forLoop lowgexp highgexp bodygexp donelab itergexp
                                         return (gexp, Unit)
                                 else throwError $ typeMisMatchError pos (show Unit) (show bodyty)
                      else throwError $ typeMisMatchError pos (show INT) (show lowty)
              else throwError $ typeMisMatchError pos (show lowty) (show highty)
      g (TAbs.BreakExp pos) =
        do l <- getLoopLevel
           if (l > 0)
              then case lab of
                     Just lab' -> do gexp <- TTra.break lab'
                                     return (gexp, Unit)
                     Nothing -> throwError $ breakOutOfLoop pos
              else throwError $ breakOutOfLoop pos
      g (TAbs.LetExp {TAbs.letDecs=decs, TAbs.letBody=bodyexp}) =
        let transdecs (d:[]) = transdec lvl lab d
            transdecs (d:ds) = do (v, t, ges) <- transdec lvl lab d
                                  (v', t', gess) <- withBinding v t $ transdecs ds
                                  return (v', t', ges++gess)
            transdecs [] = do v <- getVenv
                              t <- getTenv
                              return (v, t, [])
        in  do (v', t', ges) <- transdecs decs
               (bodygexp, bodyty) <- withBinding v' t' $ g bodyexp
               gexp <- TTra.letExpression ges bodygexp
               return (gexp, bodyty)
      g (TAbs.ArrayExp {TAbs.arrayTyp=typsym, TAbs.arraySize=sizexp, TAbs.arrayInit=initexp, TAbs.arrayPos=pos}) =
        let checkarray typ ty =  do (sizegexp, sizety) <- g sizexp
                                    if (sizety == INT)
                                       then do (initgexp, initty) <- g initexp
                                               initty' <- actualTy (TPar.extractPosition initexp) initty
                                               ty' <- actualTy pos ty
                                               if (initty' == ty')
                                                  then do gexp <- TTra.createArray sizegexp initgexp
                                                          return (gexp, typ)
                                                  else throwError $ typeMisMatchError pos (show ty) (show initty)
                                       else throwError $ typeMisMatchError pos (show INT) (show sizety)
        in do t <- getTenv
              case Map.lookup typsym t of
                Just typ@(Array (ty, _)) -> checkarray typ ty
                Just typ@(Name _) -> do aty <- actualTy pos typ
                                        case aty of
                                          Array (ty, _) -> checkarray aty ty
                                          _ -> throwError $ typeMisMatchError pos ("Array") (show typsym)
                Just typ -> throwError $ typeMisMatchError pos ("Array") (show typ)
                Nothing -> throwError $ undefinedError pos (name typsym)
           
  in  g absexp

transvar :: TTra.Level -> Maybe TTmp.Label -> TAbs.Var -> SemTr GexpTy
transvar lvl _ (TAbs.SimpleVar(s, pos)) = do v <- getVenv
                                             case Map.lookup s v of
                                               Nothing -> throwError $ undefinedError pos $ name s
                                               Just (VarEntry acc ty _) -> do isptr <- isPointer ty
                                                                              gexp <- TTra.simpleVar acc lvl isptr
                                                                              return (gexp, ty)
                                               Just _ -> throwError $ notVariable pos $ name s
transvar lvl lab (TAbs.FieldVar(v, s, pos)) = do (vgexp, vty) <- transvar lvl lab v
                                                 vty' <- actualTy (TPar.extractPosition v) vty
                                                 case vty' of
                                                   Record(symtypairs, _) ->
                                                     case findIndex (\(sym, _) -> sym == s) symtypairs of
                                                       Nothing  -> throwError $ undefinedError pos $ name s
                                                       Just idx -> do let ty = snd $ symtypairs !! idx
                                                                      isptr <- isPointer ty
                                                                      gexp <- TTra.field vgexp idx isptr
                                                                      return (gexp, ty)
                                                   _ -> throwError $ typeMisMatchError pos ("Record") (show vty')
transvar lvl lab (TAbs.SubscriptVar(v, idxexp, pos)) = do (vgexp, vty) <- transvar lvl lab v
                                                          (idxgexp, idxty) <- transexp lvl lab idxexp
                                                          idxty' <- actualTy pos idxty
                                                          if idxty' == INT 
                                                             then do vty' <- actualTy (TPar.extractPosition v) vty
                                                                     case vty' of
                                                                       Array (innerty, _) -> do 
                                                                         isptr <- isPointer innerty
                                                                         gexp <- TTra.subscript vgexp idxgexp isptr
                                                                         return (gexp, innerty)
                                                                       _ -> throwError $ typeMisMatchError pos ("Array") (show vty)
                                                             else throwError $ typeMisMatchError pos (show INT) (show idxty)
                                                 

transty :: TAbs.Ty -> SemTr Ty
transty (TAbs.NameTy(sym, pos)) =
  do t <- getTenv
     case Map.lookup sym t of
       Nothing -> throwError $ undefinedError pos $ name sym 
       Just ty -> return ty

transty (TAbs.ArrayTy(sym, pos)) =
  do t <- getTenv
     case Map.lookup sym t of
       Nothing -> throwError $ undefinedError pos $ name sym
       Just ty -> do u <- genUniq
                     return $ Array(ty, u)

transty (TAbs.RecordTy tfields) =
  do t <- getTenv
     let names = map TAbs.tfieldName tfields
     let types = map TAbs.tfieldTyp  tfields
     let poses = map TAbs.tfieldPos  tfields
     case findIndex (not . (flip exists t)) types of
       Nothing -> do let tys = map (fromJust . (flip Map.lookup t)) types
                     u <- genUniq
                     return $ Record (zip names tys, u)
       Just idx -> throwError $ undefinedError (poses !! idx) (name $ names !! idx)
  where exists typ t = typ `Map.member` t


transprog :: TAbs.Program -> SemTr [Frag]
transprog absyn = let builtintypes = [("string", String), ("int", INT)]
                      builtinfuncnames = ["chr", "concat", "exit", "flush", "getch"
                                         ,"not", "ord", "print", "size", "substring"]
                      builtinfunctys = [String, String, Unit, Unit,
                                        String, INT, INT, Unit,
                                        INT, String]
                      builtinparamtys = [[INT], [String, String]
                                        ,[INT], [], [], [INT], [String]
                                        ,[String], [String]
                                        ,[String, INT, INT]]
                      
                  in
                    do builtintypsym <- mapM symbol $ map fst builtintypes
                       mapM_ (uncurry addtypetobinding) (zip builtintypsym $ map snd builtintypes)

                       builtinfuncsyms <- mapM symbol builtinfuncnames
                       builtinfunclabs <- mapM namedLabel builtinfuncnames
                       let builtinfunclvls = take (length builtinfuncsyms) $ repeat TTra.outerMost
                       let paramtyswithdummyaccess = liftM (map (\ty -> (ty, (TTra.outerMost, 0)))) builtinparamtys
                       _ <- zipWithM5 addfunctiontobinding builtinfuncsyms builtinfunclvls builtinfunclabs paramtyswithdummyaccess builtinfunctys
                       case absyn of
                         TAbs.Pexp e -> do (mainlvl,_) <- TTra.newLevel TTra.outerMost []
                                           (gexp, _) <- transexp mainlvl Nothing e
                                           TTra.createMainFrag mainlvl gexp
                                           frags <- TTra.getResult
                                           return frags
                         TAbs.Pdecs (ds) -> do (mainlvl,_) <- TTra.newLevel TTra.outerMost []
                                               transprog' mainlvl ds
                                               frags <- TTra.getResult
                                               return frags
  where transprog' mainlvl (d:decs) = do (v, t, _) <- transdec mainlvl Nothing d
                                         withBinding v t $ transprog' mainlvl decs
                                         return ()
        transprog' _ _ = return ()
