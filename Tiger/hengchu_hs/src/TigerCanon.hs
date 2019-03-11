-- Much of the code in this file is adapted from
-- the ML version at here: http://www.cs.princeton.edu/~appel/modern/ml/chap8/canon.sml

module TigerCanon
  (
    linearize
  , basicblocks
  , tracesched
  , canonicalize
  )
  where

import qualified TigerTemp as Tmp
import TigerITree
import TigerGenSymLabTmp
import qualified Data.Map as Map
import Control.Monad.Identity
import Prelude hiding (EQ, LT, GT)

commute :: Stm -> Exp -> Bool
commute (EXP(CONST _ _)) _ = True
commute _ (NAME _) = True
commute _ (CONST _ _) = True
commute _ _ = False

nop :: Stm
nop = EXP(CONST 0 False)

type Canon = GenSymLabTmp Identity

infixl % -- combines two Stm, ignores nop
(%) :: Stm -> Stm -> Stm
s % (EXP(CONST _ _)) = s
(EXP(CONST _ _)) % s = s
a % b              = SEQ(a, b)

notrel :: Relop -> Relop
notrel EQ = NE
notrel NE = EQ
notrel LT = GE
notrel GT = LE
notrel LE = GT
notrel GE = LT
notrel ULT = UGE
notrel ULE = UGT
notrel UGT = ULE
notrel UGE = ULT
notrel FEQ = FNE
notrel FNE = FEQ
notrel FLT = FGE
notrel FLE = FGT
notrel FGT = FLE
notrel FGE = FLT

linearize :: Stm -> Canon [Stm]
linearize stmtobelinearized =
  let

    reorder dofunc (exps, build) =
      let f ((e@(CALL _ iscptr _)):rest) =
            do t <- newTemp iscptr
               f $ ESEQ(MOVE(TEMP t iscptr, e), TEMP t iscptr):rest
          f (a:rest) =
            do (stm0, e) <- dofunc a
               (stm1, el) <- f rest
               if commute stm1 e
                  then return (stm0 % stm1, e:el)
                  else do let iseptr = isExpPtr e
                          t <- newTemp iseptr
                          return (stm0 % MOVE(TEMP t iseptr, e) % stm1, TEMP t iseptr:el)
          f [] = return (nop, [])
      in do (stm0, el) <- f exps
            return (stm0, build el)
    
    expl :: [Exp] -> ([Exp] -> Exp) -> Canon (Stm, Exp)
    expl el f = reorder doexp (el, f)

    expl' :: [Exp] -> ([Exp] -> Stm) -> Canon Stm
    expl' el f = do (stm0, s) <- reorder doexp (el, f)
                    return $ stm0 % s

    doexp :: Exp -> Canon (Stm, Exp)
    doexp (BINOP(p, a, b) isbptr) =
      expl [a, b] $ \[l, r] -> BINOP(p, l, r) isbptr
    doexp (CVTOP(p, a, s1, s2)) =
      expl [a] $ \[arg] -> CVTOP(p, arg, s1, s2)
    doexp (MEM(a, sz) ismemptr) =
      expl [a] $ \[arg] -> MEM(arg, sz) ismemptr
    doexp (ESEQ(s, e)) =
      do s' <- dostm s
         (s'', expr) <- expl [e] $ \[e'] -> e'
         return (s' % s'', expr)
    doexp (CALL(e, el) iscptr retlab) =
      expl (e:el) $ \(func:args) -> CALL(func, args) iscptr retlab
    doexp e = expl [] $ \[] -> e

    dostm :: Stm -> Canon Stm
    dostm (SEQ(a, b)) = do a' <- dostm a
                           b' <- dostm b
                           return $ a' % b'
    dostm (JUMP(e, labs)) = 
      expl' [e] $ \[e'] -> JUMP(e', labs)
    dostm (CJUMP(TEST(p, a, b), t, f)) =
      expl' [a, b] $ \[a', b'] -> CJUMP(TEST(p, a', b'), t, f)
    dostm (MOVE(TEMP t istptr, CALL(e, el) iscptr retlab)) =
      expl' (e:el) $ \(func:args) -> MOVE(TEMP t istptr, CALL(func, args) iscptr retlab)
    dostm (MOVE(TEMP t istptr, b)) =
      expl' [b] $ \[src] -> MOVE(TEMP t istptr, src)
    dostm (MOVE(MEM(e, sz) ismemptr, src)) =
      expl' [e, src] $ \[e', src'] -> MOVE(MEM(e', sz) ismemptr, src')
    dostm (MOVE(ESEQ(s, e), src)) =
      do s' <- dostm s
         src' <- dostm $ MOVE(e, src)
         return $ s' % src'
    dostm (MOVE(a, b)) =
      expl' [a, b] $ \[a', b'] -> MOVE(a', b')
    dostm (EXP(CALL(e, el) iscptr retlab)) =
      expl' (e:el) $ \(func:arg) -> EXP(CALL(func, arg) iscptr retlab)
    dostm (EXP e) =
      expl' [e] $ \[e'] -> EXP e'
    dostm s =
      expl' [] $ \[] -> s

    linear (SEQ(a, b)) l = linear a $ linear b l
    linear s l = s : l

  in do stm' <- dostm stmtobelinearized
        return $ linear stm' []

basicblocks :: [Stm] -> Canon ([[Stm]], Tmp.Label)
basicblocks stms0 =
  do done <- newLabel
     let blocks ((blkhead@(LABEL _)):blktail) blist =
           let next ((s@(JUMP _)):rest) thisblock =
                 endblock rest $ s:thisblock
               next ((s@(CJUMP _)):rest) thisblock =
                 endblock rest $ s:thisblock
               next (stms@(LABEL lab: _)) thisblock =
                 next (JUMP(NAME lab, [lab]):stms) thisblock
               next (s:rest) thisblock =
                 next rest $ s:thisblock
               next [] thisblock = next [JUMP(NAME done, [done])] thisblock

               endblock more thisblock =
                 blocks more $ reverse thisblock:blist
           in next blktail [blkhead]
         blocks [] blist = return $ reverse blist
         blocks stms blist =
           do newlab <- newLabel
              blocks (LABEL newlab:stms) blist
     blks <- blocks stms0 []
     return (blks, done)

enterblock :: [Stm] -> Map.Map Tmp.Label [Stm] -> Map.Map Tmp.Label [Stm]
enterblock b@(LABEL lab:_) table = Map.insert lab b table
enterblock _ table = table

splitlast :: [a] -> ([a], a)
splitlast [x] = ([], x)
splitlast (x:xs) = let (blktail, l) = splitlast xs in (x:blktail, l)
splitlast _ = error "Compiler error: splitlast."

trace :: Map.Map Tmp.Label [Stm] -> [Stm] -> [[Stm]] -> Canon [Stm]
trace table b@(LABEL lab0:_) rest =
  let table1 = Map.insert lab0 [] table
  in  case splitlast b of
        (most, JUMP(NAME lab, _)) ->
          case Map.lookup lab table1 of
            (Just b'@(_:_)) -> do t <- trace table1 b' rest
                                  return $ most ++ t
            _ -> do next <- getnext table1 rest
                    return $ b ++ next
        (most, CJUMP(TEST(opr, x, y), t, f)) ->
          case (Map.lookup t table1, Map.lookup f table1) of
            (_, Just b'@(_:_)) -> do tr <- trace table1 b' rest
                                     return $ b ++ tr
            (Just b'@(_:_), _) -> do tc <- trace table1 b' rest
                                     let cjump = [CJUMP(TEST(notrel opr, x, y), f, t)]
                                     return $ most ++ cjump ++ tc
            _ -> do f' <- newLabel
                    let cjump = [CJUMP(TEST(opr, x, y), t, f'), LABEL f',
                                 JUMP(NAME f, [f])]
                    n <- getnext table1 rest
                    return $ most ++ cjump ++ n
        (_, JUMP _) -> do next <- getnext table1 rest
                          return $ b ++ next
        _ -> error "Compiler error: trace -> case splitlast b of."
trace _ _ _ = error "Compiler error: trace."
          

getnext :: Map.Map Tmp.Label [Stm] -> [[Stm]] -> Canon [Stm]
getnext table (b@(LABEL lab:_):rest) =
  case Map.lookup lab table of
    Just (_:_) -> trace table b rest
    _ -> getnext table rest
getnext _ [] = return []
getnext _ _ = error "Compiler error: getnext."

tracesched :: ([[Stm]], Tmp.Label) -> Canon [Stm]
tracesched (blocks, done) =
  do n <- getnext (foldr enterblock Map.empty blocks) blocks
     return $ n ++ [LABEL done]

canonicalize :: Stm -> GenSymLabTmpState -> ([Stm], GenSymLabTmpState)
canonicalize stm state = let monad = do stms <- linearize stm
                                        blocks <- basicblocks stms
                                        tracesched blocks
                             result = (runIdentity . runGSLT state) monad
                         in  result
