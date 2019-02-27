import qualified TigerSemant as TS
import qualified TigerSemTr  as TSt
import qualified TigerLexer  as TLex
import qualified TigerParser as TP
import qualified TigerGenSymLabTmp as TGSLT
import qualified TigerCodeGen as CG

import TigerFlow
import TigerITree
import TigerFrame
import TigerCanon
import TigerInterference
import TigerRegisters
import TigerTemp
import TigerColor
import TigerAssem

import qualified Data.Map as Map

import Control.Monad
import System.Environment

isdata :: Frag -> Bool
isdata (DATA _ _) = True
isdata (PROC _ _ _) = False
isdata (GCDESCARR _ _) = True
isdata (GCDESCREC _ _) = True

isproc :: Frag -> Bool
isproc = not . isdata

codegenfoldhelper :: Stm -> ([Instr], TGSLT.GenSymLabTmpState)
                         -> ([Instr], TGSLT.GenSymLabTmpState)
codegenfoldhelper stm (instrs, state) = let (instrs', state') = CG.codegen stm state
                                        in  (instrs ++ instrs', state')

initialcoloring :: Map.Map Temp Register
initialcoloring = Map.fromList [(Named EAX, EAX)
                               ,(Named EBX, EBX)
                               ,(Named ECX, ECX)
                               ,(Named EDX, EDX)
                               ,(Named EBP, EBP)
                               ,(Named ESP, ESP)
                               ,(Named ESI, ESI)
                               ,(Named EDI, EDI)]

outputfrag :: Frag -> TGSLT.GenSymLabTmpState -> IO (TGSLT.GenSymLabTmpState)
outputfrag (GCDESCARR lab bool) st = do let instrs = CG.arraydescriptordata lab bool
                                        let output = map (flip instrfmt Map.empty) instrs
                                        mapM_ putStrLn output
                                        return st
outputfrag (GCDESCREC lab str) st = do let instrs = CG.recorddescriptordata lab str
                                       let output = map (flip instrfmt Map.empty) instrs
                                       mapM_ putStrLn output
                                       return st
outputfrag (DATA lab str) st = do let instrs = CG.stringdata lab str
                                  let output = map (flip instrfmt Map.empty) instrs
                                  mapM_ putStrLn output
                                  return st
outputfrag (PROC lab stm frame) st = do let (stms, newst) = canonicalize stm st
                                        let (instrs, newst2) = foldl (flip codegenfoldhelper) ([], newst) stms
                                        let (flowgraph, nodes) = instrs2graph instrs
                                        let (intergraph, node2templist) = interferenceGraph flowgraph
                                        --let dotfile = graph2dotfile (TGSLT.name lab) (graph intergraph) False
                                        let regalloc = color intergraph initialcoloring availregs
                                        let livetemps = map node2templist nodes
                                        let body = zip instrs livetemps
                                        let tempmap = TGSLT.tmap newst2
                                        instrs2 <- CG.procEntryExit lab body regalloc [] frame tempmap
                                        let output = map (flip instrfmt regalloc) instrs2
                                        mapM_ putStrLn output
                                        return newst2

main :: IO ()
main = do args <- getArgs
          let file = args !! 0
          fileContent <- readFile file
          let eithertoken = TLex.scanner fileContent
          case eithertoken of
            Left lexerr -> print lexerr
            Right toks -> do let ptoks = map TP.token2ptoken toks
                             let parseres = TP.runParser TP.parser file ptoks
                             case parseres of
                               Left parserr -> print parserr
                               Right (prog, gsltstate) -> do (semantres, gsltstate2) <- TSt.runSemTr (TS.transprog prog) gsltstate TSt.initialSemTrState
                                                             case semantres of
                                                               Left semanterr -> print semanterr
                                                               Right frags    -> do let datafrags = filter isdata frags
                                                                                    let procfrags = filter isproc frags
                                                                                    --mapM_ putStrLn (map prettyprintfrag procfrags)
                                                                                    glst <- foldM (flip outputfrag) gsltstate2 datafrags
                                                                                    glst2 <- foldM (flip outputfrag) glst procfrags
                                                                                    let lastrlabnum = TGSLT.rlcount glst2-1
                                                                                    let lastrlab = "RET"++show lastrlabnum
                                                                                    putStrLn ".global GCINITHEAD"
                                                                                    putStrLn $ "GCINITHEAD: .4byte " ++ lastrlab ++ "PTRMAP"
