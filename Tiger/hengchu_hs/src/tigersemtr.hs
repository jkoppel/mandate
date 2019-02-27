module TigerSemTr
  ( SemantErrorClass(..)
  , SemantError(..)
  , Access
  , Level(..)
  , EnvEntry(..)
  , Venv
  , Tenv
  , SemTrState
  , LoopLevel
  , SemTr
  , symbol
  , name
  , runSemTr
  , genUniq
  , initialSemTrState
  , newLabel
  , newRetLabel
  , newTemp
  , namedLabel
  , Frag
  , getVenv
  , getTenv
  , getLoopLevel
  , getFragList
  , getUniqState
  , putVenv
  , putTenv
  , putLoopLevel
  , putFragList
  , putUniqState
  )
  where

import TigerLexer
import TigerSymbol
import TigerSemantTypes
import TigerTemp
import TigerFrame
import qualified TigerGenSymLabTmp as TGSLT
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

-- Types of errors semant can generate.
data SemantErrorClass = TypeMismatch String String
                      | TypeLoop     [String]
                      | NotCallable  String
                      | UndefinedBinop String String
                      | Undefined    String
                      | ArgumentCount Int Int
                      | ArgumentName String String
                      | BreakOutsideOfLoop
                      | DuplicateDefinition String
                      | NotVariable String
  deriving(Show, Eq)

-- The actual error type.
data SemantError = SE AlexPosn SemantErrorClass
  deriving(Show, Eq)

-- Translate types
type Access = (Level, Int)

data Level = LEVEL { levelFrame :: Frame
                   , staticLinkOffset :: Int
                   , levelParent :: Level
                   , levelUniq :: Uniq }
           | TOP
           deriving (Show, Eq)

-- These are the entries of the environment mappings during type checking.
data EnvEntry = VarEntry { varAccess :: Access
                         , varTy::Ty
                         , varReadOnly::Bool }
              | FunEntry { funLevel :: Level
                         , funLabel :: Label
                         , funFormals::[(Ty, Access)]
                         , funResult::Ty }
  deriving(Show)

-- Environment mappings.
type Venv = Map.Map Symbol EnvEntry
type Tenv = Map.Map Symbol Ty
type LoopLevel = Int

type SemTrState = ( Venv
                  , Tenv
                  , LoopLevel
                  , [Frag]
                  , Integer
                  )

getVenv :: SemTr Venv
getVenv = do (v, _, _, _, _) <- get
             return v

putVenv :: Venv -> SemTr ()
putVenv v = do (_, t, l, fs, u) <- get
               put (v, t, l, fs, u)

getTenv :: SemTr Tenv
getTenv = do (_, t, _, _, _) <- get
             return t

putTenv :: Tenv -> SemTr ()
putTenv t = do (v, _, l, fs, u) <- get
               put (v, t, l, fs, u)
               

getLoopLevel :: SemTr LoopLevel
getLoopLevel = do (_, _, l, _, _) <- get
                  return l

putLoopLevel :: LoopLevel -> SemTr () 
putLoopLevel l = do (v, t, _, fs, u) <- get
                    put (v, t, l, fs, u)

getFragList :: SemTr [Frag]
getFragList = do (_, _, _, f, _) <- get
                 return f

putFragList :: [Frag] -> SemTr ()
putFragList fs = do (v, t, l, _, u) <- get
                    put (v, t, l, fs, u)
                 

getUniqState :: SemTr Integer
getUniqState = do (_, _, _, _, u) <- get
                  return u

putUniqState :: Integer -> SemTr ()
putUniqState u = do (v, t, l, fs, _) <- get
                    put (v, t, l, fs, u)
                     

type SemTrT m = StateT SemTrState (ExceptT SemantError m)
type SemTr = SemTrT (TGSLT.GenSymLabTmp IO)

-- Repackage and export from lower level
symbol :: String -> SemTr Symbol
symbol = lift . lift . TGSLT.symbol

namedLabel :: String -> SemTr Symbol
namedLabel = symbol

name :: Symbol -> String
name = TGSLT.name

newLabel :: SemTr Label
newLabel = lift $ lift TGSLT.newLabel

newRetLabel :: SemTr RetLabel
newRetLabel = lift $ lift TGSLT.newRetLabel

newTemp :: Bool -> SemTr Temp
newTemp b = lift $ lift $ TGSLT.newTemp b

genUniq :: SemTr Integer
genUniq = do u <- getUniqState
             putUniqState $ u+1
             return u

initialSemTrState :: SemTrState
initialSemTrState = (Map.empty, Map.empty, 0, [], 0)

runSemTr :: SemTr a -> TGSLT.GenSymLabTmpState 
                    -> SemTrState 
                    -> IO (Either SemantError a,  TGSLT.GenSymLabTmpState)
runSemTr semtrmonad glststate semtrstate =
  let exceptmonad = evalStateT semtrmonad semtrstate
      glstmonad   = runExceptT exceptmonad
      result = TGSLT.runGSLT glststate glstmonad
  in  result
