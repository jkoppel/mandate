module TigerGenSymLabTmp
  (
    symbol
  , name
  , newLabel
  , newTemp
  , namedLabel
  , newRetLabel
  , prevRetLabel
  , GenSymLabTmp
  , GenSymLabTmpState
  , rlcount
  , tmap
  , runGSLT
  , initialGSLTState
  )
  where

import TigerSymbol
import TigerTemp
import qualified Data.Map as Map
import Data.List
import Control.Monad.State

data GenSymLabTmpState = GSLTSt 
                         { scount::Int
                         , tcount::Int  -- Temp count
                         , lcount::Int  -- Lab count
                         , rlcount::Int -- RLab count
                         , smap::Map.Map String Int -- Symbol map
                         , tmap::Map.Map Temp Bool -- Temp map of root pointers
                         }
type GenSymLabTmp m  = StateT GenSymLabTmpState m

initialGSLTState :: GenSymLabTmpState
initialGSLTState = GSLTSt 0 0 0 0 Map.empty Map.empty

getSymbolState :: (Monad m) => GenSymLabTmp m Int
getSymbolState = do st <- get
                    return $ scount st

putSymbolState :: (Monad m) => Int -> GenSymLabTmp m ()
putSymbolState s = do st <- get
                      put st{scount=s}

getTempState :: (Monad m) => GenSymLabTmp m Int
getTempState = do st <- get
                  return $ tcount st

putTempState :: (Monad m) => Int -> GenSymLabTmp m ()
putTempState t = do st <- get
                    put $ st{tcount=t}

getLabelState :: (Monad m) => GenSymLabTmp m Int
getLabelState = do st <- get
                   return $ lcount st

putLabelState :: (Monad m) => Int -> GenSymLabTmp m ()
putLabelState l = do st <- get
                     put $ st{lcount=l}

getRetLabelState :: (Monad m) => GenSymLabTmp m Int
getRetLabelState = do st <- get
                      return $ rlcount st

putRetLabelState :: (Monad m) => Int -> GenSymLabTmp m ()
putRetLabelState r = do st <- get
                        put $ st{rlcount=r}

getSymbolState2 :: (Monad m) => GenSymLabTmp m (Map.Map String Int)
getSymbolState2 = do st <- get
                     return $ smap st

putSymbolState2 :: (Monad m) => Map.Map String Int -> GenSymLabTmp m ()
putSymbolState2 m = do st <- get
                       put $ st{smap=m}

getTempMapState :: (Monad m) => GenSymLabTmp m (Map.Map Temp Bool)
getTempMapState = do st <- get
                     return $ tmap st

putTempMapState :: (Monad m) => Map.Map Temp Bool -> GenSymLabTmp m ()
putTempMapState m = do st <- get
                       put $ st{tmap=m}

symbol :: (Monad m) => String -> GenSymLabTmp m Symbol
symbol str = do m <- getSymbolState2
                s <- getSymbolState
                case Map.lookup str m of
                  Just c -> return (str, c)
                  Nothing -> do let map' = Map.insert str s m
                                putSymbolState $ s+1
                                putSymbolState2 map'
                                return (str, s)

namedLabel :: (Monad m) => String -> GenSymLabTmp m Label
namedLabel = symbol

name :: Symbol -> String
name = fst

newLabel :: (Monad m) => GenSymLabTmp m Label
newLabel = do l <- getLabelState
              lab <- symbol ("L"++show l)
              putLabelState $ l+1
              return lab

retLabelPrefix :: String
retLabelPrefix = "RET"

newRetLabel :: (Monad m) => GenSymLabTmp m RetLabel
newRetLabel = do r <- getRetLabelState
                 lab <- symbol (retLabelPrefix++show r)
                 putRetLabelState $ r+1
                 return lab

prevRetLabel :: RetLabel -> Maybe String
prevRetLabel lab =
  if retLabelPrefix `isPrefixOf` name lab
     then let numpart = name lab \\ retLabelPrefix
              num' = read numpart - 1 :: Int
              prev = retLabelPrefix++show num'
          in  if num' >= 0
                 then Just prev
                 else Nothing
     else Nothing

newTemp :: (Monad m) => Bool -> GenSymLabTmp m Temp
newTemp ispointer = do t <- getTempState
                       putTempState $ t+1
                       m <- getTempMapState
                       putTempMapState $ Map.insert (TEMP t) ispointer m
                       return $ TEMP t

runGSLT :: GenSymLabTmpState -> GenSymLabTmp m a -> m (a, GenSymLabTmpState)
runGSLT st m = runStateT m st
