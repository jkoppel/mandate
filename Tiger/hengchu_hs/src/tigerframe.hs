module TigerFrame
       ( 
         Offset
       , Frame(..)
       , Frag(..)
       , newFrame
       , allocLocalInFrame
       , prettyprintfrag
       )
       where

import Data.IORef
import TigerITree
import TigerTemp
import TigerGenSymLabTmp

type Offset = Int
data Frame  = Frame { frameFormals     :: Int
                     ,frameOfflist     :: [Offset]
                     ,frameLocalCount  :: IORef Int -- Count of local variables in frame
                     ,frameLocalIsPtr  :: IORef [(Int, Bool)]
                     ,frameMaxArgs     :: IORef Int
                    }
                    deriving (Eq)

instance Show Frame where
  show _ = "<frame>"

data Frag = PROC { procName  :: Label
                 , procBody  :: Stm
                 , procFrame :: Frame }
          | DATA { dataLab :: Label
                 , dataStr :: String }
          | GCDESCREC { descLab :: Label
                      , descStr :: String }
          | GCDESCARR { descLab :: Label 
                      , descIsPtr :: Bool }
          deriving (Show, Eq)

prettyprintfrag :: Frag -> String
prettyprintfrag (PROC n body _) = let fname = name n
                                      fbody = prettyprintstm body
                                  in  fname ++ ":\n" ++ fbody
prettyprintfrag (DATA n str) = let fname = name n
                               in  fname ++ ":\n" ++ show str
prettyprintfrag _ = ""

newFrame :: Int -> IO (Frame, [Offset])
newFrame numFormals = 
  do localRef <- newIORef 0
     maxargRef <- newIORef 0
     localIsPtrRef <- newIORef []
     return (Frame { frameFormals=numFormals
                   , frameOfflist=offlist
                   , frameLocalCount=localRef
                   , frameLocalIsPtr=localIsPtrRef
                   , frameMaxArgs=maxargRef }, offlist)
       where offlist = offsetListForNumFormals numFormals []
             offsetListForNumFormals 0 l  = reverse l
             offsetListForNumFormals n [] = offsetListForNumFormals (n-1) [4]
             offsetListForNumFormals n l@(x:_) = offsetListForNumFormals (n-1) ((x+4):l)

allocLocalInFrame :: Bool -> Frame -> IO Offset
allocLocalInFrame isptr Frame { frameLocalCount=locals, frameLocalIsPtr=isPtrRef } = 
  do c <- readIORef locals
     writeIORef locals (c+1)
     let off = c * (-4)
     isptrs <- readIORef isPtrRef
     writeIORef isPtrRef $ (off, isptr):isptrs
     return off
