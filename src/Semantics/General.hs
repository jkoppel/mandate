module Semantics.General (
    ExtComp
  , runExtComp
  , getVarsExtComp
  , matchExtComp
  , refreshVarsExtComp
  , fillMatchExtComp

  , showsPrecExtComp
  , showRules
  ) where

import Control.Monad ( guard )
import Data.Foldable ( fold )
import Data.List ( intersperse )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Set ( Set )
import qualified Data.Set as Set

import Configuration
import LangBase
import Matching
import Term
import Var

-- TODO: This should just be a real data structure which implements Matchable already

type ExtComp l = (CompFunc l, [Term l])

runExtComp :: (LangBase l) => ExtComp l -> Match (Configuration l)
runExtComp (f, ts) = do ts' <- fillMatchList ts
                        runMatchEffect $ runCompFunc f ts'

getVarsExtComp :: (LangBase l) => ExtComp l -> Set MetaVar
getVarsExtComp (f, ts) = fold (map getVars ts)

matchExtComp :: (LangBase l, MonadMatchable m) => Pattern (ExtComp l) -> Matchee (ExtComp l) -> m ()
matchExtComp (Pattern (f1, ts1)) (Matchee (f2, ts2)) = do
  guard (f1 == f2)
  guard (length ts1 == length ts2)
  matchList (Pattern ts1) (Matchee ts2)

fillMatchExtComp :: (LangBase l, MonadMatchable m) => ExtComp l -> m (ExtComp l)
fillMatchExtComp (f, ts) = mapM fillMatch ts >>= \ts' -> return (f, ts')

refreshVarsExtComp :: (LangBase l, MonadMatchable m) => ExtComp l -> m (ExtComp l)
refreshVarsExtComp (f, ts) = refreshVarsList ts >>= \ts' -> return (f, ts')

showsPrecExtComp :: (LangBase l) => Int -> ExtComp l -> ShowS
showsPrecExtComp d (f, ts) = showString (BS.unpack $ compFuncName f) . showsPrec (d+1) ts


---------------------------------------------------------------------

showRules :: (Show a) => [a] -> ShowS
showRules rs = showString "Begin Rules:\n\n" .
               foldr (.) id (intersperse (showString "\n\n") $ map (showsPrec 0) rs) .
               showString "\nEnd Rules"