{-# LANGUAGE FlexibleContexts #-}

module Semantics.General (
    ExtComp
  , runExtComp
  , matchExtComp
  , refreshVarsExtComp
  , fillMatchExtComp
  ) where

import Control.Monad ( guard )

import Configuration
import LangBase
import Matching
import Term

type ExtComp l = (CompFunc l, [Term l])

runExtComp :: (LangBase l) => ExtComp l -> Match (Configuration l)
runExtComp (f, ts) = do ts' <- fillMatchList ts
                        runMatchEffect $ runCompFunc f ts'

matchExtComp :: (LangBase l, MonadMatchable m) => Pattern (ExtComp l) -> Matchee (ExtComp l) -> m ()
matchExtComp (Pattern (f1, ts1)) (Matchee (f2, ts2)) = do
  guard (f1 == f2)
  guard (length ts1 == length ts2)
  matchList (Pattern ts1) (Matchee ts2)

fillMatchExtComp :: (LangBase l, MonadMatchable m) => ExtComp l -> m (ExtComp l)
fillMatchExtComp (f, ts) = mapM fillMatch ts >>= \ts' -> return (f, ts')

refreshVarsExtComp :: (LangBase l, MonadMatchable m) => ExtComp l -> m (ExtComp l)
refreshVarsExtComp (f, ts) = refreshVarsList ts >>= \ts' -> return (f, ts')