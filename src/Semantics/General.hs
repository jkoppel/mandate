{-# LANGUAGE FlexibleContexts #-}

module Semantics.General (
    ExtComp
  , runExtComp
  , fillMatchExtComp
  , matchExtComp
  ) where

import Control.Monad ( guard )

import LangBase
import Matching
import Term

type ExtComp l = (CompFunc l, [Term l])

runExtComp :: (LangBase l) => ExtComp l -> Match (Term l)
runExtComp (f, ts) = mapM fillMatch ts >>= runMatchEffect . runCompFunc f

fillMatchExtComp :: (LangBase l, MonadMatchable m) => ExtComp l -> m (ExtComp l)
fillMatchExtComp (f, ts) = mapM fillMatch ts >>= \ts' -> return (f, ts')

matchExtComp :: (LangBase l, MonadMatchable m) => Pattern (ExtComp l) -> Matchee (ExtComp l) -> m ()
matchExtComp (Pattern (f1, ts1)) (Matchee (f2, ts2)) = do
  guard (f1 == f2)
  guard (length ts1 == length ts2)
  matchList (Pattern ts1) (Matchee ts2)