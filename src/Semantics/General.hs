{-# LANGUAGE DataKinds, FlexibleContexts #-}

module Semantics.General (
    MConf
  , ExtComp
  , ExtCond
  , runExtComp
  , runExtCond
  ) where

import Configuration
import LangBase
import Matching
import Term
import Var

-- Match configuration
type MConf l = Configuration l Open

type ExtComp l v = (CompFunc l, [Term l v])
type ExtCond l v = (SideCond l, [Term l v])

runExtComp :: (LangBase l) => ExtComp l Open -> Match Closed (Term l Closed)
runExtComp (f, ts) = mapM fillMatch ts >>= runMatchEffect . runCompFunc f

runExtCond :: (LangBase l) => ExtCond l Open -> Match Closed Bool
runExtCond (f, ts) = mapM fillMatch ts >>= runMatchEffect . runSideCond f