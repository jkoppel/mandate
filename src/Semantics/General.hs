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

type ExtComp l = (CompFunc l, [Term l Open])
type ExtCond l = (SideCond l, [Term l Open])

runExtComp :: (LangBase l) => ExtComp l -> Match (Term l Closed)
runExtComp (f, ts) = mapM fillMatch ts >>= runMatchEffect . runCompFunc f

runExtCond :: (LangBase l) => ExtCond l -> Match Bool
runExtCond (f, ts) = mapM fillMatch ts >>= runMatchEffect . runSideCond f