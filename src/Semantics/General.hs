{-# LANGUAGE DataKinds, FlexibleContexts #-}

module Semantics.General (
    MConf
  , ExtComp
  , runExtComp
  ) where

import Configuration
import LangBase
import Matching
import Term
import Var

-- Match configuration
type MConf l = Configuration l Open

type ExtComp l v = (CompFunc l, [Term l v])

runExtComp :: (LangBase l) => ExtComp l Open -> Match Closed (Term l Closed)
runExtComp (f, ts) = mapM fillMatch ts >>= runMatchEffect . runCompFunc f