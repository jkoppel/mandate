{-# LANGUAGE DataKinds, GADTs #-}

module Context (
    Frame(..)
  , Context(..)
  ) where


import Term
import Var


data Frame l v where
  TODO :: Frame l Open

data Context l v where
  Halt :: Context l v
  Push :: Frame l v -> Context l v -> Context l v
  KVar :: MetaVar -> Context l Open

{-
             data StepTo l where
              StepTo :: (Typeable l) => MConf l -> Rhs l -> StepTo l

             data NamedRule l = NamedRule {ruleName :: ByteString, getRule :: StepTo l}

             type ExtComp l = (CompFunc l, [MetaVar])
             type ExtCond l = (SideCond l, [MetaVar])

             --TODO: How to get rid of this vacuous Typeable instances?
             runExtComp :: (LangBase l) => ExtComp l -> Match (Term l Closed)
             runExtComp (f, vs) = getVars vs >>= runMatchEffect . runCompFunc f

             runExtCond :: (LangBase l) => ExtCond l -> Match Bool
             runExtCond (f, vs) = getVars vs >>= runMatchEffect . runSideCond f

             data Rhs l = Build (MConf l)
                        | SideCondition (ExtCond l) (Rhs l)
                        | LetStepTo (MConf l) (MConf l) (Rhs l) -- let (x,mu) = stepto(T,mu) in R
                        | LetComputation MetaVar (ExtComp l) (Rhs l) -- let x = f(T) in R

-}