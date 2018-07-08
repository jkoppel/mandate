{-# LANGUAGE DataKinds, GADTs, OverloadedStrings #-}

-- | Implementation of "phased abstract machines," a transition system corresponding
--   to reduction (Felleisen-Hieb) semantics
--
-- Wait; nevermind. The "lockstep composition" rule (e1 || e2) -> (e1' || e2') is straightforward to implement in PAM,
-- but I don't see how to do it in reduction semantics. So....I guess then PAM is more powerful than Felleisen-Hieb.

module Semantics.PAM (
    PAMRule(..)
 -- , sosToPAM
  ) where

import Data.Monoid ( Monoid(..), )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import Configuration
import Lang
import Semantics.Context
import Semantics.General
import Semantics.SOS
import Term
import Var

data Phase = Up | Down

data AMRhs payload l = AMSideCondition  (ExtCond l Open) (AMRhs payload l)
                     | AMLetComputation MetaVar (ExtComp l Open) (AMRhs payload l)
                     | AMRhs (payload l)


data PAMState l = PAMState { pamConf :: MConf l
                           , pamK     ::Context l Open
                           , pamPhase :: Phase
                           }

type PAMRhs = AMRhs PAMState

data PAMRule l = PAM { pamBefore :: PAMState l
                     , pamAfter  :: PAMRhs l}

data NamedPAMRule l = NamedPAMRule { pamRuleName :: ByteString
                                   , getPamRule  :: PAMRule l
                                   }

namePAMRule :: ByteString -> PAMRule l -> NamedPAMRule l
namePAMRule = NamedPAMRule

type NamedPAMRules l = [NamedPAMRule l]

type InfNameStream = [ByteString]

infNameStream :: ByteString -> InfNameStream
infNameStream nam = map (\i -> mconcat [nam, "-", BS.pack $ show i]) [1..]

splitFrame :: PosFrame l Open -> Context l Open -> (PAMRhs l, Context l Open, Maybe (Frame l Open))
splitFrame (KBuild c) k = (AMRhs $ PAMState c k Up, k, Nothing)
splitFrame (KSideCond cond pf) k = let (subRhs, ctx, rest) = splitFrame pf k in
                                   (AMSideCondition cond subRhs, ctx, rest)
splitFrame (KStepTo c f) k = let cont = KPush f k in
                             (AMRhs $ PAMState c cont Down, cont, Just f)
splitFrame (KComputation comp (KInp (Conf (MetaVar mv) _) pf)) k = let (subRhs, ctx, rest) = splitFrame pf k in
                                                                   (AMLetComputation mv comp subRhs, ctx, rest)

sosRuleToPAM' :: InfNameStream -> PAMState l -> Context l Open -> PosFrame l Open -> IO [NamedPAMRule l]
sosRuleToPAM' (nm:nms) st k fr = do
    let (rhs, k', frRest) = splitFrame fr k
    restRules <- case frRest of
                   Nothing               -> return []
                   Just (KInp conf' fr') -> sosRuleToPAM' nms (PAMState conf' k' Up) k fr'
    let rule = namePAMRule nm $ PAM st rhs
    return (rule : restRules)


sosRuleToPAM :: (Lang l) => NamedRule l -> IO [NamedPAMRule l]
sosRuleToPAM (NamedRule nam (StepTo conf rhs)) = do
  kv <- nextVar
  let startState = PAMState conf (KVar kv) Down
  sosRuleToPAM' (infNameStream nam) startState (KVar kv) (rhsToFrame rhs)


sosToPAM :: (Lang l) => NamedRules l -> IO (NamedPAMRules l)
sosToPAM rs = concat <$> mapM sosRuleToPAM rs