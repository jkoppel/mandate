{-# LANGUAGE DataKinds, OverloadedStrings #-}

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
import LangBase
import Semantics.Context
import Semantics.General
import Semantics.SOS
import Var

data Phase = Up | Down

data AMRhs payload l = AMSideCondition  (ExtCond l Open) (AMRhs payload l)
                     | AMLetComputation MetaVar (ExtComp l Open) (AMRhs payload l)
                     | AMRhs (payload l)


data PAMState l = PAMState (MConf l) (Context l Open) (Phase)
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

{-
data SplitRhs l = SplitRhs (PAMRhs l) (PAMState l)  (Rhs l)
                | DoneSplit (PAMRhs l)

splitRhs :: Rhs l -> Context l -> SplitRhs l
splitRhs (Build conf) k = DoneSplit $ AMRhs (conf, k, Up)
--splitRhs (SideCondition )
splitRhs (LetStepTo vConf conf' rhs') = SplitRhs (AMRhs (, rhsToContext k rhs', Down)) () rhs'

data Rhs l = Build (MConf l)
           | SideCondition (ExtCond l) (Rhs l)
           | LetStepTo (MConf l) (MConf l) (Rhs l) -- let (x,mu) = stepto(T,mu) in R
           | LetComputation MetaVar (ExtComp l) (Rhs l) -- let x = f(T) in R


sosRuleToPAM' :: InfNameStream -> PAMState l -> RHS l -> IO [NamedPAMRule l]
sosRuleToPAM' (nm:nms) st rhs =
  case splitRhs rhs of
    SplitRhs pamRhs st' rhs' -> do
      kv1 <- nextVar
      kv2 <- nextVar
      let pr = namePAMRule nm $ PAMRule st pamRhs
      rest <- sosRuleToPAM nms st' rhs'
      return $ pr : rest
    DoneSplit pamRhs -> return [PAMRule st pamRHS]

sosRuleToPAM :: NamedRule l -> IO [NamedPAMRule l]
sosRuleToPAM (NamedRule nam (StepTo conf rhs)) = sosRuleToPAM' (infNameStream nam) <$> startState <*> return rhs
  where
    startState = nextVar >>= \kv -> return (conf, KVar kv, Down)


sosToPAM :: NamedRules l -> IO (NamedPAMRules l)
sosToPAM rs = concat <$> mapM sosRuleToPAM rs
-}