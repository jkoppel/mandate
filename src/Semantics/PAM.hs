{-# LANGUAGE FlexibleContexts, GADTs, OverloadedStrings, StandaloneDeriving, UndecidableInstances #-}

-- | Implementation of "phased abstract machines," a transition system corresponding
--   to reduction (Felleisen-Hieb) semantics
--
-- Wait; nevermind. The "lockstep composition" rule (e1 || e2) -> (e1' || e2') is straightforward to implement in PAM,
-- but I don't see how to do it in reduction semantics. So....I guess then PAM is more powerful than Felleisen-Hieb.

module Semantics.PAM (
    PAMRule(..)
  , NamedPAMRule(..)
  , sosToPAM
  ) where

import Data.Maybe ( fromJust )
import Data.Monoid ( Monoid(..), )

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import Configuration
import Lang
import Matching
import Semantics.Context
import Semantics.General
import Semantics.SOS
import Term
import Var

data Phase = Up | Down
  deriving (Eq, Ord, Show)

data AMRhs payload l = AMLetComputation (Configuration l) (ExtComp l) (AMRhs payload l)
                     | AMRhs (payload l)

deriving instance (Show (Configuration l), Show (payload l), LangBase l) => Show (AMRhs payload l)


data PAMState l = PAMState { pamConf  :: Configuration l
                           , pamK     :: Context l
                           , pamPhase :: Phase
                           }

deriving instance (Show (Configuration l), Show (Context l)) => Show (PAMState l)

type PAMRhs = AMRhs PAMState

data PAMRule l = PAM { pamBefore :: PAMState l
                     , pamAfter  :: PAMRhs l
                     }

deriving instance (Show (PAMRhs l), Show (PAMState l)) => Show (PAMRule l)

data NamedPAMRule l = NamedPAMRule { pamRuleName :: ByteString
                                   , getPamRule  :: PAMRule l
                                   }

deriving instance (Show (PAMRule l)) => Show (NamedPAMRule l)

namePAMRule :: ByteString -> PAMRule l -> NamedPAMRule l
namePAMRule = NamedPAMRule

type NamedPAMRules l = [NamedPAMRule l]

type InfNameStream = [ByteString]

infNameStream :: ByteString -> InfNameStream
infNameStream nam = map (\i -> mconcat [nam, "-", BS.pack $ show i]) [1..]

splitFrame :: (Lang l) => PosFrame l -> Context l -> IO (PAMRhs l, Context l, Maybe (Configuration l, PosFrame l))
splitFrame (KBuild c) k = return (AMRhs $ PAMState c k Up, k, Nothing)
-- TODO: Why is this so ugly?
splitFrame (KStepTo c f@(KInp i pf)) k = fromJust <$> (runMatch $ do
                                         i' <- refreshVars i
                                         pf' <- fillMatch pf
                                         let f' = KInp i' pf'
                                         let cont = KPush f k
                                         let cont' = KPush (KInp i' pf') k
                                         return (AMRhs $ PAMState c cont Down, cont', Just (i, pf')))
splitFrame (KComputation comp (KInp c pf)) k = do (subRhs, ctx, rest) <- splitFrame pf k
                                                  return (AMLetComputation c comp subRhs, ctx, rest)

sosRuleToPAM' :: (Lang l) => InfNameStream -> PAMState l -> Context l -> PosFrame l -> IO [NamedPAMRule l]
sosRuleToPAM' (nm:nms) st k fr = do
    (rhs, k', frRest) <- splitFrame fr k
    restRules <- case frRest of
                   Nothing           -> return []
                   Just (conf', fr') -> sosRuleToPAM' nms (PAMState conf' k' Up) k fr'
    let rule = namePAMRule nm $ PAM st rhs
    return (rule : restRules)


sosRuleToPAM :: (Lang l) => NamedRule l -> IO [NamedPAMRule l]
sosRuleToPAM (NamedRule nam (StepTo conf rhs)) = do
  kv <- nextVar
  let startState = PAMState conf (KVar kv) Down
  sosRuleToPAM' (infNameStream nam) startState (KVar kv) (rhsToFrame rhs)


sosToPAM :: (Lang l) => NamedRules l -> IO (NamedPAMRules l)
sosToPAM rs = concat <$> mapM sosRuleToPAM rs