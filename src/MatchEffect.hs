{-# LANGUAGE  GeneralizedNewtypeDeriving #-}

module MatchEffect (
    MatchEffect(..) -- I'd like to only reveal the internals to the Match module
  , matchEffectInputChar
  , matchEffectInput
  , matchEffectOutput
  , matchEffectFlush
  ) where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Logic ( LogicT(..), runLogicT, observeAllT, lift )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

import System.IO ( hFlush, stdout )


-- | This file exists to break a circular dependency between Matching.hs and LangBase.hs
-- Ideally, it would be defined in Matching.hs

-- TODO: Actually, this isn't really about matching, and should not be called that.


-- | A monad indicating the effects that may run when executing a rule
--
-- This allows effects in a target language to be implemented using effects in the meta-language.
-- E.g.: I/O in the target language is implemented using Haskell's IO
newtype MatchEffect a = MatchEffect (LogicT IO a)
  deriving ( Functor, Applicative, Monad, MonadPlus, Alternative )

matchEffectInputChar :: MatchEffect Char
matchEffectInputChar = MatchEffect $ lift getChar

matchEffectInput :: MatchEffect ByteString
matchEffectInput = MatchEffect $ lift BS.getLine

matchEffectOutput :: ByteString -> MatchEffect ()
matchEffectOutput s = MatchEffect $ lift $ BS.putStr s

matchEffectFlush :: MatchEffect ()
matchEffectFlush = MatchEffect $ lift $ hFlush stdout