{-# LANGUAGE  GeneralizedNewtypeDeriving #-}

module MatchEffect (
    MatchEffect(..) -- I'd like to only reveal the internals to the Match module
  , matchEffectInput
  , matchEffectOutput
  ) where

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS


-- | This file exists to break a circular dependency between Matching.hs and LangBase.hs
-- Ideally, it would be defined in Matching.hs

-- TODO: Actually, this isn't really about matching, and should not be called that.


-- | A monad indicating the effects that may run when executing a rule
--
-- This allows effects in a target language to be implemented using effects in the meta-language.
-- E.g.: I/O in the target language is implemented using Haskell's IO
newtype MatchEffect a = MatchEffect (IO a)
  deriving ( Functor, Applicative, Monad )

matchEffectInput :: MatchEffect ByteString
matchEffectInput = MatchEffect BS.getLine

matchEffectOutput :: ByteString -> MatchEffect ()
matchEffectOutput s = MatchEffect $ BS.putStr s