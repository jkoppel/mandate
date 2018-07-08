{-# LANGUAGE  GeneralizedNewtypeDeriving #-}

module MatchEffect (
    MatchEffect(..) -- I'd like to only reveal the internals to the Match module
  , matchEffectInput
  , matchEffectOutput
  ) where

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS


-- This file exists to break a circular dependency

newtype MatchEffect a = MatchEffect (IO a)
  deriving ( Functor, Applicative, Monad )

matchEffectInput :: MatchEffect ByteString
matchEffectInput = MatchEffect BS.getLine

matchEffectOutput :: ByteString -> MatchEffect ()
matchEffectOutput s = MatchEffect $ BS.putStr s