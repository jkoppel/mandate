{-# LANGUAGE DataKinds #-}

module Matching (
    OpenClosed(..)
  , Matchable
  ) where

data OpenClosed = Closed | Open


class Matchable f where

instance Matchable ()

-- Matchable instances for Term, Context, Map