module Lattice (
    Meetable(..)
  , meetDefault

  , UpperBound(..)
  , upperBoundDefault

  , maximalElts
  ) where


class (Eq m) => Meetable m where
  meet :: m -> m -> Maybe m

  prec :: m -> m -> Bool
  prec a b = (a `meet` b) == Just a

  -- | isMinimal x = true iff  there is no y such that y /= x && (y `prec` x)
  -- In our usage, this corresponds to non-abstract terms
  isMinimal :: m -> Bool

class (Meetable m) => UpperBound m where
  top :: m

  -- | If z = x `upperBound` y, then x < z and y < z
  -- Not necessarily a join; the relevannt poset may not have least upper-bounds
  -- (Namely, maps do not)
  upperBound  :: m -> m -> m


instance Meetable () where
  meet () () = Just ()
  isMinimal _ = True

instance (Meetable a, Meetable b) => Meetable (a, b) where
  meet (a1, b1) (a2, b2) = (,) <$> (a1 `meet` a2) <*> (b1 `meet` b2)
  prec (a1, b1) (a2, b2) = (a1 `prec` a2) && (b1 `prec` b2)
  isMinimal (a, b) = isMinimal a && isMinimal b


instance (UpperBound a, UpperBound b) => UpperBound (a, b) where
  top = (top, top)
  upperBound (a1, b1) (a2, b2) = (a1 `upperBound` a2, b1 `upperBound` b2)


-- WARNING:
-- Most of the time, this will not actually yield a definition
-- of meet which is correct wrt the relevant ordering. (It is correct,
-- however, wrt the identity ordering.)
--
-- Meet is only used in nonlinear abstract pattern matching,
-- so this may not actually matter
meetDefault :: (Eq m) => m -> m -> Maybe m
meetDefault a b = if a == b then Just a else Nothing


upperBoundDefault :: (Eq m, UpperBound m) => m -> m -> m
upperBoundDefault a b = if a == b then a else top


-- Naive n^2
maximalElts :: (Meetable a) => [a] -> [a]
maximalElts []     = []
maximalElts (x:xs) = if any (x `prec`) xs then
                       maximalElts xs
                     else
                       x : maximalElts xs