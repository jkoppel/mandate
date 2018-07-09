module Rose (
    Rose(..)
  , addDepth
  , happyPath
  ) where


data Rose a = Rose { roseGetElt :: a, roseGetChildren ::  [Rose a] }

addDepth :: Rose a -> Rose (a, Int)
addDepth (Rose a rs) = Rose (a, 1 + maximum ([-1] ++ map (snd.roseGetElt) rs')) rs'
  where
    rs' = map addDepth rs

happyPath :: Rose a -> [a]
happyPath rose = go (addDepth rose)
  where
    go (Rose (a, 0) []) = [a]
    go (Rose (a, d) rs) = a : (go $ head $ filter (\t -> (snd $ roseGetElt t) == d-1) rs)

instance Show a => Show (Rose a) where
  showsPrec d (Rose a rs) = showString (concat $ replicate d "--") . showsPrec 0 a . showString "\n" .
                            foldr (.) id (map (showsPrec (d+1)) rs)