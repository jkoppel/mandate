module Rose (
    Rose(..)
  , addDepth
  , happyPath
  ) where


-- | A rose tree is a tree where each node may have an arbitrary number of children.
-- Used for representing the execution of a nondeterministic transition system.
data Rose a = Rose { roseGetElt :: a, roseGetChildren ::  [Rose a] }

-- | Tags every internal node of a rose tree with the length of the longest path
-- rooted at that node
addDepth :: Rose a -> Rose (a, Int)
addDepth (Rose a rs) = Rose (a, 1 + maximum ([-1] ++ map (snd.roseGetElt) rs')) rs'
  where
    rs' = map addDepth rs

-- | Returns a maximum-length path in a rose tree
--
-- Its motivating use: When converting an SOS rule to PAM, many deterministic executions
-- become nondeterministic: even if only one SOS rule matches a term, multiple PAM rules may,
-- as matching one SOS rule corresponds to matching multiple PAM rules in sequence.
-- However, all branches save the intended match will be dead ends, i.e.: not the longest path.
happyPath :: Rose a -> [a]
happyPath rose = go (addDepth rose)
  where
    go (Rose (a, 0) []) = [a]
    go (Rose (a, d) rs) = a : (go $ head $ filter (\t -> (snd $ roseGetElt t) == d-1) rs)

instance Show a => Show (Rose a) where
  showsPrec d (Rose a rs) = showString (concat $ replicate d "--") . showsPrec 0 a . showString "\n" .
                            foldr (.) id (map (showsPrec (d+1)) rs)