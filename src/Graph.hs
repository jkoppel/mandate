module Graph (
    Graph
  , empty
  , insert
  , member
  ) where


import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import Data.Hashable ( Hashable(..) )

newtype Graph a = Graph { getGraph :: HashMap a (HashSet a) }

instance Show a => Show (Graph a) where
  showsPrec d (Graph edgeMap) = M.foldrWithKey showsPrecNode id edgeMap
    where
      showsPrecNode n es s = showString "Node: " . showsPrec d n . showString "\n" .
                             showString "Edges: \n" . S.foldr showsTarg id es .
                             showString "\n\n" . s

      showsTarg e s = showString "--" . showsPrec d e . showString "\n" . s


empty :: Graph a
empty = Graph M.empty

insert :: (Eq a, Hashable a) => a -> a -> Graph a -> Graph a
insert x y = Graph . M.insertWith S.union x (S.singleton y) . getGraph

member :: (Eq a, Hashable a) => a -> Graph a -> Bool
member a = M.member a . getGraph

