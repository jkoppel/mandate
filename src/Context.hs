module Context (
  ) where


import Term


-- Context types
-- Context values

data Frame = TODO

data Context = Halt
             | Push Context Frame