module GraphPattern.Internals (
    funName
  ) where

import Language.Haskell.TH ( Name, Q, Exp, reify, stringE )

funName :: Name -> Q Exp
funName nm = stringE . show =<< reify nm