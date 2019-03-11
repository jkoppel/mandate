module TigerRegisters
  (
    Register(..)
  , npseudoregs
  , localbaseoffset
  , parambaseoffset
  , availregs
  )
  where

data Register = EAX | EBX | ECX | EDX | EBP | ESP | ESI | EDI | ZERO
                    | PSEUDO Int
                deriving (Eq, Ord)

npseudoregs :: Int
npseudoregs = 200

localbaseoffset :: Int
localbaseoffset = -4 - 4 * npseudoregs

parambaseoffset :: Int
parambaseoffset = 4

availregs :: [Register]
availregs = [EAX, EBX, ESI, EDI] ++ (map PSEUDO [1..npseudoregs])

instance Show Register where
  show EAX = "%eax"
  show EBX = "%ebx"
  show ECX = "%ecx"
  show EDX = "%edx"
  show EBP = "%ebp"
  show ESP = "%esp"
  show ESI = "%esi"
  show EDI = "%edi"
  show ZERO = "zero"
  show (PSEUDO d) =show (-4*d)++"(%ebp)"
