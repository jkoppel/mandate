module TigerColor
  (
    color
  , Allocation
  )
  where

import TigerInterference
import TigerTemp
import TigerRegisters
import qualified TigerGraph as Gr
import qualified Data.Map as Map
import Data.List
import Data.Maybe

type Allocation = Map.Map Temp Register

reducelist :: [Maybe a] -> [a]
reducelist xs = reducelist' xs []
  where reducelist' [] list = list
        reducelist' (a:as) list = case a of
                                    Nothing -> reducelist' as list
                                    Just a' -> reducelist' as (a':list)

canreduce :: IGraph -> Allocation -> Int -> Maybe (Gr.Node Temp)
canreduce (IGRAPH gr tn _ _) coloring deg = 
  let ns = Gr.nodes gr
      coloredtemps = Map.keys coloring
      coloredunodes = reducelist $ map (flip Map.lookup tn) coloredtemps
      colorednodes = map (fromJust . flip Gr.labeledNode gr) coloredunodes
      rest = ns \\ colorednodes
      degree n = length $ Gr.adj n gr
  in  case rest of
        [] -> Nothing
        xs -> case find (\n -> degree n < deg) xs of
                Just n' -> Just n'
                Nothing -> Nothing

allcolored :: IGraph -> Allocation -> Bool
allcolored (IGRAPH gr _ _ _) coloring =
  let ns = Gr.nodes gr
      temps = map snd ns
  in  all (\t -> Map.member t coloring) temps

getcolor :: (Gr.Node Temp) -> Allocation -> Maybe Register
getcolor n coloring = Map.lookup (snd n) coloring

color :: IGraph -> Allocation -> [Register] -> Allocation
color ig@(IGRAPH gr tn gt mvs) initial colors =
  case canreduce ig initial $ length colors of
    Nothing -> if allcolored ig initial then initial else error "Compiler error: Not enough colors for register allocation."
    Just n -> let un = fst n
                  neighborcolors = reducelist $ map (flip getcolor initial) $ Gr.adj n gr
                  availcolors = colors \\ neighborcolors
                  g' = Gr.rmNode un gr
                  coloring = color (IGRAPH g' tn gt mvs) initial $ tail availcolors
              in  Map.insert (snd n) (head availcolors) coloring
