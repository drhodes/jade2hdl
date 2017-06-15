module Jade.Module where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Graph as G
import qualified Jade.Decode as D
import Jade.Types
import Jade.Wire

testConnected = do
  Right (TopLevel m) <- D.decodeTopLevel "./test-data/fan5rot4connect.json"
  case DM.elems m of
    [Module (Just (Schematic wirecs)) _ _] -> 
      let wires = [w | WireC w <- DV.toList wirecs]
          edges = map wireToEdge wires
          g = G.fromEdges edges
      in return g
    x -> fail "No schematic found in Module.testConnected"

terminals :: Module -> Coord3 -> [Terminal]
terminals (Module _ _ icon) offset@(Coord3 dx dy _) =
  case icon of
    Nothing -> []
    Just (Icon parts) ->
      [Terminal (Coord3 (x+dx) (y+dy) r) sig | IconTerm (Terminal (Coord3 x y r) sig) <- parts]

-- components (Module (Just (Schematic comps)) _ _) =
--   let wires = [w | WireC w <- DV.toList comps]
--       ports = [p | PortC p <- DV.toList comps]
--       --terms = [p | Module p <- DV.toList comps]
      
--       wireEdges = map wireToEdge wires
--       portEdges = map portToEdge ports
--       edges = wireEdges ++ portEdges
--   in G.components $ G.fromEdges edges


-- terminals :: SubModule -> [TermC]
