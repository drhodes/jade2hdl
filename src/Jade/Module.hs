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

components (Module (Just (Schematic comps)) _ _) =
  let wires = [w | WireC w <- DV.toList comps]
      ports = [p | PortC p <- DV.toList comps]
      
      wireEdges = map wireToEdge wires
      portEdges = map portToEdge ports
      edges = wireEdges ++ portEdges
  in G.components $ G.fromEdges edges
