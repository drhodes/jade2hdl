module Jade.Module where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Graph as G
import qualified Jade.Decode as D
import Jade.Types
import Jade.Wire

-- data TopLevel = TopLevel (DM.Map String (Maybe Module))
--               deriving  (Show, Eq)

--testConnected :: IO (G.Graph (Integer, Integer))
testConnected = do
  Right (TopLevel m) <- D.decodeTopLevel "./test-data/fan5rot4connect.json"
  let [Module (Schematic wirecs) _] = DM.elems m
  let wires = [w | WireC w <- DV.toList wirecs]
  let edges = map wireToEdge wires
  let g = G.fromEdges edges
  return g
