module Jade.TopLevel where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Graph as G
import qualified Jade.Decode as D
import qualified Jade.Module as Module
import Jade.Types
import Jade.Wire

-- |Get a list of pairs (modulename, module)
modules :: TopLevel -> [(String, Module)]
modules (TopLevel m) = DM.toList m

-- |Get a module from a TopLevel given a module name
getModule :: TopLevel -> String -> Maybe Module
getModule (TopLevel m) name = DM.lookup name m


-- |Transform a located terminal to a degenerate edge. A located
-- terminal is one that has been placed in a schematic, with absolute coordinate.
termToEdge t@(Terminal (Coord3 x y _) _) =
  let n = G.Node (x, y) (TermC t)
  in G.Edge n n


-- |Get the components of a module given the module's name.
components topl modname = 
  case getModule topl modname of
    Just (Module (Just (Schematic comps)) _ _) ->
      let wires = [w | WireC w <- DV.toList comps]
          ports = [p | PortC p <- DV.toList comps]
          terms = concat [terminals topl submod | SubModuleC submod <- DV.toList comps]
          
          wireEdges = map wireToEdge wires
          portEdges = map portToEdge ports -- these are degenerate edges.
          termEdges = map termToEdge terms
          
          edges = wireEdges ++ portEdges ++ termEdges
      
      in Right $ G.components $ G.fromEdges edges
    Nothing -> Left $ "Couldn't find module: " ++ modname

-- | Get a list of terminals in a submodule offset by the position of the submodule  
terminals :: TopLevel -> SubModule -> [Terminal]
terminals topl (SubModule modname offset)  =
  case getModule topl modname of
    Just mod -> Module.terminals mod offset
    Nothing -> []


numComponents topl modname =
  case components topl modname of
    Left msg -> Left $ "Couldn't get number of componenents\n " ++ msg
    Right set -> Right $ DS.size set
