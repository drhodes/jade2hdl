module Jade.TopLevel where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Graph as G
import qualified Jade.Decode as D
import qualified Jade.Module as Module
import Jade.Types
import Jade.Wire
import Control.Monad

-- |Get a list of pairs (modulename, module)
modules :: TopLevel -> [(String, Module)]
modules (TopLevel m) = DM.toList m

-- |Get a module from a TopLevel given a module name
getModule :: TopLevel -> String -> J Module
getModule (TopLevel m) name =
  case DM.lookup name m of
    Just mod -> return mod
    Nothing -> die $ "TopLevel.getModule couldn't find module:" ++ name   

-- |Transform a located terminal to a degenerate edge. A located
-- terminal is one that has been placed in a schematic, with absolute coordinate.
termToEdge t@(Terminal (Coord3 x y _) _) =
  let n = Node (x, y) (TermC t)
  in Edge n n

-- |Get the components of a module given the module's name.
components topl modname = do
  --let msg = "TopLevel.components couldn't find module: " ++ modname
  (Module (Just (Schematic comps)) _ _) <- getModule topl modname ? "TopLevel.components"
  terms <- sequence [terminals topl submod | SubModuleC submod <- DV.toList comps]
  
  let wires = [w | WireC w <- DV.toList comps]
      ports = [p | PortC p <- DV.toList comps]
          
      wireEdges = map wireToEdge wires
      portEdges = map portToEdge ports -- these are degenerate edges.
      termEdges = map termToEdge (concat terms)
          
      edges = wireEdges ++ portEdges ++ termEdges
      
  return $ G.components $ G.fromEdges edges

-- | Get a list of terminals in a submodule offset by the position of the submodule  
terminals :: TopLevel -> SubModule -> J [Terminal]
terminals topl (SubModule modname offset) = do
  mod <- getModule topl modname 
  Module.terminals mod offset

-- | Get the number of distinct nodes in the schematic
-- numComponents :: TopLevel -> String -> J Int
numComponents topl modname = 
  liftM length $ components topl modname ? "Couldn't get number of componenents"


-- | Get the input of a module. This requires tests to be defined in
-- the module referenced, because .input directive of the test script
-- indicate the target signals in the schematic

getInputs :: TopLevel -> String -> J Inputs
getInputs topl modname = do
  let msg = "TopLevel.getInputs couldn't find inputs"
  mod <- getModule topl modname ? msg
  Module.getInputs mod ? msg


-- | Get the outputs of a module. This requires tests to be defined in
-- the module referenced, because the .output directive of the test
-- script indicate the source signals in the schematic

getOutputs :: TopLevel -> String -> J Outputs
getOutputs topl modname = do
  mod <- getModule topl modname ? "TopLevel.getOutputs"
  let msg = "TopLevel.getOutputs couldn't find outputs in module: " ++ modname
  Module.getOutputs mod ? msg

