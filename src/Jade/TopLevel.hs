module Jade.TopLevel where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Graph as G
import qualified Jade.Decode as D
import qualified Jade.Module as Module
import qualified Jade.Part as Part
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

getSubModules topl modname = do
  (Module (Just (Schematic parts)) _ _) <- getModule topl modname ? "TopLevel.components"
  return [submod | SubModuleC submod <- DV.toList parts]

-- |Get the components of a module given the module's name.
components :: TopLevel -> String -> J [GComp]
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

-- |Get the graph component which contains the terminals.

componentWithTerminal :: TopLevel -> String -> Terminal -> J GComp
componentWithTerminal topl modname term@(Terminal (Coord3 x y _) _) = do
  comps <- components topl modname
  let pred set = (Node (x, y) (TermC term)) `elem` set
      result = filter pred comps -- filter out components that don't contain term
  case length result of
    0 -> die $ concat [ " No component found in module: ", modname
                      , " that has a terminal: ", show term
                      ]
    1 -> return $ head result
    x -> die $ concat [ show x, " components found in module: ", modname
                      , " that has a terminal: ", show term, "."                     
                      , " This should not be possible, because all such components should"
                      , " be connected if they contain the same node"
                      ]

-- | Get a list of terminals in a submodule offset by the position of the submodule  
terminals :: TopLevel -> SubModule -> J [Terminal]
terminals topl (SubModule modname offset) = do
  mod <- getModule topl modname ? "TopLevel.terminals"
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

-- unit1 : AND2 port map (in1 => a, in2 => b, out1 => c);

-- | The assumption always is, that the jade module works and is
-- tested. With that in mind, then it's safely assumed that there is
-- one driving signal per component. This function finds the driving
-- signal for a given component.

-- If a signal is in more than one component then it is a driving signal.

-- | What signals are driving? .input signals from the test script
-- clearly indicate a set of driving signals. .output terminals of sub
-- modules are also driving signals.  

getInputTermDriver topl modname term = do
  m <- getModule topl modname ? "TopLevel.getInputTermDriver"
  graphComp <- componentWithTerminal topl modname term ? "TopLevel.getInputTermDriver"

  let partList = map nodePart $ DS.toList graphComp

  -- check the test script, if any graph comp signals match the .input
  -- lines.  if so, then that's it.  If this list is empty, then need
  -- to check sub module output terminals.  if those don't match
  -- ... well
  
  xs <- filterM (Module.partInInputs m) partList
  case length xs of    
    1 -> return $ head xs
    0 -> do undefined
      -- check sub module output terminals.
  --     xs <- filterM (Module.partInSubmoduleOutputs) partList
  --     case length xs of
  --       1 -> return $ head xs
  --       _ -> die "TopLevel.getInputTermDriver needs to do more work to find the driver"
  -- -- NOT DONE YET. Also need 

  

  
  -- which schem check each wire, term, etc. to see if it a driver of term. 

figurePortMap topl modname = do
  comps <- components topl modname
  return comps
