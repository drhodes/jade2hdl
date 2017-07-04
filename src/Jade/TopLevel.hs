{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Jade.TopLevel where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import qualified Data.Vector as DV
import qualified Jade.UnionFindST as UF
import qualified Jade.Wire as Wire
import qualified Data.Maybe as Maybe
import qualified Jade.Decode as D
import qualified Jade.Module as Module
import qualified Jade.Part as Part
import qualified Jade.Sig as Sig
import Jade.Types
import Jade.Util
import Control.Monad
import qualified Web.Hashids as WH

-- |Get a list of pairs (modulename, module)
modules :: TopLevel -> [(String, Module)]
modules (TopLevel m) = DM.toList m

-- |Get a module from a TopLevel given a module name
getModule :: TopLevel -> String -> J Module
getModule (TopLevel m) name =
  -- if name `startsWith` "/gate"
  -- then return $ BuiltInModule name
  case DM.lookup name m of
    Just mod -> return mod
    Nothing -> die $ "TopLevel.getModule couldn't find module:" ++ name   

-- |Transform a located terminal to a degenerate edge. A located
-- terminal is one that has been placed in a schematic, with absolute coordinate.
termToEdge t@(Terminal (Coord3 x y _) _) =  
  let n = Node (x, y) (TermC t)
  in Edge n n

getSubModules :: TopLevel -> String -> J [SubModule]
getSubModules topl modname = do
  (Module (Just (Schematic parts)) _ _) <- getModule topl modname ? "TopLevel.components"
  return [submod | SubModuleC submod <- DV.toList parts]

-- a function to possible create an edge given a wire and a part
makePartEdge :: Wire -> Part -> J (Maybe (Edge (Integer, Integer)))

makePartEdge wire j@(JumperC (Jumper (Coord3 x y r))) = do
  nb $ "TopLevel.makePartEdge: "
  
  let (loc1, loc2) = Wire.ends wire
      fakeWire = Wire (Coord5 x y r (x+8) y) Nothing
      (loc3, loc4) = Wire.ends fakeWire
      econ p1 v p2 w = Just $ Edge (Node p1 (WireC v)) (Node p2 j)

  (show ((loc1, loc2), (loc3, loc4))) <? do
  return $ case (loc1 == loc3, loc1 == loc4, loc2 == loc3, loc2 == loc4) of
             (True, _, _, _) -> econ loc1 wire loc3 j
             (_, True, _, _) -> econ loc1 wire loc4 j
             (_, _, True, _) -> econ loc2 wire loc3 j
             (_, _, _, True) -> econ loc2 wire loc4 j
             _ -> Nothing
  

makePartEdge wire part = do
  let (loc1, loc2) = Wire.ends wire
  ploc <- Part.loc part ? "TopLevel.makePartEdge"
  
  if ploc == loc1 
    then return $ Just $ Edge (Node loc1 (WireC wire)) (Node ploc part)
    else if ploc == loc2
         then return $ Just $ Edge (Node loc2 (WireC wire)) (Node ploc part)
         else return Nothing

makeWire2WireEdge :: Wire -> Wire -> J (Maybe (Edge (Integer, Integer)))
makeWire2WireEdge w1 w2 = 
  if w1 == w2
  then return Nothing
  else let (loc1, loc2) = Wire.ends w1
           (loc3, loc4) = Wire.ends w2
           econ p1 v p2 w = Just $ Edge (Node p1 (WireC v)) (Node p2 (WireC w))
       in return $ case (loc1 == loc3, loc1 == loc4, loc2 == loc3, loc2 == loc4) of
                     (True, _, _, _) -> econ loc1 w1 loc3 w2
                     (_, True, _, _) -> econ loc1 w1 loc4 w2
                     (_, _, True, _) -> econ loc2 w1 loc3 w2
                     (_, _, _, True) -> econ loc2 w1 loc4 w2
                     _ -> Nothing


jumperToWire j@(JumperC (Jumper (Coord3 x y r))) = do
  nb $ "TopLevel.jumperToWire: "
  return $ Wire (Coord5 x y r (x+8) y) Nothing
          
processEdges wires parts = do
  -- with all wires, make an edge from ones that share a point with a part 
  let wireEdges = map Wire.wireToEdge wires
  partEdges <- sequence [makePartEdge w p | w <- wires, p <- parts]
  -- nb $ "TopLevel.processEdges"
  -- nb $ show partEdges
  let Just edges = sequence $ filter Maybe.isJust partEdges
  wireNbrs <- sequence [makeWire2WireEdge v w | v <- wires, w <- wires]
  let Just nbrs = sequence $ filter Maybe.isJust wireNbrs
  return $ edges ++ nbrs

components topl modname = do
  (Module (Just (Schematic parts)) _ _) <- getModule topl modname ? "TopLevel.components"
  terms' <- sequence [terminals topl submod | SubModuleC submod <- DV.toList parts]
  
  let wires = [w | WireC w <- DV.toList parts]
      ports = [PortC p | PortC p <- DV.toList parts]
      jumpers = [JumperC j | JumperC j <- DV.toList parts]
      terms = map TermC $ concat terms'

  jwires <- mapM jumperToWire jumpers
  let wireEdges = map Wire.wireToEdge (wires ++ jwires)

  edges <- processEdges (wires ++ jwires) (terms ++ ports) -- ++ jumpers)

  let comps = UF.components (edges ++ wireEdges) 
  return comps

getInputTerminals :: TopLevel -> SubModule -> J [Terminal]
getInputTerminals topl (SubModule name offset) = do
  m <- getModule topl name
  Module.getInputTerminals m offset

-- |Get the graph component which contains the terminals.
componentWithTerminal topl modname term@(Terminal (Coord3 x y _) _) = do
  comps <- components topl modname
  let pred set = (Node (x, y) (TermC term)) `elem` set
      result = filter pred comps -- filter out components that don't contain term
  case length result of
    0 -> die $ concat [ " No component found in module: ", modname
                      , " that has a terminal: ", show term ]
    1 -> return $ head result
    x -> die $ concat [ show x, " components found in module: ", modname
                      , " that has a terminal: ", show term, "."                     
                      , " This should not be possible, because all such components should"
                      , " be connected if they contain the same node" ]

-- | Get a list of input and output terminals in a submodule offset by
-- the position of the submodule

terminals :: TopLevel -> SubModule -> J [Terminal]
terminals topl (SubModule modname offset) = do
  mod <- getModule topl modname ? "TopLevel.terminals"
  Module.terminals mod offset

-- | Get the number of distinct nodes in the schematic
numComponents :: TopLevel -> String -> J Int
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


getInputTermDriver topl modname term = 
  "TopLevel.getInputTermDriver" <? do
  
  m <- getModule topl modname
  gcomp <- componentWithTerminal topl modname term

  let partList = let ps1 = map nodePart gcomp
                     -- remove the source terminal
                     ps2 = DL.delete (TermC term) ps1
                     -- remove parts with no signal name
                     ps3 = filter Part.hasSigName ps2
                 in ps3

  -- check the test script, if any graph comp signals match the .input
  -- lines.  if so, then that's it.
  (Inputs inputSigs) <- getInputs topl modname
  let partsMatchingInput = DL.nub [p | sig <- inputSigs, p <- partList, Part.sig p == Just sig]

  case length partsMatchingInput of
    1 -> let match = head partsMatchingInput
         in case Part.sig match of
              Just sig -> return sig
              Nothing -> die $ "Impossible, no signal was found in this part: " ++ show match
      -- ok, so none of the parts have signals matching the a signal
      -- found in the .input line of the module's test script.  See if
      -- any of the parts are terminals, and if those terminals belong
      -- to the output of a sub module, and which sub module.
    0 -> "no parts matching inputs" <? do
      let terms = [t | (TermC t) <- partList]
      submods' <- mapM (subModuleWithOutputTerminal topl modname) terms
      let submods = Maybe.catMaybes . concat . concat $ submods'
      case submods of
        [(Terminal coord sig, submod)] -> Sig.hashMangle (hashid submod) sig 
        [] -> die $ "Couldn't find the driving signal in a test script input or sub module output: " ++ show partList
        xs -> die $ "The impossible happened, many submodules output to this terminal" ++ show xs
      
    _ -> die $ "component somehow contains more than one .input, \
               \ which is impossible because that would mean more \
               \ than one signal is driving this component: " ++ show partsMatchingInput

subModuleWithOutputTerminal topl modname term = do
  let try x = x ? "subModuleWithOutputTerminal"
  allsubs <- try $ getSubModules topl modname
  forM allsubs $ \submod@(SubModule subname subloc) -> do
    -- for each submodule check to see if its terminals contain the terminal
    m <- try $ getModule topl subname
    subterms <- try $ terminals topl submod
    forM subterms $ \subterm -> do
      if (term == subterm)
        then return $ Just (term, submod)
        else return Nothing


testMakeEdge1 = do
  let wire = Wire (Coord5 0 0 Rot0 0 2) Nothing
  let part = TermC $ Terminal (Coord3 0 0 Rot0) (SigSimple "test")
  printJ $ makePartEdge wire part


outputConnectedToSubModuleP topl modname signal = do
  nb "Check if an output signal is connected to a submodule."
  nb "If not, then code for that connection will need to be generated"

  comps <- components topl modname
  nb "Find the component with signal name"
  nb "If that component doesn't contain a terminal, then true"

  return "asdf"
