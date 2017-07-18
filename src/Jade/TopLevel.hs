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
import qualified Jade.GComp as GComp
import qualified Jade.Schematic as Schem
import qualified Jade.Jumper as Jumper
import Jade.Types
import Jade.Util
import Control.Monad
import qualified Web.Hashids as WH

-- |Get a list of pairs (modulename, module)
modules :: TopLevel -> [(String, Module)]
modules (TopLevel m) = DM.toList m

-- |Get a module from a TopLevel given a module name
getModule :: TopLevel -> String -> J Module
getModule (TopLevel m) name = "TopLevel.getModule" <? do
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
  (Module (Just schem) _ _) <- getModule topl modname ? "TopLevel.components"
  return $ Schem.getSubModules schem

-- a function to possible create an edge given a wire and a part
makePartEdge :: Wire -> Part -> J (Maybe Edge)
makePartEdge wire part = do
  let (loc1, loc2) = Wire.ends wire
  ploc <- Part.loc part ? "TopLevel.makePartEdge"
  
  if ploc == loc1 
    then return $ Just $ Edge (Node loc1 (WireC wire)) (Node ploc part)
    else if ploc == loc2
         then return $ Just $ Edge (Node loc2 (WireC wire)) (Node ploc part)
         else return Nothing

makeWire2WireEdge :: Wire -> Wire -> J (Maybe Edge)
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
          
processEdges :: [Wire] -> [Part] -> J [Edge]
processEdges wires parts = do
  -- with all wires, make an edge from ones that share a point with a part 
  let wireEdges = map Wire.toEdge wires
  partEdges <- sequence [makePartEdge w p | w <- wires, p <- parts]
  -- nb $ "TopLevel.processEdges"
  -- nb $ show partEdges
  let Just edges = sequence $ filter Maybe.isJust partEdges
  wireNbrs <- sequence [makeWire2WireEdge v w | v <- wires, w <- wires]
  let Just nbrs = sequence $ filter Maybe.isJust wireNbrs
  return $ edges ++ nbrs

findWireWithEndPoint parts p = "findWireWithEndPoint" <? do
  let matches = [wire | wire@(WireC w) <- parts, let (p1, p2) = Wire.ends w
                                                 in p == p1 || p == p2 ]
  if null matches
    then die $ "Couldn't find wire with end point: " ++ show p
    else return $ head matches

makeJumperWire :: [Part] -> Jumper -> J Wire
makeJumperWire wires jumper = "makeJumperEdge" <? do
  nb "find the endpoints of the jumper"
  let (p1, p2) = Jumper.getEnds jumper
  nb "create a wire where the jumper is"
  return $ Wire.new p1 p2

connectWiresWithSameSigName parts = "connectWiresWithSameSigName" <? do
  let pairs = DL.nub [DL.sort [wc1, wc2] |
                      wc1@(WireC w1) <- parts,
                      wc2@(WireC w2) <- parts, (w1 /= w2) && (w1 `Wire.hasSameSig` w2)]
  return [Wire.new (fst $ Wire.ends w1) (fst $ Wire.ends w2) | [WireC w1, WireC w2] <- pairs]

components  :: TopLevel -> String -> J [GComp] 
components topl modname = do
  (Module (Just schem@(Schematic parts)) _ _) <- getModule topl modname ? "TopLevel.components"
  terms' <- sequence [terminals topl submod | submod <- Schem.getSubModules schem]
  nb $ DL.intercalate "\n" $ map show parts
  nb $ DL.intercalate "\n" $ map show terms'

  
  let wires = [w | WireC w <- parts]
      ports = [PortC p | PortC p <- parts]
      jumpers = Schem.getJumpers schem
      terms = map TermC $ concat terms'
  
  ssnw <- connectWiresWithSameSigName parts 
  jumperWires <- mapM (makeJumperWire parts) jumpers
  let wireEdges = map Wire.toEdge (wires ++ jumperWires ++ ssnw)
  edges <- processEdges (wires ++ jumperWires ++ ssnw) (terms ++ ports)  

  let comps = UF.components (edges ++ wireEdges)
  return comps

getInputTerminals :: TopLevel -> SubModule -> J [Terminal]
getInputTerminals topl (SubModule name offset) = do
  m <- getModule topl name
  Module.getInputTerminals m offset

-- |Get the graph component which contains the terminals.
componentWithTerminal topl modname term@(Terminal c3@(Coord3 x y _) _) =
  ("TopLevel.componentWithTerminal: " ++ modname) <? do
  comps <- components topl modname
  let pred (GComp set) = (Node (x, y) (TermC term)) `elem` set
      result = filter pred comps -- filter out components that don't contain term
  case length result of
    0 -> do
      nb $ show $ map GComp.getSigs comps
      die $ concat [ " No component found in module: ", modname
                   , " that has a terminal: ", show term ]
    1 -> return $ head result
    x -> die $ concat [ show x, " components found in module: ", modname
                      , " that has a terminal: ", show term, "."                     
                      , " This should not be possible, because all such components should"
                      , " be connected if they contain the same node" ]

-- | Get a list of input and output terminals in a submodule offset by
-- the position of the submodule

terminals :: TopLevel -> SubModule -> J [Terminal]
terminals topl (SubModule modname offset) = "TopLevel.terminals" <? do
  mod <- getModule topl modname
  Module.terminals mod offset

-- | Get the number of distinct nodes in the schematic
numComponents :: TopLevel -> String -> J Int
numComponents topl modname = 
  liftM length $ components topl modname ? "Couldn't get number of componenents"

-- | Get the input of a module. This requires tests to be defined in
-- the module referenced, because .input directive of the test script
-- indicate the target signals in the schematic

getInputs :: TopLevel -> String -> J Inputs
getInputs topl modname =
  "TopLevel.getInputs" <? getModule topl modname >>= Module.getInputs 

-- | Get the outputs of a module. This requires tests to be defined in
-- the module referenced, because the .output directive of the test
-- script indicate the source signals in the schematic

getOutputs :: TopLevel -> String -> J Outputs
getOutputs topl modname = "TopLevel.getOutputs" <? do
  mod <- getModule topl modname
  let msg = "TopLevel.getOutputs couldn't find outputs in module: " ++ modname
  Module.getOutputs mod ? msg

-- | The assumption always is, that the jade module works and is
-- tested. With that in mind, then it's safely assumed that there is
-- one driving signal per component. This function finds the driving
-- signal for a given component.

-- If a signal is in more than one component then it is a driving signal.

-- | What signals are driving? .input signals from the test script
-- clearly indicate a set of driving signals. .output terminals of sub
-- modules are also driving signals.  

getInputTermDriver :: TopLevel -> String -> Terminal -> J Sig
getInputTermDriver topl modname term = 
  "TopLevel.getInputTermDriver" <? do
  
  m <- getModule topl modname
  (GComp nodes) <- componentWithTerminal topl modname term

  let partList = let ps1 = map nodePart nodes
                     -- remove the source terminal
                     ps2 = DL.delete (TermC term) ps1
                     -- remove parts with no signal name
                     ps3 = filter Part.hasSigName ps2
                 in ps3

  -- check the test script, if any graph comp signals match the .input
  -- lines.  if so, then that's it.
  (Inputs inputSigs) <- getInputs topl modname
  let partsMatchingInput = DL.nub [p | sig <- inputSigs, p <- partList, Part.sig p == Just sig]

  nb $ show nodes
  case length partsMatchingInput of
    0 -> "no parts matching inputs" <? do
      let terms = [t | (TermC t) <- partList]
      submods' <- mapM (subModuleWithOutputTerminal topl modname) terms
      let submods = Maybe.catMaybes . concat . concat $ submods'
      case submods of
        [(Terminal coord sig, submod)] -> Sig.hashMangle (hashid submod) sig 
        [] -> "looing in component attached to terminal" <? do
          comp <- getComponentWithTerminal topl modname term
          nb "Does this component have a .input signal?"
          -- let (Terminal _ s) = term
          
          let drivers = filter (GComp.hasSig comp) inputSigs
          case drivers of
            [sig] -> return sig
            [] -> die $ "Couldn't find driving signal in a test script input, \
                        \sub module output or in shared component: " ++ show comp
            _ -> impossible $ "More than one driver found in terminal component: " ++ show term
        xs -> impossible $ "Many submodules output to this terminal" ++ show xs
      
    _ -> let match = head partsMatchingInput
         in case Part.sig match of
              Just sig -> return sig
              Nothing -> die $ "Impossible, no signal was found in this part: " ++ show match


getComponentWithTerminal :: TopLevel -> String -> Terminal -> J GComp
getComponentWithTerminal topl modname term  = "getComponentWithTerminal" <? do
  comps <- components topl modname
  let matches =  [c | c <- comps, GComp.hasTerm c term] 
  case matches of
    [c] -> return c
    [] -> die $ "No component found with terminal: " ++ show term
    _ -> impossible $ "More than one component found with terminal: " ++ show term


subModuleWithOutputTerminal topl modname term = "subModuleWithOutputTerminal" <? do
  allsubs <- getSubModules topl modname
  forM allsubs $ \submod@(SubModule subname subloc) -> do
    -- for each submodule check to see if its terminals contain the terminal
    m <- getModule topl subname
    subterms <- terminals topl submod
    forM subterms $ \subterm -> do
      if (term == subterm)
        then return $ Just (term, submod)
        else return Nothing

sigConnectedToSubModuleP :: TopLevel -> String -> Sig -> J Bool
sigConnectedToSubModuleP topl modname sig = do
  nb "Check if an output signal is connected to a submodule."
  nb "If not, then code for that connection will need to be generated"
  gcomps <- components topl modname  
  nb "Find the components with signal name"
  let compsWithSig = filter (flip GComp.hasSig sig) gcomps
  nb "Of those components, do any have a terminal?"
  return $ or $ map GComp.hasAnyTerm compsWithSig
  
getCompsWithoutTerms :: TopLevel -> String -> J [GComp]
getCompsWithoutTerms topl modname = "getCompsWithoutTerms" <?
  (fmap (filter (not . GComp.hasAnyTerm)) (components topl modname))



--getCompDriver 


  
  --   return sig


-- If there is no driving signal in this component, then find other other components

