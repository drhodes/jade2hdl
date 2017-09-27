{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Jade.TopLevel ( getNetsWithName
                     , connectWiresWithSameSigName
                     , dependencyOrder
                     , explodeConnect
                     , getAllIndexesWithName
                     , getAllSigNames
                     , getInputs
                     , getInternalSigNames
                     , getModule
                     , getOutputs
                     , getSchematic
                     , getSubModules
                     , getTerminalsAtPoint
                     , getWidthOfValName
                     , getWidthOfPartsAtTerminal
                     , isNetDriver
                     , netWithTerminal
                     , nets
                     , numNets
                     , replicationDepth
                     , terminals
                     ) where

import Control.Monad
import Control.Monad.State
import qualified Data.List as DL
import qualified Data.Map as DM
import Data.Maybe
import qualified Data.Maybe as Maybe
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Decode.Decode as D
import qualified Jade.Jumper as Jumper
import qualified Jade.MemUnit as MemUnit
import qualified Jade.Module as Module
import qualified Jade.Net as Net
import qualified Jade.Part as Part
import qualified Jade.Term as Term
import qualified Jade.Schematic as Schem
import Jade.Common
import qualified Jade.UnionFindST as UF
import qualified Jade.Wire as Wire
import qualified Web.Hashids as WH
import qualified Jade.Decode.Bundle as Bundle

getSubModules :: String -> J [SubModule]
getSubModules modname = do --"TopLevel.getSubModules" <? do
  (Module _ schem _ _) <- getModule modname
  case schem of 
    Just schem -> return $ Schem.getSubModules schem
    Nothing -> die "No schematics found"
  
-- a function to possible create an edge given a wire and a part
makePartEdge :: Wire -> Part -> J (Maybe Edge)
makePartEdge wire part = "TopLevel.makePartEdge" <? do
  let (loc1, loc2) = Wire.ends wire
  ploc <- Part.loc part 
  
  if ploc == loc1 
    then return $ Just $ Edge (Node loc1 (WireC wire)) (Node ploc part)
    else if ploc == loc2
         then return $ Just $ Edge (Node loc2 (WireC wire)) (Node ploc part)
         else return Nothing

makeWire2WireEdge :: Wire -> Wire -> J (Maybe Edge)
makeWire2WireEdge w1 w2 = "TopLevel.makeWire2WireEdge" <? 
  let (loc1, loc2) = Wire.ends w1
      (loc3, loc4) = Wire.ends w2
      econ p1 v p2 w = Just $ Edge (Node p1 (WireC v)) (Node p2 (WireC w))
  in return $ case (loc1 == loc3, loc1 == loc4, loc2 == loc3, loc2 == loc4) of
                (True, _, _, _) -> econ loc1 w1 loc3 w2
                (_, True, _, _) -> econ loc1 w1 loc4 w2
                (_, _, True, _) -> econ loc2 w1 loc3 w2
                (_, _, _, True) -> econ loc2 w1 loc4 w2
                _ -> Nothing

terminalsOverlapP (Terminal (Coord3 x1 y1 _) _) (Terminal (Coord3 x2 y2 _)  _)
  = (x1,y1) == (x2,y2)

getOverlappingTerminals :: String -> J [(Terminal, Terminal)]
getOverlappingTerminals modname = "TopLevel.collectOverlappingTerminals" <? do
  allsubs <- getSubModules modname
  ts <- concatMapM terminals allsubs
  return $ DL.nub $ DL.sort [ (min t1 t2, max t1 t2) | t1 <- ts, t2 <- ts,
                              terminalsOverlapP t1 t2,
                              t1 /= t2 ]

connectOverlappingTerminals :: [(Terminal, Terminal)] -> J [Wire]
connectOverlappingTerminals termPairs = "TopLevel.connectOverlappingTerminals" <? do
  return $ [Wire.mkDegenerate c | (Terminal c _, _) <- termPairs]

processEdges :: [Wire] -> [Part] -> J [Edge]
processEdges wires parts = "TopLevel.processEdges" <? do
  nb "with all wires, make an edge from ones that share a point with a part"
  partEdges <- sequence [makePartEdge w p | w <- wires, p <- parts]
  
  let Just edges = sequence $ filter Maybe.isJust partEdges
  wireNbrs <- sequence [makeWire2WireEdge v w | (v, w) <- triangleProd wires]
  
  let Just nbrs = sequence $ filter Maybe.isJust wireNbrs
  return $ edges ++ nbrs

makeJumperWire :: Jumper -> J Wire
makeJumperWire jumper = "TopLevel.makeJumperEdge" <? do
  nb "find the endpoints of the jumper"
  let (p1, p2) = Jumper.getEnds jumper
  nb "create a wire where the jumper is"
  return $ Wire.new p1 p2

makePortWire :: Port -> J Wire
makePortWire (Port (Coord3 x y r) sig)  = "TopLevel.makePortWire" <? do
  nb "create a wire of length zero, that has the signal from the port"
  return $ Wire (Coord5 x y r 0 0) sig

connectSubWires :: Wire -> Wire -> J (Maybe Wire)
connectSubWires w1 w2 = "TopLevel.connectSubWires" <? do
  assert (Wire.width w1 == 1) "this function assumes wires are width 1"
  assert (Wire.width w2 == 1) "this function assumes wires are width 1"
  assert (Wire.hasSigName w1) "can't be literal"
  assert (Wire.hasSigName w2) "can't be literal"
  
  if w1 `Wire.hasSameSig` w2
    then do nb "--------------------------------------------"
            enb w1
            enb w2
            return $ Just $ Wire.new (fst $ Wire.ends w1) (fst $ Wire.ends w2)
    else return Nothing

explodeConnect :: (Wire, Wire) -> J [Wire]
explodeConnect (w1, w2) = "TopLevel.explodeConnect" <? do 
  let subwires1 = Wire.explode w1
      subwires2 = Wire.explode w2
  enb (subwires1)
  enb (subwires2)  
  catMaybes <$> sequence [connectSubWires sw1 sw2 | sw1 <- subwires1, sw2 <- subwires2 ]

connectWiresWithSameSigName :: [Part] -> J [Wire]
connectWiresWithSameSigName parts = "TopLevel.connectWiresWithSameSigName" <? do
  let wires = triangleProd $ filter Wire.hasSigName $ catMaybes $ map Part.toWire parts
  let pairs = [(w1, w2) | (w1, w2) <- wires, w1 `Wire.hasSameSig` w2]              
  return [Wire.new (fst $ Wire.ends w1) (fst $ Wire.ends w2) | (w1, w2) <- pairs]
  -- concat <$> sequence [explodeConnect (w1, w2) | (w1, w2) <- wires] --, w1 `Wire.hasSameSig` w2]          
-- | What's going on here? Now that the decoder explodes signal names
-- out into val bundles immediately, it's possible to connect wire
-- names at the very beginning, rather it's possible to associate two
-- wires for the unionfind algorithm. this is the right way to do it.
-- The wrong way, is what I was doing, some after the fact kludge
-- braindead patching.  So two parts enter this function. parts have
-- wires. wires have bundles.  The change in procedure here, what the
-- whole refactoring was about, is to intersect these bundles and feed
-- them to union find. This will reduce the total number of nets!
-- which is good.  wires should just magically connect if this works.
-- So, back to how union find distinguishes nodes, by 

nets :: String -> J [Net] 
nets modname = "TopLevel.nets" <? do
  -- memoize, TODO: abstract this away.
  Memo table <- getMemo
  case DM.lookup modname table of
    -- Already computed this net, so return it.
    Just nets -> return nets
    -- Compute the net, insert it into the memo map, then return the net
    Nothing -> do cs <- nets' modname
                  putMemo $ Memo (DM.insert modname cs table)
                  return cs

nets'  :: String -> J [Net] 
nets' modname = do --"TopLevel.nets_" <? do
  edges <- getEdges modname
  let nets_ = UF.components $ edges
  nb "let nets = UF.components $ edges ++ wireEdges"
  enb nets_
  return nets_

getEdges  :: String -> J [Edge]
getEdges modname = "TopLevel.getEdges" <? do
  nb "---------------------------------"
  nbf "get the module: {0}" [modname]
  (Module _ (Just schem@(Schematic parts)) _ _) <- getModule modname
  
  terms <- sequence [terminals submod | submod <- Schem.getSubModules schem]
  let wires = [w | WireC w <- parts]
      ports = [p | PortC p <- parts]
      jumpers = Schem.getJumpers schem
      termcs = map TermC $ concat terms

  ssnw <- connectWiresWithSameSigName parts
  jumperWires <- mapM makeJumperWire jumpers
  portWires <- mapM makePortWire ports
  ts <- getOverlappingTerminals modname
  overlappingTermWires <- connectOverlappingTerminals ts
  
  let allWires = concat [ wires
                        , jumperWires
                        , ssnw
                        , portWires
                        , overlappingTermWires]
      wireEdges = map Wire.toEdge allWires

  edges <- processEdges allWires termcs 
  return (wireEdges ++ edges)

-- | VHDL requires that modules be instantiated in dependency order,
dependencyOrder :: String -> J [String]
dependencyOrder modname = "TopLevel.dependencyOrder" <? 
  if not $ modname `startsWith` "/user/" then return []
  else do m <- getModule modname
          schem <- Module.getSchematic m
          let subnames = DL.nub [subname | (SubModule subname _) <- Schem.getSubModules schem]
          children <- concatMapM dependencyOrder subnames
          return $ filter (`startsWith` "/user") $ DL.nub $ children ++ subnames 

-- |Get the graph net which contains the terminals.
netWithTerminal :: [Char] -> Terminal -> J Net
netWithTerminal modname term@(Terminal c3@(Coord3 x y _) _) = "TopLevel.netWithTerminal" <? do
  nets <- nets modname
  let result = filter (flip Net.hasTerm term) nets   
  case length result of
    0 -> die $ concat [ " No net found in module: ", modname
                      , " that has a terminal: ", show term ]
    1 -> return $ head result
    x -> die $ concat [ show x, " nets found in module: ", modname
                      , " that has a terminal: ", show term, "."                     
                      , " This should not be possible, because all such nets should"
                      , " be connected if they contain the same node" ]

-- | Get a list of input and output terminals in a submodule offset by
-- the position of the submodule
terminals :: SubModule -> J [Terminal]
terminals (SubModule modname offset) = do --"TopLevel.terminals" <? do
  nb $ show ("TopLevel.terminals checks submodule: " ++ modname)
  mod <- getModule modname
  Module.terminals mod offset

terminals (SubMemUnit memunit) = "TopLevel.terminals/memunit" <? do
  MemUnit.terminals memunit

-- | Get the number of distinct nodes in the schematic
numNets :: String -> J Int
numNets modname = "TopLevel.numNets" <? do
  length <$> nets modname ? "Couldn't get number of nets"

-- -- | Get the input of a module. This requires tests to be defined in
-- -- the module referenced, because .input directive of the test script
-- -- indicate the target signals in the schematic

getInputs :: String -> J Inputs
getInputs modname = "TopLevel.getInputs" <? do
  getModule modname >>= Module.getInputs 

-- -- | Get the outputs of a module. This requires tests to be defined in
-- -- the module referenced, because the .output directive of the test
-- -- script indicate the source signals in the schematic

getOutputs :: String -> J Outputs
getOutputs modname = "TopLevel.getOutputs" <? do
  mod <- getModule modname
  let msg = "TopLevel.getOutputs couldn't find outputs in module: " ++ modname
  Module.getOutputs mod ? msg

-- | The assumption always is, that the jade module works and is
-- tested. With that in mind, then it's safe to assume that there is
-- one driving signal per net. This
-- function finds the driving signal for a given net.

-- If a signal is in more than one net then it is a driving signal.

-- | What signals are driving? .input signals from the test script
-- indicate a set of driving signals. OUTPUT terminals of sub modules
-- are also driving signals. 

getNetWithTerminal :: String -> Terminal -> J Net
getNetWithTerminal modname term  = "getNetWithTerminal" <? do
  allNets <- nets modname
  let matches =  [c | c <- allNets, Net.hasTerm c term] 
  case matches of
    [] -> die $ "No net found with terminal: " ++ show term
    [c] -> do nb $ "found net with terminal: " ++ show term
              return c
    _ -> impossible $ "More than one net found with terminal: " ++ show term

getPartsConnectedToTerminal :: String -> Terminal -> J [Part]
getPartsConnectedToTerminal modname terminal = "TopLevel.getPartsConnectedToTerminal" <? do
  let (Terminal (Coord3 x y _) _) = terminal
  schem <- getSchematic modname
  Schem.getAllPartsAtPoint schem (Point x y)

getWidthOfPartsAtTerminal :: String -> Terminal -> J Int
getWidthOfPartsAtTerminal modname terminal = "TopLevel.getWidthOfPartsAtTerminal" <? do
  parts <- getPartsConnectedToTerminal modname terminal
  maximum <$> mapM Part.width (Part.removeTerms parts)

getTerminalsAtPoint :: String -> Point -> J [Terminal]
getTerminalsAtPoint modname point@(Point x1 y1) = "TopLevel.getTerminalAtPoint" <? do
  schem <- getSchematic modname
  let subs = Schem.getSubModules schem
  allTerms <- concatMapM terminals subs
  return $ filter (flip Term.atPoint point) allTerms

getRatio :: Terminal -> Part -> J (Int, Int)
getRatio terminal part = "TopLevel.getTerminalRatio" <? do
  let tw = Term.width terminal        
  pw <- Part.width part
  return (tw, pw)

getRatios modname term = do
  parts <- getPartsConnectedToTerminal modname term
  mapM (getRatio term) (filter (not . Part.isSubModule) parts)

replicationDepth :: String -> SubModule -> J Int
replicationDepth modname submod  = "TopLevel.replicationDepth" <? do
  terms <- terminals submod
  ratios <- concatMapM (getRatios modname) terms
  let numReps = [if tw >= pw then 1 else pw `div` tw | (tw, pw) <- ratios]
  return $ maximum numReps

getNetsWithName :: String -> String -> J [Net]
getNetsWithName modname signame = "TopLevel.getNetsWithName" <? do
  ns <- nets modname
  filterM (flip Net.containsIdent signame) ns
  
getAllSigNames :: String -> J [String]
getAllSigNames modname = do
  nets <- nets modname
  let parts = (concat $ map Net.parts nets)    
      results = concat $ map Part.getNames parts
  return $ DL.nub results

-- | Given a valnal name scour all the nets that contains the
-- name for the total width of all valnals with the name.
getWidthOfValName :: String -> String -> J Int
getWidthOfValName modname valname = "TopLevel.getWidthOfValName" <? do
  nets <- getNetsWithName modname valname
  vals <- concatMapM (flip Net.getValsWithIdent valname) nets
  return $ length (DL.nub vals)
  
getInternalSigNames modname = "TopLevel.getInternalSigNames" <? do
  m <- getModule modname
  allNames <- getAllSigNames modname
  
  (Inputs inputBundles) <- getInputs modname
  (Outputs outputBundles) <- getOutputs modname

  let inputNames = concat $ map Bundle.getNames inputBundles
      outputNames = concat $ map Bundle.getNames outputBundles
  return $ allNames DL.\\ (inputNames ++ outputNames)
  
getAllIndexesWithName :: String -> String -> J [Val]
getAllIndexesWithName modname name = "TopLevel.getAllIndexesWithName" <? do
  allNets <- nets modname
  return $ DL.nub $ concat $ map (flip Net.getIndexesWithName name) allNets

isNetDriver :: String -> Net -> J Bool
isNetDriver modname net = "TopLevel.isNetDriven" <? do
  -- does this net contain names that are included in the .input
  -- groups?  does this net contain nodes that are connected to the
  -- output of contradiction. if an internal name is both on the
  -- output and input of a submodule, then unfortunately this means
  -- that the net actually belongs to both. but wait. net analysis is pass#1 analysis
  -- so, no, they actually aren't.
  -- how many nets are there here?
  return False
