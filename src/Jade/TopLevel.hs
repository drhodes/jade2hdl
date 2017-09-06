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
import qualified Jade.Net as Net
import qualified Jade.Schematic as Schem
import qualified Jade.Jumper as Jumper
import qualified Jade.Coord as Coord
import qualified Jade.MemUnit as MemUnit
import Jade.Types
import Jade.Util
import Control.Monad
import Control.Monad.State
import qualified Web.Hashids as WH

-- |Get a list of pairs (modulename, module)
modules :: TopLevel -> [(String, Module)]
modules (TopLevel m) = DM.toList m

-- |Get a module from a TopLevel given a module name
getModule :: String -> J Module
getModule name = "TopLevel.getModule" <? do  
  -- if name `startsWith` "/gate"
  -- then return $ BuiltInModule name
  TopLevel m <- getTop
  case DM.lookup name m of
    Just mod -> return mod{moduleName = name}
    Nothing -> die $ "TopLevel.getModule couldn't find module:" ++ name   

-- |Transform a located terminal to a degenerate edge. A located
-- terminal is one that has been placed in a schematic, with absolute coordinate.
termToEdge t@(Terminal (Coord3 x y _) _) =  
  let n = Node (x, y) (TermC t)
  in Edge n n

getSubModules :: String -> J [SubModule]
getSubModules modname = "TopLevel.getSubModules" <? do
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

terminalsOverlapP (Terminal (Coord3 x1 y1 _) _) (Terminal (Coord3 x2 y2 _)  _) = (x1,y1) == (x2,y2)

getOverlappingTerminals :: String -> J [(Terminal, Terminal)]
getOverlappingTerminals modname = "TopLevel.collectOverlappingTerminals" <? do
  allsubs <- getSubModules modname
  ts <- concatMapM terminals allsubs
  return $ DL.nub $ DL.sort [(min t1 t2, max t1 t2) | t1 <- ts, t2 <- ts, terminalsOverlapP t1 t2, t1 /= t2]

connectOverlappingTerminals :: [(Terminal, Terminal)] -> J [Wire]
connectOverlappingTerminals termPairs = "TopLevel.connectOverlappingTerminals" <? do
  return $ [Wire.mkDegenerate c | (Terminal c _, _) <- termPairs]

processEdges :: [Wire] -> [Part] -> J [Edge]
processEdges wires parts = "TopLevel.processEdges" <? do
  nb "with all wires, make an edge from ones that share a point with a part"
  let wireEdges = map Wire.toEdge wires
  partEdges <- sequence [makePartEdge w p | w <- wires, p <- parts]
  
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

makeJumperWire :: Jumper -> J Wire
makeJumperWire jumper = "makeJumperEdge" <? do
  nb "find the endpoints of the jumper"
  let (p1, p2) = Jumper.getEnds jumper
  nb "create a wire where the jumper is"
  return $ Wire.new p1 p2

makePortWire :: Port -> J Wire
makePortWire (Port (Coord3 x y r) sig)  = "makePortWire" <? do
  nb "create a wire of length zero, that has the signal from the port"
  return $ Wire (Coord5 x y r 0 0) sig

connectWiresWithSameSigName parts = "connectWiresWithSameSigName" <? do
  let pairs = DL.nub [DL.sort [wc1, wc2] |
                      wc1@(WireC w1) <- parts,
                      wc2@(WireC w2) <- parts, (w1 /= w2) && (w1 `Wire.hasSameSig` w2)]
  return [Wire.new (fst $ Wire.ends w1) (fst $ Wire.ends w2) | [WireC w1, WireC w2] <- pairs]

nets :: String -> J [Net] 
nets modname = "TopLevel.nets" <? do
  -- memoize
  Memo table <- getMemo
  case DM.lookup modname table of
    -- Already computed this net, so return it.
    Just nets -> return nets
    -- Compute the net, insert it into the memo map, then return the net
    Nothing -> do cs <- nets' modname
                  putMemo $ Memo (DM.insert modname cs table)
                  return cs
      

nets'  :: String -> J [Net] 
nets' modname = "TopLevel.nets" <? do
  (Module _ (Just schem@(Schematic parts)) _ _) <- getModule modname
  terms <- sequence [terminals submod | submod <- Schem.getSubModules schem]
  
  nb "check to see if ports are directly on terminals."
  
  let wires = [w | WireC w <- parts]
      ports = [p | PortC p <- parts]
      jumpers = Schem.getJumpers schem
      termcs = map TermC $ concat terms
  
  ssnw <- connectWiresWithSameSigName parts 
  jumperWires <- mapM makeJumperWire jumpers
  portWires <- mapM makePortWire ports
  ts <- getOverlappingTerminals modname
  overlappingTermWires <- connectOverlappingTerminals ts
  
  let allWires = concat [wires, jumperWires, ssnw, portWires, overlappingTermWires]
      wireEdges = map Wire.toEdge allWires
                  
  edges <- processEdges allWires termcs 
  
  let nets = UF.components $ edges ++ wireEdges
  return nets

-- | VHDL requires that modules be instantiated in dependency order,


dependencyOrder :: String -> J [String]
dependencyOrder modname = "TopLevel.dependencyOrder" <? 
  if not $ modname `startsWith` "/user/" then return []
  else do m <- getModule modname
          schem <- Module.getSchematic m
          let subnames = DL.nub [subname | (SubModule subname _) <- Schem.getSubModules schem]
          children <- concatMapM dependencyOrder subnames
          return $ filter (`startsWith` "/user") $ DL.nub $ children ++ subnames 

getInputTerminals :: SubModule -> J [Terminal]
getInputTerminals (SubModule name offset) = do
  m <- getModule name
  Module.getInputTerminals m offset

-- |Get the graph net which contains the terminals.
netWithTerminal :: [Char] -> Terminal -> J Net
netWithTerminal modname term@(Terminal c3@(Coord3 x y _) _) =
  ("TopLevel.netWithTerminal: " ++ modname) <? do
  nets <- nets modname
  let pred (Net gid set) = (Node (x, y) (TermC term)) `elem` set
      result = filter pred nets -- filter out nets that don't contain term
  case length result of
    0 -> do
      nb $ show $ map Net.getSigs nets
      die $ concat [ " No net found in module: ", modname
                   , " that has a terminal: ", show term ]
    1 -> return $ head result
    x -> die $ concat [ show x, " nets found in module: ", modname
                      , " that has a terminal: ", show term, "."                     
                      , " This should not be possible, because all such nets should"
                      , " be connected if they contain the same node" ]

-- | Get a list of input and output terminals in a submodule offset by
-- the position of the submodule
terminals :: SubModule -> J [Terminal]
terminals (SubModule modname offset) = "TopLevel.terminals" <? do
  nb $ show ("TopLevel.terminals checks submodule: " ++ modname)
  mod <- getModule modname
  Module.terminals mod offset

terminals (SubMemUnit memunit) = "TopLevel.terminals/memunit" <? do
  MemUnit.terminals memunit

-- | Get the number of distinct nodes in the schematic
numNets :: String -> J Int
numNets modname = "TopLevel.numNets" <? do
  liftM length $ nets modname ? "Couldn't get number of netonenents"

-- | Get the input of a module. This requires tests to be defined in
-- the module referenced, because .input directive of the test script
-- indicate the target signals in the schematic

getInputs :: String -> J Inputs
getInputs modname = "TopLevel.getInputs" <? do
  nb modname
  getModule modname >>= Module.getInputs 

-- | Get the outputs of a module. This requires tests to be defined in
-- the module referenced, because the .output directive of the test
-- script indicate the source signals in the schematic

getOutputs :: String -> J Outputs
getOutputs modname = "TopLevel.getOutputs" <? do
  mod <- getModule modname
  let msg = "TopLevel.getOutputs couldn't find outputs in module: " ++ modname
  Module.getOutputs mod ? msg

-- | The assumption always is, that the jade module works and is
-- tested. With that in mind, then it's safe to assume that there is
-- one driving signal (or 'SigConcat') per net. This
-- function finds the driving signal for a given net.

-- If a signal is in more than one net then it is a driving signal.

-- | What signals are driving? .input signals from the test script
-- clearly indicate a set of driving signals. .output terminals of sub
-- modules are also driving signals.  

getInputTermDriver :: String -> Terminal -> J (Maybe Sig)
getInputTermDriver modname term = "TopLevel.getInputTermDriver" <? do
  m <- getModule modname
  Net gid nodes <- netWithTerminal modname term

  let partList = let ps1 = map nodePart nodes
                     -- remove the source terminal
                     ps2 = DL.delete (TermC term) ps1
                     -- remove parts with no signal name
                     ps3 = filter Part.hasSigName ps2
                 in ps3

  -- check the test script, if any graph net signals match the .input
  -- lines.  if so, then that's it.
  (Inputs inputSigs) <- getInputs modname
  let partsMatchingInput = DL.nub [p | sig <- inputSigs, p <- partList, Part.sig p == Just sig]
  
  case length partsMatchingInput of
    0 -> "no parts matching inputs" <? do
      let terms = [t | (TermC t) <- partList]
      submods' <- mapM (subModuleWithOutputTerminal modname) terms
      let submods = Maybe.catMaybes . concat . concat $ submods'
      case submods of
        [(Terminal coord sig, submod)] -> Just `liftM` Sig.hashMangle (hashid submod) sig 
        [] -> "looking in net attached to terminal" <? do
          net <- getNetWithTerminal modname term
          nb "Does this net have a .input signal?"
          let quotedSigs = Net.getQuotedSigs net
          nb $ "Found quoted signal: " ++ show quotedSigs
          nb $ "Checking input signals: " ++ show inputSigs
          let drivers = filter (Net.hasSig net) inputSigs
          case drivers ++ quotedSigs of
            [sig] -> do nb $ "found sig: " ++ show sig
                        return (Just sig)
            [] -> do nb $ "Couldn't find driving signal in a test script input, \
                          \sub module output or in shared net: " ++ show net
                     return Nothing
            _ -> impossible $ "More than one driver found in terminal net: " ++ show term
        xs -> impossible $ "Many submodules output to this terminal" ++ show xs
      
    _ -> let match = head partsMatchingInput
         in case Part.sig match of
              Just sig -> return (Just sig)
              Nothing -> die $ "Impossible, no signal was found in this part: " ++ show match

getNetWithTerminal :: String -> Terminal -> J Net
getNetWithTerminal modname term  = "getNetWithTerminal" <? do
  nets <- nets modname
  let matches =  [c | c <- nets, Net.hasTerm c term] 
  case matches of
    [c] -> do nb $ "found net with terminal: " ++ show term
              return c
    [] -> die $ "No net found with terminal: " ++ show term
    _ -> impossible $ "More than one net found with terminal: " ++ show term

subModuleWithOutputTerminal modname term = "subModuleWithOutputTerminal" <? do
  allsubs <- getSubModules modname
  forM allsubs $ \submod@(SubModule subname subloc) -> do
    -- for each submodule check to see if its terminals contain the terminal
    m <- getModule subname
    subterms <- terminals submod
    forM subterms $ \subterm -> do
      if (term == subterm)
        then return $ Just (term, submod)
        else return Nothing

sigConnectedToSubModuleP :: String -> Sig -> J Bool
sigConnectedToSubModuleP modname sig = do
  nb "Check if an output signal is connected to a submodule."
  nb "If not, then code for that connection will need to be generated"
  nets <- nets modname  
  nb "Find the nets with signal name"
  let netsWithSig = filter (flip Net.hasSig sig) nets
  nb "Of those nets, do any have a terminal?"
  return $ or $ map Net.hasAnyTerm netsWithSig
  
getNetsWithoutTerms :: String -> J [Net]
getNetsWithoutTerms modname = "getNetsWithoutTerms" <?
  (fmap (filter (not . Net.hasAnyTerm)) (nets modname))

replicationDepth modname submod  = "TopLevel.replicationDepth" <? do
  -- get the terminals of the submodule
  terms <- terminals submod

  let determineWidthFromTerm t = do
        net <- getNetWithTerminal modname t
        nb "know the width of the terminals?"
        termWidth <- Part.width (TermC t)
        nb "remove terms from net and guess its width"
        cw <- Net.width (Net.removeTerms net)
        nb $ "The net sigs are: {0}" ++ show (Net.getSigs net)
        case (termWidth, cw) of
          (Just tw, cw) -> do
            nb "found guesses for terminal width and net width"
            nb $ show (tw, cw)
            return $ cw `div` tw
          -- (_, _) -> do
          --   die $ "net width couldn't be determined"
          (_, _) -> return 1 -- die $ "Couldn't find guess for terminal width nor net width."

  -- if the width of a net is undeclared, this may mean that
  guesses <- mapM determineWidthFromTerm terms
  return $ maximum guesses
  
getNetsWithName :: String -> String -> J [Net]
getNetsWithName modname signame = "TopLevel.getNetsWithName" <? do
  nets <- nets modname  
  filterM (flip Net.containsSigIdent signame) nets
  
getAllSigNames modname = do
  nets <- nets modname
  let parts = (concat $ map Net.parts nets)    
  results <- concatMapM Sig.getNames $ [x | Just x <- map Part.sig parts]
  return $ DL.nub results

-- | Given a signal name scour all the nets that contains the
-- name for the total width of all signals with the name.
getWidthOfSigName modname signame = do
  nets <- getNetsWithName modname signame
  sigs <- concatMapM (flip Net.getSigsWithIdent signame) nets
  return $ sum $ map Sig.width (DL.nub sigs)


