module Jade.Module where

import Control.Monad
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Decode as D
import qualified Jade.Part as Part
import qualified Jade.Icon as Icon
import qualified Jade.Coord as Coord
import Jade.Types
import Jade.Wire

       -- // compute new position and rotation
       -- var new_x = transform_x(rotation, rx, ry) + cx;
       -- var new_y = transform_y(rotation, rx, ry) + cy;
       -- var new_rotation = rotate[old_rotation * 8 + rotation];

-- terminals :: Module -> Coord3 -> J [Terminal]
-- terminals (Module _ _ icon) offset@(Coord3 dx dy dr) =
--   "Module.testTerms:"   <? do
--   nb $ show offset
  
--   case icon of
--     Nothing -> die "No icon found in module, module needs an icon to figure\n\
--                    \ out where the terminals are in submodules"
--     Just ic@(Icon parts) -> do
--       (cx, cy) <- Icon.center ic
--       nb $ show ("Center", (cx, cy))
--       return $ [Terminal (Coord.transform3 c3 offset cx cy) sig |
--                 IconTerm (Terminal c3 sig) <- parts]

terminals :: Module -> Coord3 -> J [Terminal]
terminals (Module _ _ icon) offset@(Coord3 dx dy _) =
  "Module.terminals:"   <? do
  case icon of
    Nothing -> die "No icon found in module, module needs an icon to figure\n\
                   \ out where the terminals are in submodules"
    Just (Icon parts) ->
      return $ [Terminal (Coord3 (x+dx) (y+dy) r) sig |
                IconTerm (Terminal (Coord3 x y r) sig) <- parts]


getInputTerminals :: Module -> Coord3 -> J [Terminal]
getInputTerminals mod offset = "Module.getInputTerminals" <? do
  ts <- terminals mod offset
  (Inputs ins) <- getInputs mod
  return [term | term@(Terminal _ sig1) <- ts, sig2 <- ins, sig1 == sig2]

getOutputTerminals :: Module -> Coord3 -> J [Terminal]
getOutputTerminals mod offset = "Module.getOutputTerminals" <? do
  ts <- terminals mod offset
  (Outputs ins) <- getOutputs mod
  return [term | term@(Terminal _ sig1) <- ts, sig2 <- ins, sig1 == sig2]
    
getInputs :: Module -> J Inputs
getInputs m = "Module.getIntputs" <? do
  case moduleTest m of
    Just mod ->
      case modInputs mod of
        Just outs -> return outs
        Nothing -> die "Module.getInputs couldn't find inputs"
    Nothing ->  die "Module.getInputs could not find test script"

getOutputs :: Module -> J Outputs
getOutputs m = "Module.getOutputs" <? do
  case moduleTest m of
    Just mod ->
      case modOutputs mod of
        Just outs -> return outs
        Nothing -> die "Module.getOutputs couldn't find outputs"
    Nothing ->  die "Module.getOutputs could not find test script"

inputsHaveSig :: Module -> Sig -> J Bool
inputsHaveSig mod sig = "Module.inputsHaveSig" <? do
  (Inputs ins) <- getInputs mod
  return $ sig `elem` ins

outputsHaveSig :: Module -> Sig -> J Bool
outputsHaveSig mod sig = "outputsHaveSig" <? do
  (Outputs outs) <- getOutputs mod
  return $ sig `elem` outs

partInInputs :: Module -> Part -> J Bool
partInInputs mod comp = "Module.partInInputs" <? do
  case Part.sig comp of 
    Just s -> inputsHaveSig mod s
    Nothing -> return False

cycleLine :: Module -> J CycleLine
cycleLine m = "Module.cycleLine" <? do
  case moduleTest m of
    Nothing -> die "No test found in this module."
    Just mt -> case modCycleLine mt of
      Nothing -> die "Not cycle line found in this module test"
      Just cl -> return cl

testLines :: Module -> J [TestLine]
testLines m = "Module.testLines" <? do
  case moduleTest m of
    Nothing -> die "No test found in this module."
    Just mt -> return $ modTestLines mt

replace c r xs = concat [if [x] == c then r else [x] | x <- xs]

mangleModName :: String -> String
mangleModName modname = "mod" ++ replace "/" "_" modname


testBenchName modname = mangleModName modname ++ "_tb"
