module Jade.Module ( getSchematic
                   , terminals
                   , getInputs
                   , getOutputs
                   , getInputTerminals
                   , getOutputTerminals
                   , getInputsNoSetSigs
                   , mangleModName
                   , testLines
                   , cycleLine
                   , boundingBox
                   , testBenchName
                   , getSamplesWithName
                   ) where


import Control.Monad
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Decode as D
import qualified Jade.Icon as Icon
import qualified Jade.Coord as Coord
import qualified Jade.ModTest as ModTest
import qualified Jade.BoundingBox as BoundingBox
import Jade.Common hiding (replace)
import Text.Format
import qualified Jade.Bundle as Bundle

getIcon :: Module -> J Icon
getIcon (Module _ _ _ (Just x)) = return x
getIcon _ = die "No icon found in module"

terminals :: Module -> Coord3 -> J [Terminal]
terminals mod p@(Coord3 mx my mr) = do --"Module.terminals" <? do
  icon <- getIcon mod
  let rotateTerm (Terminal (Coord3 tx ty tr) sig) =
        let (Coord3 dx dy _) = Coord.rotate (Coord3 tx ty Rot0) mr 0 0
        in Terminal (Coord3 (mx + dx) (my + dy) (Coord.composeRot mr tr)) sig
  return $ [rotateTerm t | IconTerm t <- iconParts icon]

getSchematic :: Module -> J Schematic
getSchematic (Module name schem _ __) = "Module.getSchematic" <?
  case schem of
    Just schem -> return schem
    Nothing -> die $ "No schematic found for module: " ++ name

boundingBox :: Module -> Coord3 -> J BoundingBox
boundingBox (Module _ _ _ icon) offset = "Module.boundingBox" <? do
  -- move the bounding box of the icon to the offset coord
  -- rotate the new bounding box points around the center of the bounding box.
  case icon of 
    (Just icon) -> do bb <- Icon.boundingBox icon
                      return $ BoundingBox.transRot bb offset
    Nothing -> die "No icon found in, can't find bounding box."

getInputTerminals :: Module -> Coord3 -> J [Terminal]
getInputTerminals mod offset = "Module.getInputTerminals" <? do
  ts <- terminals mod offset
  (Inputs ins) <- getInputs mod
  return [term | term@(Terminal _ sig1) <- ts, sig2 <- ins, sig1 == sig2]

getOutputTerminals :: Module -> Coord3 -> J [Terminal]
getOutputTerminals mod offset = "Module.getOutputTerminals" <? do
  ts <- terminals mod offset
  (Outputs outs) <- getOutputs mod
  return [term | term@(Terminal _ sig1) <- ts, sig2 <- outs, sig1 == sig2]
    
getInputs :: Module -> J Inputs
getInputs m = "Module.getInputs" <? do
  case moduleTest m of
    Just mod ->
      case modInputs mod of
        Just (Inputs ins) -> return $ Inputs $ ins ++ setSignals m
        Nothing -> do nb $ format "Is there a test script in module: {0}?" [moduleName m]
                      die $ "Module.getInputs couldn't find inputs in module: " ++ (moduleName m)
    Nothing ->  die $ "Module.getInputs could not find test script in module: " ++ (moduleName m)

getInputsNoSetSigs :: Module -> J Inputs
getInputsNoSetSigs m = "Module.getInputsNoSetSigs" <? do
  case moduleTest m of
    Just mod ->
      case modInputs mod of
        Just ins -> return ins
        Nothing -> die $ "Module.getInputs couldn't find inputs in module: " ++ (moduleName m)
    Nothing ->  die $ "Module.getInputs could not find test script in module: " ++ (moduleName m)

getOutputs :: Module -> J Outputs
getOutputs m = "Module.getOutputs" <? do
  case moduleTest m of
    Just mod ->
      case modOutputs mod of
        Just outs -> return outs
        Nothing -> die "Module.getOutputs couldn't find outputs"
    Nothing ->  die "Module.getOutputs could not find test script"

-- | For a given testline, extract the expected sample output values
-- associated with a output name.
getSamplesWithName :: Module -> TestLine -> String -> J ValBundle
getSamplesWithName m testline outputName = "Module.getSamples" <? do
  let TestLine binvals _ = testline
  Outputs bundles <- getOutputs m
  let names = concatMap Bundle.getNames bundles 
  return $ Bundle [Lit bv | (name, bv)  <- zip names binvals, name == outputName]

cycleLine :: Module -> J CycleLine
cycleLine m = "Module.cycleLine" <? do
  case moduleTest m of
    Nothing -> die "No test found in this module."
    Just mt -> case modCycleLine mt of
      Nothing -> die "Not cycle line found in this module test"
      Just cl -> return cl

setSignals m =
  case moduleTest m of
    Just mt -> ModTest.setSignals mt
    Nothing -> []

testLines :: Module -> J [TestLine]
testLines m = "Module.testLines" <? do
  case moduleTest m of
    Nothing -> die "No test found in this module."
    Just mt -> return $ modTestLines mt

replace c r xs = concat [if [x] == c then r else [x] | x <- xs]

mangleModName :: String -> String
mangleModName modname = "mod" ++ replace "/" "_" modname

testBenchName modname = mangleModName modname ++ "_tb"

