module Jade.Module where

import Control.Monad
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Decode as D
import qualified Jade.Part as Part
import Jade.Types
import Jade.Wire

terminals :: Module -> Coord3 -> J [Terminal]
terminals (Module _ _ icon) offset@(Coord3 dx dy _) =
  case icon of
    Nothing -> die "no icon found in module"
    Just (Icon parts) ->
      
      return $ [Terminal (Coord3 (x+dx) (y+dy) r) sig |
                IconTerm (Terminal (Coord3 x y r) sig) <- parts]

getInputTerminals :: Module -> Coord3 -> J [Terminal]
getInputTerminals mod offset = do
  ts <- terminals mod offset
  (Inputs ins) <- getInputs mod
  return [term | term@(Terminal _ sig1) <- ts, sig2 <- ins, sig1 == sig2]

getOutputTerminals :: Module -> Coord3 -> J [Terminal]
getOutputTerminals mod offset = do
  ts <- terminals mod offset
  (Outputs ins) <- getOutputs mod
  return [term | term@(Terminal _ sig1) <- ts, sig2 <- ins, sig1 == sig2]
    
getInputs :: Module -> J Inputs
getInputs m = case moduleTest m of
                 Just mod -> case modInputs mod of
                   Just outs -> return outs
                   Nothing -> die "Module.getInputs couldn't find inputs"
                 Nothing ->  die "Module.getInputs could not find test script"

getOutputs :: Module -> J Outputs
getOutputs m = case moduleTest m of
                 Just mod -> case modOutputs mod of
                   Just outs -> return outs
                   Nothing -> die "Module.getOutputs couldn't find outputs"
                 Nothing ->  die "Module.getOutputs could not find test script"

inputsHaveSig :: Module -> Sig -> J Bool
inputsHaveSig mod sig = do
  (Inputs ins) <- getInputs mod ? "Module.inputsHaveSig"
  return $ sig `elem` ins

outputsHaveSig :: Module -> Sig -> J Bool
outputsHaveSig mod sig = do
  (Outputs outs) <- getOutputs mod ? "Module.outputsHaveSig"
  return $ sig `elem` outs

partInInputs :: Module -> Part -> J Bool
partInInputs mod comp =
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
