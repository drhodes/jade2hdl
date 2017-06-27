module Test.TestTopLevel where

import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module

testTopLevelComponents1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-2.json"
  return $ TopLevel.numComponents topl "/user/UseAND2"

testTopLevelComponents2 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-2.json"
  return $ TopLevel.components topl "/user/UseAND2"

testTopLevelGetInputs :: IO ()
testTopLevelGetInputs = do
  let modname =  "/user/UseAND2_3"
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  printJ $ do cs <- TopLevel.components topl modname
              subs <- TopLevel.getSubModules topl modname
              -- pick a submodule with anonymous wires connected to
              -- two other submodule outputs
              let subm = subs !! 2
              -- get the input terminals of the chosen submodule.
              terms <- TopLevel.getInputTerminals topl subm
              -- pick the first input terminal.
              let term = terms !! 0
              -- find the connected components to that input terminal.
              connected <- TopLevel.componentWithTerminal topl modname term
              
              --return $ filter (/= (TermC term)) $ map nodePart connected

              -- find which signal is driving the input terminal.
              TopLevel.getInputTermDriver topl modname (terms !! 1)
  
