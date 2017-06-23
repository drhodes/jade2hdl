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
  -- printJ $ TopLevel.getInputs topl modname
  -- printJ $ TopLevel.getOutputs topl modname
  printJ $ do cs <- TopLevel.components topl modname
              subs <- TopLevel.getSubModules topl modname
              let subm = subs !! 2
              terms <- TopLevel.getInputTerminals topl subm
              let term = terms !! 0
              connected <- TopLevel.componentWithTerminal topl modname term
              return $ filter (/= (TermC term)) $ map nodePart connected
              --TopLevel.getInputTermDriver topl modname (terms !! 0)
  
