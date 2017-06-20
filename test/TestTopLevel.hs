module Test.TestTopLevel where

import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode

testTopLevelComponents1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-2.json"
  return $ TopLevel.numComponents topl "/user/UseAND2"

testTopLevelComponents2 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-2.json"
  return $ TopLevel.components topl "/user/UseAND2"

testTopLevelGetInputs :: IO ()
testTopLevelGetInputs = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  printJ $ TopLevel.getInputs topl "/user/UseAND2_3"
  printJ $ TopLevel.getOutputs topl "/user/UseAND2_3"
  printJ $ TopLevel.components topl "/user/UseAND2_3"



