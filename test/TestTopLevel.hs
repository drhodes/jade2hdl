module TestModule where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Language.VHDL.Syntax as S
import qualified Language.VHDL.Pretty as P
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Vhdl as Vhdl
import qualified Data.Hashable as H
import Data.List
import qualified Data.Char as Char
import GHC.Stack

testTopLevelComponents1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-2.json"
  return $ TopLevel.numComponents topl "/user/UseAND2"

testTopLevelComponents2 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-2.json"
  return $ TopLevel.components topl "/user/UseAND2"

testTopLevelGetInputs = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  print $ TopLevel.getInputs topl "/user/UseAND2_3"
  print $ TopLevel.getOutputs topl "/user/UseAND2_3"
  return $ TopLevel.components topl "/user/UseAND2_3"
  
