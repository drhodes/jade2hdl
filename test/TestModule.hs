module TestModule where

import qualified Data.Map as DM
import qualified Language.VHDL.Syntax as S
import qualified Language.VHDL.Pretty as P
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Vhdl as Vhdl
import qualified Data.Hashable as H


testEntityDecl = do
  Right (TopLevel m) <- Decode.decodeTopLevel "./test-data/and2.json"
  let pair = head $ DM.toList m
  print $ P.pp $ Vhdl.mkEntityDecl pair

testMkArchBody = do
  Right (TopLevel m) <- Decode.decodeTopLevel "./test-data/and2.json"
  let pair = head $ DM.toList m
  print $ P.pp $ Vhdl.mkArchBody pair

testGraphVoodoo filename modname = do
  Right tl <- Decode.decodeTopLevel filename
  case TopLevel.getModule tl modname of
    Just mod -> do
      let comps = Module.components mod
      print comps
      print $ (filename, modname, "Numcomponents", length comps)
    Nothing -> print $ "couldn't find module: " ++ modname ++ " in toplevel"

testGraphAnd2 = testGraphVoodoo "./test-data/and2-with-wires.json" "/user/AND2"
testGraphPortWire = testGraphVoodoo "./test-data/port-wire.json" "/user/port_wire"
testGraphPortWireMovedUp =
  testGraphVoodoo "./test-data/port-wire-moved-up.json" "/user/port_wire"

testGraphSubModule filename modname = do
  Right tl <- Decode.decodeTopLevel filename
  case TopLevel.getModule tl modname of
    Just (Module schem _) -> print $ schem
    Nothing -> print $ "couldn't find module: " ++ modname ++ " in toplevel"
    
testGraphSubModuleAnd2 =
  testGraphSubModule "./test-data/and2-with-wires.json" "/user/AND2"
