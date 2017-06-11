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

testGraphVoodoo = do
  Right tl <- Decode.decodeTopLevel "./test-data/and2-with-wires.json"
  let modname = "/user/AND2"
  case TopLevel.getModule tl modname of
    Just mod -> print $ Module.components mod
    Nothing -> print $ "couldn't find module: " ++ modname ++ " in toplevel"



