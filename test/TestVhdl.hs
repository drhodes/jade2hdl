module TestVhdl where

import qualified Data.Map as DM
import qualified Language.VHDL.Syntax as S
import qualified Language.VHDL.Pretty as P
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Part as Part
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

testMkArchBodyAnd2WithWires = do
  Right (TopLevel m) <- Decode.decodeTopLevel "./test-data/and2-with-wires.json"
  let pair = head $ DM.toList m
  print $ P.pp $ Vhdl.mkArchBody pair

testMkArchBodyUserAnd2_3 = do
  Right (TopLevel m) <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  let pair = head $ DM.toList m
  print $ P.pp $ Vhdl.mkArchBody pair


testMkBuiltIn name constructor = do
  print $ P.pp $ Vhdl.mkBuiltInEntityDecl name
  print $ P.pp $ Vhdl.mkBuiltInArchBody name constructor


testAllBuiltIn = do
  testMkBuiltIn "AND2" S.EAnd
  testMkBuiltIn "OR2" S.EOr
  testMkBuiltIn "XOR2" S.EXor
  testMkBuiltIn "NAND2" S.EAnd
  

{- In a world with subcomponents, wires of width one and simple signals -}

buildUserAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  printJ $ buildUserAnd23' topl

buildUserAnd23' topl = do
  let modname =  "/user/UseAND2_3"
  subs <- TopLevel.getSubModules topl modname
  let submodule@(SubModule subname subloc) = subs !! 2

  cs <- TopLevel.components topl modname
  return $ cs !! 0
  
  -- mod1 <- TopLevel.getModule topl subname
  
  -- inTerms <- Module.getInputTerminals mod1 subloc
  
  -- TopLevel.getInputTermDriver topl modname (inTerms !! 0)
  --let s = Part.sig x

  
