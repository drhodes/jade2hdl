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
import qualified Jade.UnionFind as UF

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

testWires2 = do
  -- a box made of wires, should be have on component
  Right topl <- Decode.decodeTopLevel "./test-data/wires2.json"
  printJ $ do let modname =  "/user/Wires2"
              subs <- TopLevel.getSubModules topl modname
              let submodule@(SubModule subname subloc) = subs !! 2
              cs <- TopLevel.components topl modname
              case length cs of
                1 -> return cs
                x -> die $ "there should only be one component: " ++ show x

testWires21 = do
  -- a box made of wires, should be have on component
  Right topl <- Decode.decodeTopLevel "./test-data/wires21.json"
  printJ $ do let modname =  "/user/Wires2"
              subs <- TopLevel.getSubModules topl modname
              let submodule@(SubModule subname subloc) = subs !! 2
              cs <- TopLevel.components topl modname
              case length cs of
                2 -> return cs
                _ -> die "there should be two components"

testWires22 = do
  -- two box made of wires, one copy pasted with rotation
  Right topl <- Decode.decodeTopLevel "./test-data/wires22.json"
  printJ $ do let modname =  "/user/Wires2"
              subs <- TopLevel.getSubModules topl modname
              let submodule@(SubModule subname subloc) = subs !! 2
              cs <- TopLevel.components topl modname
              case length cs of
                2 -> return cs
                _ -> die "there should be two components"

testWires23 = do
  -- two box made of wires, one copy pasted with rotation, moved to overlap
  -- should be one component.
  Right topl <- Decode.decodeTopLevel "./test-data/wires23.json"
  printJ $ do let modname =  "/user/Wires2"
              subs <- TopLevel.getSubModules topl modname
              cs <- TopLevel.components topl modname
              case length cs of
                1 -> return "Pass"
                _ -> die "there should be two components"

testWires24 = do
  -- large art made of wires, should be one component.
  Right topl <- Decode.decodeTopLevel "./test-data/wires24.json"
  printJ $ do let modname =  "/user/Wires2"
              subs <- TopLevel.getSubModules topl modname
              cs <- TopLevel.components topl modname
              case length cs of
                1 -> return "Pass"
                _ -> die "there should be two components"

{- In a world with subcomponents, wires of width one and simple signals -}
buildUserAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  printJ $ do let modname =  "/user/UseAND2_3"
              -- subs <- TopLevel.getSubModules topl modname
              -- let submodule@(SubModule subname subloc) = subs !! 2
              cs <- TopLevel.components topl modname
              return $ cs !! 4
              --return $ map nodePart $ cs !! 7

portTest1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/port-test-1.json"
  printJ $ do
    let modname =  "/user/PortTest1"
    cs <- TopLevel.components topl modname
    case length cs of
      1 -> return "Pass"
      x -> die $ "hmm, found: " ++ show x

buildUserAnd24 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-4.json"
  printJ $ do let modname =  "/user/UseAND2_4"
              cs <- TopLevel.components topl modname
              return $ map nodePart $ cs !! 5

bendyWire1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/bendy-wire-1.json"
  printJ $ do let modname =  "/user/BendyWire1"
              cs <- TopLevel.components topl modname
              case length cs of
                1 -> return "Pass"
                x -> die $ "hmm, found: " ++ show x

testWire1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/wires/wire1.json"
  printJ $ do let modname =  "/user/Wire1"
              cs <- TopLevel.components topl modname
              case length cs of
                1 -> return "Pass"
                x -> die $ "hmm, found: " ++ show x


testTermDriverAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  printJ $ do let modname =  "/user/UseAND2_3"
              subs <- TopLevel.getSubModules topl modname
              let submodule@(SubModule subname subloc) = subs !! 2
              inputTerms <- TopLevel.getInputTerminals topl submodule
              result <- mapM (TopLevel.getInputTermDriver topl modname) inputTerms
              case result of
                [SigSimple "pQ7aGyV4wvYWO_out",SigSimple "7nbK7QoR5Kpao_out"] -> return "Pass"
                x -> die $ "hmm, found: " ++ show x

testTermDriverAnd23_Wire = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  printJ $ do let modname =  "/user/UseAND2_3"
              subs <- TopLevel.getSubModules topl modname
              let submodule@(SubModule subname subloc) = subs !! 0
              inputTerms <- TopLevel.getInputTerminals topl submodule
              mapM (TopLevel.getInputTermDriver topl modname) inputTerms
