module Test.TestTopLevel where

import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module

testWires2 = do
  -- a box made of wires, should have one component
  Right topl <- Decode.decodeTopLevel "./test-data/wires2.json"
  printJ $ do let modname =  "/user/Wires2"
              subs <- TopLevel.getSubModules topl modname
              let submodule@(SubModule subname subloc) = subs !! 2
              cs <- TopLevel.components topl modname
              case length cs of
                1 -> return cs
                x -> die $ "there should only be one component: " ++ show x

testWires21 = do
  -- a box made of wires, should be have one component
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
              result <- mapM (TopLevel.getInputTermDriver topl modname) inputTerms
              case result of
                [SigSimple "A",SigSimple "B"] -> return "Pass"
                x -> die $ "hmm, found: " ++ show x

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
  
