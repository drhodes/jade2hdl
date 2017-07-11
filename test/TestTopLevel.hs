module Test.TestTopLevel where

import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Modul
import qualified Jade.GComp as GComp

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
              let (GComp cs') = cs !! 5
              return $ map nodePart $ cs'

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

testSigConnectedToSubModuleP1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper1.json"

  printJ $ do
    Outputs outs <- TopLevel.getOutputs topl "/user/Jumper1"
    eh <- TopLevel.sigConnectedToSubModuleP topl "/user/Jumper1" (outs !! 0)
    return eh

testSigConnectedToSubModuleP2 = do
  Right topl <- Decode.decodeTopLevel "./test-data/UseAND2_3.json"
  printJ $ do
    let modname =  "/user/UseAND2_3"
    Outputs outs <- TopLevel.getOutputs topl modname
    mapM (TopLevel.sigConnectedToSubModuleP topl modname) outs

testJumper41 = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper41.json"
  printJ $ do
    let modname =  "/user/Jumper41"
    TopLevel.components topl modname

testJumper41NumComponents = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper41.json"
  return $ TopLevel.numComponents topl "/user/Jumper41"


testJumper3NumComponents = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper3.json"
  return $ TopLevel.numComponents topl "/user/Jumper3"

testJumper3Components = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper3.json"
  printJ $ TopLevel.components topl "/user/Jumper3"

testJumper4NumComponents = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper4.json"
  case runJ $ TopLevel.numComponents topl "/user/Jumper4" of
    Right 1 -> putStrLn "PASS"
    Right x -> putStrLn $ "Expected 1, got: " ++ show x
    Left msg -> fail msg

testJumper5NumComponents = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper5.json"
  case runJ $ do comps <- TopLevel.numComponents topl "/user/Jumper5"
                 return comps
    of
    Right 1 -> putStrLn "PASS"
    Right x -> putStrLn $ "Expected 1, got: " ++ show x
    Left msg -> fail msg

testJumper21NumComponents = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper21.json"
  case runJ $ TopLevel.numComponents topl "/user/Jumper21"
    of
    Right 3 -> putStrLn "PASS"
    Right x -> putStrLn $ "Expected 3, got: " ++ show x
    Left msg -> fail msg

testJumper21components = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper21.json"
  case runJ $ do TopLevel.components topl "/user/Jumper21" of
    Right comps -> print $ map GComp.getSigs comps
    Left msg -> fail msg

testJumper1components = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper1.json"
  case runJ $ do TopLevel.components topl "/user/Jumper1" of
    Right comps -> print (map GComp.getSigs comps)
    Left msg -> fail msg

testLoneJumper1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/LoneJumper1.json"
  case runJ $ do TopLevel.components topl "/user/LoneJumper1" of
    Right comps -> print $ (map GComp.getSigs comps) == [[]]
    Left msg -> fail msg
