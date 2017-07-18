module Test.TestTopLevel where

import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Modul
import qualified Jade.GComp as GComp
import Text.Format

pass = putStrLn "PASS"


testWires2 = do
  -- a box made of wires, should have one component
  Right topl <- Decode.decodeTopLevel "./test-data/wires2.json"
  printJ $ do let modname =  "/user/Wires2"
              subs <- TopLevel.getSubModules topl modname
              let submodule@(SubModule subname subloc) = subs !! 2
              cs <- TopLevel.components topl modname
              case length cs of
                1 -> return "PASS" -- cs
                x -> die $ "there should only be one component: " ++ show x

testWires21 = do
  -- a box made of wires, should be have one component
  Right topl <- Decode.decodeTopLevel "./test-data/wires21.json"
  printJ $ do let modname =  "/user/Wires2"
              subs <- TopLevel.getSubModules topl modname
              let submodule@(SubModule subname subloc) = subs !! 2
              cs <- TopLevel.components topl modname
              case length cs of
                2 -> return "PASS" --cs
                _ -> die "there should be two components"

testWires22 = do
  -- two box made of wires, one copy pasted with rotation
  Right topl <- Decode.decodeTopLevel "./test-data/wires22.json"
  printJ $ do let modname =  "/user/Wires2"
              subs <- TopLevel.getSubModules topl modname
              let submodule@(SubModule subname subloc) = subs !! 2
              cs <- TopLevel.components topl modname
              case length cs of
                2 -> return "PASS" --cs
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
                _ -> die "there should be 1 component"

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
                [SigSimple "pg7Bj1XGp2OJ9_out",SigSimple "QxrKbYgWM4dLd_out"] -> return "PASS"
                --[SigSimple "pQ7aGyV4wvYWO_out",SigSimple "7nbK7QoR5Kpao_out"] -> return "Pass"
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
              driver <- TopLevel.getInputTermDriver topl modname (terms !! 1)
              if driver == SigSimple "QxrKbYgWM4dLd_out"
                then return "PASS"
                else return "FAIL"
                
testSigConnectedToSubModuleP1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper1.json"
  printJ $ do
    Outputs outs <- TopLevel.getOutputs topl "/user/Jumper1"
    eh <- TopLevel.sigConnectedToSubModuleP topl "/user/Jumper1" (outs !! 0)
    if eh == False then return "PASS" else return "FAIL"

testSigConnectedToSubModuleP2 = do
  Right topl <- Decode.decodeTopLevel "./test-data/UseAND2_3.json"
  printJ $ do
    let modname =  "/user/UseAND2_3"
    Outputs outs <- TopLevel.getOutputs topl modname
    v <- mapM (TopLevel.sigConnectedToSubModuleP topl modname) outs
    if v == [True] then return "PASS" else return "FAIL"
    
checkJumper41 = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper41.json"
  printJ $ do
    let modname =  "/user/Jumper41"
    TopLevel.components topl modname

testJumper4NumComponents = testNumComponents "Jumper4" 1
testJumper5NumComponents = testNumComponents "Jumper5" 1
testJumper21NumComponents = testNumComponents "Jumper21" 3
testJumper41NumComponents = testNumComponents "Jumper41" 7
testJumper3NumComponents = testNumComponents "Jumper3" 1

checkJumper21components = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper21.json"
  case runJ $ do TopLevel.components topl "/user/Jumper21" of
    Right comps -> print $ map GComp.getSigs comps
    Left msg -> fail msg

checkComponents modname = do
  Right topl <- Decode.decodeTopLevel $ format "./test-data/{0}.json" [modname]
  case runJ (TopLevel.components topl (format "/user/{0}" [modname])) of
    Right comps -> print $ map GComp.getSigs comps
    Left msg -> fail msg

testLoneJumper1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/LoneJumper1.json"
  case runJ $ do TopLevel.components topl "/user/LoneJumper1" of
    Right comps -> print $ (map GComp.getSigs comps) == [[]]
    Left msg -> fail msg

testWireWidth2 = do
  Right topl <- Decode.decodeTopLevel "./test-data/WireWidth2.json"
  case runJ $ do TopLevel.components topl "/user/WireWidth2" of
    Right comps -> print comps
    Left msg -> fail msg

testNumComponents modname numcomps = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  case runJ $ do comps <- TopLevel.numComponents topl ("/user/" ++ modname)
                 return comps
    of
    Right x -> if x == numcomps
               then putStrLn "PASS"
               else putStrLn $ format "Expected {0}, got: {1}" [show numcomps, show x]
    Left msg -> fail msg

testComponentUseAND2Rot90 = do
  let modname = "UseAND2Rot90"
  --let modname = "UseAND2"
  print modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let expected = [ [SigSimple "out1",SigSimple "useOut1"]
                 , [SigSimple "in1",SigSimple "useIn1"]
                 , [SigSimple "in2",SigSimple "useIn2"]]

  let func = do comps <- TopLevel.components topl ("/user/" ++ modname)
                die $ show $ map GComp.getSigs comps
                
  case runJ func of
    Right _ -> undefined
    Left msg -> putStrLn msg

  putStrLn $ runLog func

testAll = do
  testWires2 
  testWires21 
  testWires22 
  testWires23 
  testWires24 
  testWire1 
  testTermDriverAnd23 
  testTermDriverAnd23_Wire 
  testTopLevelComponents1 
  testTopLevelComponents2 
  testTopLevelGetInputs
  testTopLevelGetInputs 
  testSigConnectedToSubModuleP1 
  testSigConnectedToSubModuleP2 
  testJumper41NumComponents 
  testJumper3NumComponents 
  testJumper4NumComponents 
  testJumper5NumComponents 
  testJumper21NumComponents 
  testLoneJumper1 
  testWireWidth2 
  testComponentUseAND2Rot90 



