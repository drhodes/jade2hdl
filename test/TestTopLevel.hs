module TestTopLevel where

import Jade.Types
import qualified Data.List as DL
import qualified Data.ByteString as DB
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Modul
import qualified Jade.Net as Net
import qualified Jade.Wire as Wire
import Text.Format
import TestUtil
import Control.Monad
import Jade.Rawr.Types 


bendyWire1 :: IO TestState
bendyWire1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/bendy-wire-1.json"
  let result = runJ topl $ do
        let modname =  "/user/BendyWire1"
        cs <- TopLevel.nets modname
        case length cs of
          1 -> return "+"
          x -> die $ "hmm, found: " ++ show x

  case result of
    Right _ -> return Pass
    Left msg -> return $ Fail msg


testTermDriverAnd23_Wire :: IO TestState
testTermDriverAnd23_Wire = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  let result = runJ topl $ do
        let modname =  "/user/UseAND2_3"
        subs <- TopLevel.getSubModules modname
        let submodule@(SubModule subname subloc) = subs !! 0
        inputTerms <- TopLevel.getInputTerminals submodule
        result <- mapM (TopLevel.getInputTermDriver modname) inputTerms
        case result of
          [(Just (ValIndex "A" 0)), (Just (ValIndex "B" 0))] -> return Pass
          x -> return $ Fail $ runLog topl $ die $ "hmm, found: " ++ show x
  case result of
    Right x -> return x
    Left msg -> return $ Fail msg

testGetNetsWithName :: String -> String -> Int -> IO TestState
testGetNetsWithName modname signame exp = do
  Right topl <- Decode.decodeTopLevel $ "./test-data/" ++ modname ++ ".json"
  let modname' =  "/user/" ++ modname
      cs = runJ topl $ TopLevel.getNetsWithName modname' signame
  case cs of
    Left msg -> return $ Fail msg
    Right cs -> if length cs == exp
                then return Pass
                else return $ Fail (show ("expected", exp, "got", length cs))

testTopLevelGetInputs :: IO TestState
testTopLevelGetInputs = do
  let modname =  "/user/UseAND2_3"
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  let func =do cs <- TopLevel.nets modname
               subs <- TopLevel.getSubModules modname
               -- pick a submodule with anonymous wires connected to
               -- two other submodule outputs
               let subm = subs !! 2
               -- get the input terminals of the chosen submodule.
               terms <- TopLevel.getInputTerminals subm
               nb "!! Terms"
               list terms
               -- pick the first input terminal.
               let term = terms !! 0
               nbf "EE Chosen term: {0}" [show term]
               
               -- find the connected nets to that input terminal.
               connected <- TopLevel.netWithTerminal modname term

               nbf "EE Connected: {0}" [show connected]

               -- find which signal is driving the input terminal.
               driver <- TopLevel.getInputTermDriver modname (terms !! 1)
               if driver == (Just $ ValIndex "LdyPxAwJGq0vO_RESERVED_OUT" 0)
                 then return Pass
                 else do nb $ show driver
                         die $ "driver `not equal to` what it should be"
  case runJ topl func of
    Right x -> return x
    Left msg -> return $ Fail $ runLog topl func

{-               
testSigConnectedToSubModuleP1 :: IO TestState
testSigConnectedToSubModuleP1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper1.json"
  let func = do Outputs outs <- TopLevel.getOutputs "/user/Jumper1"
                eh <- TopLevel.sigConnectedToSubModuleP "/user/Jumper1" (outs !! 0)
                if eh == False
                  then return Pass
                  else return $ Fail "no message"
  return $ case runJ topl func of
             Right x -> x
             Left msg -> Fail msg

testSigConnectedToSubModuleP2 = do
  Right topl <- Decode.decodeTopLevel "./test-data/UseAND2_3.json"
  let func = do
        let modname =  "/user/UseAND2_3"
        Outputs outs <- TopLevel.getOutputs modname
        v <- mapM (TopLevel.sigConnectedToSubModuleP modname) outs
        if v == [True] then return Pass else return (Fail "no message")
  return $ case runJ topl func of
             Right x -> x
             Left msg -> Fail msg

testLoneJumper1 :: IO TestState
testLoneJumper1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/LoneJumper1.json"
  case runJ topl $ TopLevel.nets "/user/LoneJumper1" of
    Right comps -> if (map Net.getSigs comps) == [[]]
                   then return Pass
                   else return $ Fail $ "FAIL: unexpected result in testLoneJumper1"
    Left msg -> return $ Fail msg


testNetUseAND2Rot90 = do
  let modname = "UseAND2Rot90"
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let expected = [ [SigSimple "OUT1",SigSimple "USEOUT1"]
                 , [SigSimple "IN1",SigSimple "USEIN1"]
                 , [SigSimple "IN2",SigSimple "USEIN2"]]

  let func = do comps <- TopLevel.nets ("/user/" ++ modname)
                return $ map Net.getSigs comps
  case runJ topl func of
    Right x -> return Pass
    Left msg -> return $ Fail msg
-}
testTreeMiscEtc =
  let t name f = TestCase name f
  in TestTree "MiscEtc" [-- t "testTermDriverAnd23_Wire" testTermDriverAnd23_Wire
    t "bendyWire1" bendyWire1
    , t "portTest1" portTest1
      --, t "testTopLevelGetInputs" testTopLevelGetInputs
      -- , t "testSigConnectedToSubModuleP1" testSigConnectedToSubModuleP1
      -- , t "testSigConnectedToSubModuleP2" testSigConnectedToSubModuleP2
      -- , t "testLoneJumper1" testLoneJumper1
    ]
  {-
testNets :: String -> [[Sig]] -> IO TestState
testNets modname exp = do
  Right topl <- Decode.decodeTopLevel $ format "./test-data/{0}.json" [modname]
  let func = do
        comps <- TopLevel.nets $ format "/user/{0}" [modname]
        let r1 = DL.sort exp
            r2 = DL.sort $ map Net.getSigs comps
        if r1 == r2
          then return True
          else do expected exp (map Net.getSigs comps)
                  list comps
                  return False
  case runJ topl func of
    Left msg -> return $ Fail msg
    Right True -> return Pass
    Right False -> return $ Fail $ runLog topl func
-}
testReplicationDepth :: String -> Int -> IO TestState
testReplicationDepth modname expDepth = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let func = do
        let parentModuleName = "/user/" ++ modname
        subs <- TopLevel.getSubModules parentModuleName
        let sub = subs !! 0
        d <- TopLevel.replicationDepth ("/user/" ++ modname) sub
        nb $ show d
        if (expDepth == d)
          then return ()
          else die $ format "expected: {0}, got: {1}" [show expDepth, show d]
        
  case runJ topl func of
    Right _ -> return Pass
    Left msg -> return $ Fail $ runLog topl func ++ msg

testNumNets2 modname numcomps = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  
  let func = do
        comps <- TopLevel.nets ("/user/" ++ modname)
        let wires = map Wire.ends (concat $ map Net.getWires comps)                 
        return (length comps, comps)
        
  case runJ topl func of
    Right (n, comps) ->
      if n == numcomps
      then return Pass
      else return $ Fail $ unlines [ format "{2}: Expected {0}, got: {1}" [show numcomps, show n, modname]
                                   , runLog topl func] 
    Left msg -> return $ Fail msg

testTreeNumNets = 
  let t modname exp = TestCase modname (testNumNets2 modname exp)
  in TestTree "testNumNets"
  [ t "Jumper4" 1
  , t "Jumper5" 1
  , t "Jumper21" 3
  , t "Jumper41" 7
  , t "Jumper3" 1
  , t "Wires2" 1
  , t "Wires3" 1
  , t "Wires4" 1
  , t "Wires5" 1
  , t "Wires6" 1
  , t "Wire1" 1
  , t "RepAnd2" 3
  , t "And2Ports2" 3
  , t "And2Ports" 3
  , t "And2Ports4" 3
  , t "JumperPort1" 1
  , t "JumperPort2" 1
  , t "Nor32Arith5" 6
  ] 

testTree = TestTree "TopLevel" [ testTreeNumNets
                               , testTreeGetNetsWithNameAll
                               --, testTreeTerminals
                               , testTreeReplicationDepth
                               , testTreeGetWidthOfSigName
                               , testTreeMiscEtc
                               ]

testTreeGetNetsWithNameAll =
  let t modname signame exp = TestCase modname (testGetNetsWithName modname signame exp)
  in TestTree "getNetsWithName" [ t "RepAnd2" "IN2" 1
                                , t "RepAnd2" "IN1" 1
                                , t "RepAnd2" "OUT1" 1
                                , t "RepAnd2" "FARFENNUGEN" 0
                                , t "Jumper1" "A" 1
                                , t "Jumper1" "VOUT" 1
                                , t "BuiltInAnd4Messy" "VOUT" 1                                
                                ]

testTreeReplicationDepth =
  let t modname exp = TestCase modname (testReplicationDepth modname exp)
  in TestTree "getReplicationDepth" [ t "And2Ports" 1 
                                    , t "And2Ports2" 1
                                    , t "And2Ports3" 1
                                    , t "And2Ports4" 1
                                    , t "RepAnd2" 2
                                    , t "RepAnd3" 4
                                    , t "RepAnd4" 4
                                    , t "RepWonkyBuffer1" 2
                                    , t "RangeStep2" 8
                                    ]

testGetWidthOfSigName modname signame expectedWidth = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let func = do
        w <- TopLevel.getWidthOfValName ("/user/" ++ modname) signame
        if w == expectedWidth
          then return Pass
          else do let log = runLog topl func
                      msg = format "Expected width {0}, got {1}, {2}" [ show expectedWidth
                                                                      , show w
                                                                      , log
                                                                      ]
                  return $ Fail msg
          
  case runJ topl func of
    Right state -> return state
    Left msg -> return $ Fail $ runLog topl func ++ msg
  
     
testTreeGetWidthOfSigName = 
  let t modname signame exp = TestCase modname (testGetWidthOfSigName modname signame exp)
  in TestTree "GetWidthOfSigName" [ t "Rep1FA2" "CO" 1 
                                  , t "Rep1FA2" "S" 2
                                  , t "Rep1FA2" "A" 2
                                  , t "Rep1FA2" "B" 2
                                  , t "Rep1FA2" "CIN" 1
                                  , t "Rep1FA2" "COUT" 1
                                  , t "Buffer7" "X" 1
                                  , t "Buffer7" "OUT1" 3
                                  ]
{-
testTreeTerminals = let t name f = TestCase name testTerminals1
  in TestTree "terminals" [ t "testTerminals1" testTerminals1
                          ]
-}

{-
testTerminals1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/MemUnit1.json" 
  let modname =  "/user/MemUnit1"
      name = "Mem1"
      loc = Coord3 0 0 Rot0
      contents = "0\n1"
      (numports, naddr, ndata) = (1,1,1)
      
      func = TopLevel.terminals (SubMemUnit (MemUnit name loc contents numports naddr ndata))
      cs = runJ topl func
      
      exp = [ Terminal (Coord3 {c3x = 0, c3y = 0, c3r = Rot0}) (SigSimple "ADDR_PORT1")
            , Terminal (Coord3 {c3x = 72, c3y = 0, c3r = Rot0}) (SigSimple "DATA_PORT1")
            , Terminal (Coord3 {c3x = 0, c3y = 8, c3r = Rot0}) (SigSimple "OE_PORT1")
            , Terminal (Coord3 {c3x = 0, c3y = 16, c3r = Rot0}) (SigSimple "WE_PORT1")
            , Terminal (Coord3 {c3x = 0, c3y = 24, c3r = Rot0}) (SigSimple "CLK_PORT1")
            ]
            
  case cs of
    Right cs -> if DL.sort cs == DL.sort exp
                then return Pass
                else return $ Fail $ runLog topl (func >> (expected exp cs))
    Left msg -> return $ Fail msg
-}

portTest1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/port-test-1.json" 
  let modname =  "/user/PortTest1"
      cs = runJ topl $ TopLevel.nets modname
  case cs of
    Right cs -> case length cs of
                  1 -> return Pass
                  x -> return $ Fail $ show (runLog topl $ die $ "hmm, found: " ++ show x)
    Left msg -> return $ Fail msg

------------------------------------------------------------------
-- CHECKS
{-
checkJumper21nets = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper21.json"
  case runJ topl $ do TopLevel.nets "/user/Jumper21" of
    Right comps -> print $ map Net.getSigs comps
    Left msg -> fail msg

checkNets modname = do
  Right topl <- Decode.decodeTopLevel $ format "./test-data/{0}.json" [modname]
  case runJ topl (TopLevel.nets (format "/user/{0}" [modname])) of
    Right comps -> mapM_ print $ map Net.getSigs comps
    Left msg -> fail msg

checkJumper41 = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper41.json"
  putStrJ topl $ do
    let modname =  "/user/Jumper41"
    liftM show $ TopLevel.nets modname

checkDependencyOrder1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/CLA32.json"
  printJ topl $ do
    let modname =  "/user/CLA32"
    cs <- TopLevel.dependencyOrder modname
    return cs 

buildUserAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-3.json"
  printJ topl $ do
    let modname =  "/user/UseAND2_3"
    cs <- TopLevel.nets modname
    return $ cs !! 4

buildUserAnd24 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-4.json"
  putStrJ topl $ do
    let modname =  "/user/UseAND2_4"
    cs <- TopLevel.nets modname
    let (Net gid cs') = cs !! 5
    return $ show $ map nodePart $ cs'

checkTopLevelNets = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-2.json"
  return $ TopLevel.nets "/user/UseAND2"

checkTopLevelNets1 :: IO (J Int)
checkTopLevelNets1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/user-and2-2.json"
  return $ TopLevel.numNets "/user/UseAND2"

-}
