module TestTopLevel (testTree) where

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
import Jade.Common

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
        nets <- TopLevel.nets ("/user/" ++ modname)
        list nets
        return (length nets, nets)        

  
  writeCallGraph (format "/tmp/{0}.dot" [modname]) topl func

  
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
  , t "And2Ports2" 3
  , t "And2Ports" 3
  , t "And2Ports4" 3
  , t "JumperPort1" 1
  , t "JumperPort2" 1
  , t "Buffer1" 2
  , t "Buffer2" 2
  , t "Nor32Arith5" 6
  , t "RepAnd2" 9
  ] 

check topl func = case runJ topl func of
                    Left msg -> return $ Fail $ unlines [msg, runLog topl func]
                    Right state -> return Pass

testConnectWiresWithSameSigName modname exp = do
  let modpath =  (format "./test-data/{0}.json" [modname])
  let qualModname = "/user/" ++ modname
  Right topl <- Decode.decodeTopLevel modpath
  
  let func = do
        (Module _ (Just schem@(Schematic parts)) _ _) <- TopLevel.getModule qualModname
        wwssn <- TopLevel.connectWiresWithSameSigName parts
        enb wwssn        
        bail
  check topl func

testTreeConnectWiresWithSameSigName = 
  let t modname exp = TestCase modname (testConnectWiresWithSameSigName modname exp)
  in TestTree "ConnectWiresWithSameSigName" [t "RepAnd2" 9
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

portTest1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/port-test-1.json" 
  let modname =  "/user/PortTest1"
      cs = runJ topl $ TopLevel.nets modname
  case cs of
    Right cs -> case length cs of
                  1 -> return Pass
                  x -> return $ Fail $ show (runLog topl $ die $ "hmm, found: " ++ show x)
    Left msg -> return $ Fail msg

testNumTerminals modname expected = do
  testExpGot modname expected $ do
    let qualModName = "/user/" ++ modname 
    allSubs <- TopLevel.getSubModules qualModName
    length <$> concatMapM TopLevel.terminals allSubs
    
testExpGot modname expected f = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let func = do
        got <- f
        case got == expected of
          True -> return ()
          False -> let temp = "exp: {0}, got: {1}" 
                   in do enb got
                         die $ format temp [show expected, show got]
  case runJ topl func of
    Right x -> return Pass
    Left msg -> return $ Fail $ unlines [runLog topl func]

testTreeNumTerminals = TestTree "numTerminals" $
  let t name exp = TestCase name (testNumTerminals name exp)
  in [ t "RepAnd2" 3
     , t "Rep1FA2" 5
     , t "RangeStep2" 5
     , t "RangeStep1" 3
     , t "Nor32Arith5" 6 
     , t "Nor32Arith4" 12
     ]

testNumSubModules modname expected = do
  testExpGot modname expected $ do
    length <$> TopLevel.getSubModules (qualifiedModName modname)

testTreeNumSubModules = TestTree "numSubModules" $
  let t name exp = TestCase name (testNumSubModules name exp)
  in [ t "RepAnd2" 1
     , t "Rep1FA2" 1
     , t "Nor32Arith4" 4
     , t "Nor32Arith3" 3
     , t "Nor32Arith2" 4
     , t "Nor32Arith" 4
     , t "RepAnd4" 1
     , t "RepAnd3" 1
     , t "RangeStep2" 1  
     , t "RangeStep1" 1 
     , t "CLA32" 17
   ]

testTree = TestTree "TopLevel" [ testTreeNumNets
                               , testTreeNumSubModules
                               , testTreeNumTerminals
                               , testTreeGetNetsWithNameAll
                               , testTreeConnectWiresWithSameSigName
                               , testTreeReplicationDepth
                               , testTreeGetWidthOfSigName
                               , testTreeMiscEtc
                               ]
