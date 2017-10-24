{-# LANGUAGE OverloadedStrings #-}
module TestTopLevel (testTree) where

import qualified Data.List as DL
import qualified Data.ByteString as DB
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Part as Part
import qualified Jade.Decode.Decode as Decode
-- import qualified Jade.Module as Modul
-- import qualified Jade.Net as Net
-- import qualified Jade.Wire as Wire
-- import Text.Format
-- import TestUtil
import Control.Monad
import Rawr.Types 
import Rawr.Rawr
import Jade.Common
import qualified Text.PrettyPrint.Leijen as P
import Data.Monoid
import TestUtil

testTree = TestTree "TopLevel" [ testTreeNumWireComponents
                               , testTreeReplicationDepth
                               -- , testTreeNameConnectedToTerminal
                               --   testTreeNumNets
                               -- , testTreeNumSubModules
                               -- , testTreeNumTerminals
                               -- , testTreeGetNetsWithNameAll
                               -- , testTreeConnectWiresWithSameSigName
                               -- , testTreeGetWidthOfSigName
                               -- , testTreeMiscEtc
                               -- , testTreeGetInternalSigNames
                               -- , testTreeGetAllIndexesWithName
                               -- , testTreeGetTerminalsAtPoint
                               ]

testNumWireComponents modname exp = do
  Right topl <- Decode.decodeTopLevel ("./test-data/" ++ modname ++ ".json")
  let func = do cs <- TopLevel.wireComponents ("/user/" ++ modname) --ok
                return (length cs)
  return $ case runJ topl func of
    Right x -> if x == exp
               then Pass
               else let log = runLog topl func
                    in Fail $ log <> (P.text $ "Expected: " ++ show exp ++ " got: " ++ show x)
    Left msg -> Fail msg

testTreeNumWireComponents =
  let t modname exp = TestCase modname (testNumWireComponents modname exp)
  in TestTree "numWireComponents" $
     [ t "Jumper21" 4
     , t "Jumper3" 3
     , t "Jumper41" 13                          
     , t "Jumper41Rot90" 13
     , t "Jumper5" 2 
     , t "Jumper6" 2
     , t "Jumper7" 1
     , t "Jumper8" 1
     , t "AnonWire3" 1
     , t "JumperPort1" 1
     , t "JumperPort2" 1
     , t "Mux21Rep32" 4
     , t "Mux4Rep1" 6
     , t "NoRepFA2" 9
     , t "Nor32Arith" 15
     , t "Nor32Arith2" 15
     , t "Nor32Arith4" 10
     , t "Nor32Arith5" 6
     , t "PortTest1" 1                          
     , t "RepBuffer1" 3
     , t "ShiftL1" 24
     , t "WireConnectMid2" 2
     ]

testReplicationDepth :: String -> Int -> IO TestState
testReplicationDepth modname expDepth = do
  Right topl <- Decode.decodeTopLevel $ "./test-data/" ++ modname ++ ".json"
  let func = do
        TopLevel.deanonymizeTopLevel
        let parentModuleName = "/user/" ++ modname
        subs <- TopLevel.getSubModules parentModuleName
        let sub = subs !! 0
        d <- TopLevel.replicationDepth ("/user/" ++ modname) sub
        nb $ show d
        if (expDepth == d)
          then return ()
          else die $ concat ["expected: ", show expDepth, " got: ", show d]
        
  case runJ topl func of
    Right _ -> return Pass
    Left msg -> return $ Fail $ runLog topl func <> msg

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
     
testNameConnectedToTerminal :: String -> String -> IO TestState
testNameConnectedToTerminal modname expName = do
  Right topl <- Decode.decodeTopLevel ("./test-data/" ++ modname ++ ".json")
  let func = do
        TopLevel.deanonymizeTopLevel
        terms <- TopLevel.getAllTerminals (qualifiedModName modname)
        list terms
        assertEq (length terms) 2 "There should only be 2 terminal in this module!"
        names <- mapM (TopLevel.getNameConnectedToTerminal (qualifiedModName modname)) terms
        nb $ show names
        if (expName `elem` names)
          then return True
          else die $ concat ["couldn't find: ", expName, "in name set: ", show names]
        
  case runJ topl func of
    Right True -> return Pass
    Right False -> return $ Fail $ runLog topl func 
    Left msg -> return $ Fail $ runLog topl func <> msg

testTreeNameConnectedToTerminal =
  let t modname expName = TestCase modname (testNameConnectedToTerminal modname expName)
  in TestTree "getNameConnectedToTerminal"
     [ t "Buffer1" "A" ]

-- foo2 = do
--   Right topl <- Decode.decodeTopLevel "./test-data/AnonWire1.json"
--   let func = do parts <- TopLevel.getAllPartsNoTerms "/user/AnonWire1"
--                 let connectors = filter Part.isNamedConnector parts
--                 TopLevel.buildPointPartTable connectors
--   case runJ topl func of
--     Right thing -> do print "RESULT"
--                       print thing
--     Left msg -> error msg

-- foo3 :: String -> Int -> IO ()
-- foo3 modname exp = do
--   Right topl <- Decode.decodeTopLevel (printf "./test-data/%s.json" modname)
--   let func = do cs <- TopLevel.wireComponents (printf "/user/%s" modname)
--                 return (length cs)
--   case runJ topl func of
--     Right x -> if x == exp
--                then print "ok"
--                else putStrLn $ printf "Expected %d: got: %d" exp x
--     Left msg -> error msg
    
{-
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
  in TestTree "MiscEtc" [ t "bendyWire1" bendyWire1
                        , t "portTest1" portTest1
                        ]
     

testNumNets2 modname numcomps = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  
  let func = do
        nets <- TopLevel.nets ("/user/" ++ modname)
        list nets
        return (length nets, nets)
        
  case runJ topl func of
    Right (n, comps) ->
      if n == numcomps
      then return Pass
      else do
        writeCallGraph (format "/tmp/{0}.dot" [modname]) topl func
        return $ Fail $ unlines [ format "{2}: Expected {0}, got: {1}" [show numcomps, show n, modname] 
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
  , t "RepAnd2" 3
  , t "Rep1FA2" 5
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
    Left msg -> return $ Fail $ unlines [msg ++ runLog topl func]
                           
testNumTerminals modname expected = do
  testExpGot modname expected $ do
    let qualModName = "/user/" ++ modname 
    allSubs <- TopLevel.getSubModules qualModName
    length <$> concatMapM TopLevel.terminals allSubs

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

testGetInternalSigNames modname expected = do
  testExpGot modname expected $ do
    TopLevel.getInternalSigNames (qualifiedModName modname)

testTreeGetInternalSigNames = TestTree "getinternalsigname" $
  let t name exp = TestCase name (testGetInternalSigNames name exp)
  in [ t "Rep1FA2" ["CO"]
     ]

testGetAllIndexesWithName modname name expected = do
  testExpGot modname expected $ do
    TopLevel.getAllIndexesWithName (qualifiedModName modname) name

testTreeGetAllIndexesWithName = TestTree "getallIndexesWithName" $
  let t testname signame exp = TestCase testname (testGetAllIndexesWithName testname signame exp)
  in [ t "Rep1FA2" "CO" [ValIndex "CO" 0]
     ]

testExplodeConnect1 = do
  let c1 = Coord5 0 0 Rot0 0 0
      c2 = Coord5 8 8 Rot0 8 8
      valBundle1 = Bundle [ValIndex "A" 1, ValIndex "A" 0]
      valBundle2 = Bundle [ValIndex "A" 1, ValIndex "B" 0]
      w1 = Wire c1 (Just $ Signal (Just valBundle1) 2 Nothing)
      w2 = Wire c2 (Just $ Signal (Just valBundle2) 2 Nothing)
  TopLevel.explodeConnect (w1, w2) 
     
testGetTerminalsAtPoint modname point expected = do
  testExpGot modname expected $ do    
    length <$> TopLevel.getTerminalsAtPoint (qualifiedModName modname) point

testTreeGetTerminalsAtPoint = TestTree "getTerminalsAtPoint" $
  let t modname point expNumPoints = TestCase modname (testGetTerminalsAtPoint modname point expNumPoints)
  in [ t "Buffer8" (Point 0 0) 0
     , t "Buffer8" (Point 0 0) 0
     , t "Buffer8" (Point 8 0) 1
     , t "CL" (Point 48 56) 2
     ]
-}
