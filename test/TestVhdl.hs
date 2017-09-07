{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module TestVhdl where

import Text.Format
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Part as Part
import qualified Jade.Vhdl as Vhdl
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified System.Directory as SD
import Control.Monad
import Data.FileEmbed
import Data.Text.Encoding 
import Jade.Types
import TestUtil
import Jade.Util

import System.Process 
import System.Exit      
import Control.Exception.Base as CEB
import Jade.Rawr.Types
import qualified System.IO as SIO
       
spawnOneTest jadefile modname = do
  let autoTestPath = format "test-data/auto-vhdl/{0}/" [hashid modname]
      tbname = Module.testBenchName modname
      --prelude = decodeUtf8 $(embedFile "app-data/vhdl/prelude.vhdl")
      outfile = autoTestPath ++ (hashid modname) ++ ".vhdl"
      
  SD.removePathForcibly autoTestPath
  SD.createDirectoryIfMissing False "test-data/auto-vhdl"
  SD.createDirectory autoTestPath
  Right topl <- Decode.decodeTopLevel jadefile
  errlog <- runJIO topl $ do
    nb $ "spawnOneTest: " ++ modname
    moduleCode <- Vhdl.mkAllMods modname
    testCode <- Vhdl.mkTestBench modname    
    return $ TIO.writeFile outfile (T.concat [moduleCode, testCode])
    
  let sh s = (shell s) { cwd = Just autoTestPath , std_out = CreatePipe , std_err= CreatePipe }
  let preludePath = "../../../app-data/vhdl/prelude.vhdl"
      cmd1 = sh $ format "ghdl -a -g --std=08 {0} *.vhdl" [preludePath]
      cmd2 = sh $ format "ghdl -r --std=08 {0} --vcd={0}.vcd" [tbname]
  
  (ecode, stdout, stderr) <- readCreateProcessWithExitCode cmd1 ""
  case ecode of
    ExitSuccess -> do 
      (ecode', stdout', stderr') <- readCreateProcessWithExitCode cmd2 ""
      case ecode' of
        ExitSuccess -> return Pass
        ExitFailure err' -> do return $ Fail $ unlines [ format "module: {0}" [modname]
                                                       , format "ecode:  {0}" [show ecode']
                                                       , errlog
                                                       , stdout'
                                                       , stderr'
                                                       , show err'
                                                       ]
                                 
    ExitFailure err -> do return $ Fail $ unlines [ format "module: {0}" [modname]
                                                  , format "ecode:  {0}" [show ecode]
                                                  , format "stdout: {0}" [stdout]
                                                  , format "stdout: {0}" [stderr]
                                                  , errlog 
                                                  , show err
                                                  ]

testTree = TestTree "Vhdl" [ testTreeReplication
                           , testTreeBuffer
                           , testTreeMemUnit
                           , testTree1
                           , testTree2
                           ]
                            
testTreeBuffer =
  let node s = TestCase s (spawn (ModPath "./test-data" s))
  in TestTree "Buffers" $ map node [ "Buffer4"
                                   , "Buffer5"
                                   , "Buffer6"
                                   , "WonkyBuffer1"
                                   ]

testTreeMemUnit = let node s = TestCase s (spawn (ModPath "./test-data" s))
            in TestTree "MemUnit" $ map node [ "MemUnit1"
                                             , "MemUnitMoved1"
                                             , "MemUnitRotate1"
                                             , "MemUnitRotate2"
                                             , "MemUnit2" ]

testTree1 = let node s = TestCase s (spawn (ModPath "./test-data" s))
            in TestTree "One" $ map node [ "Jumper1"
                                         , "SimpleJumper21"
                                         , "And41"
                                         , "AndStuff4" 
                                         , "AndStuff5"
                                         , "Jumper41"
                                         , "Jumper1"
                                         , "Jumper1Rot90"
                                         , "Jumper3"
                                         , "AND2"
                                         , "AND2Rot90"
                                         , "And2Ports"
                                         , "And2Ports2"
                                         , "And2Ports3"
                                         , "And2Ports4"
                                         , "Constant1"
                                         , "Buffer1"
                                         , "Buffer2"
                                         , "WireConnectMid1"
                                         , "WireConnectMid2"
                                         , "CLA1_notext"
                                         , "CLA1"
                                         , "Mux2to1_1"
                                         , "Buffer3"
                                         , "BuiltInAnd4"
                                         , "BuiltInAnd4Messy"
                                         , "LeReg1"
                                         , "CLwiresAdded"
                                         , "CL"
                                         , "CLA4"
                                         , "fast_and4"
                                         , "CLA32"
                                         , "GarrInc4"
                                         , "AndStuff6"
                                         , "zreg"
                                         , "zreg2"
                                         , "CycleIdentity1"
                                         , "FreqDivider"
                                         , "FA1"
                                         
                                         -- , "CycleCounter"
                                         --, "GarrInc32"
                                         ]

testTreeReplication =
  let node s = TestCase s (spawn (ModPath "./test-data" s))
  in TestTree "Replication" $
  map node [ "RepAnd2"
           , "RepAnd3"
           , "RepAnd4"
           , "Mux21Rep4"
           , "Mux21Rep32"
           , "Mux4Rep1"
           , "Buffer7"
           , "RepWonkyBuffer1Exp"
           , "RepWonkyBuffer1"
           --, "ZipReplication"
           --, "Rep1FA2"
           --, "Ripple32"
           --, "Bool2"                                       
           ]
  
spawn (ModPath path filename) =
  let testPath = path ++ "/" ++ filename ++ ".json"
      modname = "/user/" ++ filename
  in spawnOneTest testPath modname

testTree2 =
  let testcase filename modname =
        TestCase (slashesToScores modname) (spawnOneTest filename modname)
  in TestTree "Two" [ testcase "./test-data/SubmodAnd6.json" "/user/submod/and6"
                    , testcase "./test-data/SubmodAnd6.json" "/user/submod/and6"
                    ]


testDUT_UseAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/use-and2-3.json"
  runJIO topl $ "testDUT" <? do
    let modname = "/user/UseAND2_3"
    m <- TopLevel.getModule modname 
    xs <- Vhdl.mkDUT m modname
    return (putStrLn xs)

testGenVhdlUseAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/use-and2-3.json"
  runJIO topl $ "testGenVhdlUseAnd23" <? do
    let modname = "/user/UseAND2_3"
    liftM TIO.putStrLn $ Vhdl.mkCombinationalTest modname

testGenMakeModuleUseAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/use-and2-3.json"
  runJIO topl $ "testGenVhdlUseAnd23" <? do
    let modname = "/user/UseAND2_3"
    liftM TIO.putStrLn $ Vhdl.mkModule modname

testGenMakeModuleAnd41 = do
  Right topl <- Decode.decodeTopLevel "./test-data/And41.json"
  runJIO topl $ "testGenMakeModuleAnd41" <? do
    let modname = "/user/And41"
    liftM TIO.putStrLn $ Vhdl.mkModule modname

testGenMakeModuleAnd41SubMods = do
  Right topl <- Decode.decodeTopLevel "./test-data/And41.json"
  runJIO topl $ "testGenMakeModuleAnd41" <? do
    let modname = "/user/And41"
    subs <- TopLevel.getSubModules modname
    liftM print $ mapM (Vhdl.mkSubModuleInstance modname) subs

testGenMakeModuleAnd41NodeDecls = do
  Right topl <- Decode.decodeTopLevel "./test-data/And41.json"
  runJIO topl $ "testGenMakeModuleAnd41NodeDecls" <? do
    let modname = "/user/And41"
    liftM print $ Vhdl.mkNodeDecls modname

testGenMakeModule = do
  Right topl <- Decode.decodeTopLevel "./test-data/And41.json"
  runJIO topl $ "testGenMakeModuleAnd41NodeDecls" <? do
    let modname = "/user/And41"
        prelude = decodeUtf8 $(embedFile "app-data/vhdl/prelude.vhdl")
    moduleCode <- Vhdl.mkModule modname
    testCode <- Vhdl.mkTestBench modname 
  
    return $ do
      let outfile = "test-data/vhdl/mod1/" ++ (hashid modname) ++ ".vhdl"
      TIO.writeFile outfile (T.concat [ prelude, moduleCode, testCode ])

testConnectOutputJumper1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper1.json"
  runJIO topl $ "testConnectOutputJumper1" <? do
    let modname = "/user/Jumper1"
    liftM print $ Vhdl.connectOutput modname (SigSimple "vout")


