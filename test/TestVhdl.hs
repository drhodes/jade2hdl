{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module TestVhdl (testTree) where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import Data.FileEmbed
import Data.Text.Encoding 
import Jade.Common
import Rawr.Types
import System.Exit      
import System.Process 
import TestUtil
import Text.Format
import qualified Control.Exception.Base as CEB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Map as DM
import qualified Data.String.Class as DSC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Jade.Decode.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Part as Part
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Vhdl as Vhdl
import qualified System.Directory as SD
import qualified System.IO as SIO
import qualified Text.PrettyPrint.Leijen as P
       
spawnOneTest jadefile modname = do
  let autoTestPath = format "test-data/auto-vhdl/{0}/" [hashid modname]
      tbname = Module.testBenchName modname
      --prelude = decodeUtf8 $(embedFile "app-data/vhdl/prelude.vhdl")
      outfile = autoTestPath ++ (hashid modname) ++ ".vhdl"
      
  SD.removePathForcibly autoTestPath
  SD.createDirectoryIfMissing False "test-data/auto-vhdl"
  SD.createDirectory autoTestPath  
  Right topl <- Decode.decodeTopLevel jadefile
  
  let func = do
        nb $ "spawnOneTest: " ++ modname
        TopLevel.deanonymizeTopLevel
        moduleCode <- Vhdl.mkAllMods modname
        testCode <- Vhdl.mkTestBench modname    
        return $ TIO.writeFile outfile (T.concat [moduleCode, testCode])
  errlog <- runJIO topl func
    
  let sh s = (shell s) { cwd = Just autoTestPath , std_out = CreatePipe , std_err= CreatePipe }
  let preludePath = "../../../app-data/vhdl/prelude.vhdl"
      cmd1 = sh $ format "ghdl -a -g --std=08 {0} *.vhdl" [preludePath]
      cmd2 = sh $ format "ghdl -r --std=08 {0} --vcd={0}.vcd" [tbname]
  
  (ecode, stdout, stderr) <- readCreateProcessWithExitCode cmd1 ""
  case ecode of
    ExitSuccess -> do 
      (ecode', stdout', stderr') <- readCreateProcessWithExitCode cmd2 ""
      case ecode' of
        ExitSuccess -> do
          --writeCallGraph (format "logs/dots/{0}-PASS.dot" [hashid modname]) topl func
          return Pass        
        ExitFailure err' -> do
          --writeCallGraph (format "logs/dots/{0}-FAIL.dot" [hashid modname]) topl func
          return $ Fail $ P.text $ unlines [ format "[{ \"module\": {0}}, " [show modname]
                                           , format "{ \"hashcode\": {0}}, " [show $ hashid modname]
                                           , format "{ \"ecode\":  {0}}, " [show $ show ecode']
                                           , format "{ \"errlog\": {0}}, " [show errlog]
                                           , format "{ \"stdout\": {0}}, " [show $ show stdout']
                                           , format "{ \"stderr\": {0}}, " [show stderr']
                                           , format "{ \"err\": {0}} ] " [ show $ show err']
                                           ]
                                 
    ExitFailure err -> do
      --writeCallGraph (format "/tmp/dots/{0}-FAIL.dot" [hashid modname]) topl func
      return $ Fail $ P.text $ unlines [ format "[{ \"module\": {0}}, " [show modname]
                                       , format "{ \"ecode\":  {0}}, " [show $ show ecode]
                                       , format "{ \"errlog\": {0}}, " [show errlog]
                                       , format "{ \"stdout\": {0}}, " [show $ show stdout]
                                       , format "{ \"stderr\": {0}}, " [show stderr]
                                       , format "{ \"err\": {0}} ] " [ show $ show err]
                                       ]

node s = TestCase s (spawn (ModPath "./test-data" s))
tree s xs = TestTree s $ Prelude.map node xs

testTree = TestTree "Vhdl" [ testTreeInnerSignal
                           -- , testTreeReplication
                           -- , testTreeBuffer
                           -- , testTreeBeta
                           -- , testTreeJumpers
                           -- , testTreeMisc
                           -- , testTree1
                           -- , testTree2
                           -- , testTreeMemUnit
                           ]

testTreeReplication = tree "Replication" [ "Rep1FA2"
                                         , "RepAnd2"
                                         , "RepAnd3"
                                         , "RepAnd4"
                                         , "Mux21Rep4"
                                         , "Mux21Rep32" 
                                         , "Mux4Rep1"
                                         , "RepWonkyBuffer1Exp"
                                         , "RepWonkyBuffer1"
                                         , "ZipReplication"
                                         , "Bool2"
                                         , "Buffer7"
                                         , "Ripple3_1"
                                         , "Ripple3"
                                         --, "Ripple32"
                                         ]

testTreeInnerSignal = tree "InnerSignal" [ "InnerSignal1"
                                         , "InnerSignal2"
                                         ]
           
testTreeBeta = tree "Beta" [ "Bool2" 
                           , "Nor32Arith3"
                           , "Nor32Arith4"
                           , "Nor32Arith5"
                           , "Nor32Arith2"
                           , "Nor32Arith"
                           -- , "Shift1"
                           ]

testTreeBuffer = tree "Buffer" [ "Buffer1"
                               , "Buffer2"
                               , "Buffer3"
                               , "Buffer4"
                               , "Buffer5"
                               , "Buffer5_1"
                               , "Buffer5_2"
                               , "Buffer5_3"
                               , "Buffer6"
                               , "Buffer8"
                               , "WonkyBuffer1"
                               ]
                 
testTreeMisc = tree "Misc" [ "Vdd"
                           , "RangeStep1"
                           , "RangeStep2"
                           ]

testTreeMemUnit = tree "MemUnit" [ "MemUnit1"
                                 , "MemUnitMoved1"
                                 , "MemUnitRotate1"
                                 , "MemUnitRotate2"
                                 , "MemUnit2" ]

testTreeJumpers = tree "One" [ "Jumper1" 
                             , "SimpleJumper21"                             
                             , "Jumper41"
                             , "Jumper1Rot90"
                             , "Jumper3"
                             , "InternalJumper1"
                             ]

testTree1 = tree "One" [ "And41"
                       , "AndStuff4" 
                       , "AndStuff5"
                       , "AND2"
                       , "AND2Rot90"
                       , "And2Ports"
                       , "And2Ports2"
                       , "And2Ports3"
                       , "And2Ports4"
                       , "Constant1"
                       , "Constant2"
                       , "Constant3"
                       , "WireConnectMid1"
                       , "WireConnectMid2"
                       , "CLA1_notext"
                       , "CLA1"
                       , "Mux2to1_1" 
                       , "BuiltInAnd4"
                       , "BuiltInAnd4Messy"
                       , "LeReg1"
                       , "CLwiresAdded"
                       , "CL"
                       , "CLA4"
                       , "fast_and4"
                       , "FA1"
                       , "AndStuff6"
                       , "zreg"
                       , "zreg2"
                       -- , "CLA32" passed, but slowly.  why so slow?
                       , "FreqDivider"
                       , "GarrInc4"
                       , "CycleIdentity1"

                       -- these haven't worked yet.
                       -- , "CycleCounter"
                       -- , "GarrInc32"
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

{-
testDUT_UseAnd23 = do
  Right topl <- decodeTopLevel "./test-data/use-and2-3.json"
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


-}
