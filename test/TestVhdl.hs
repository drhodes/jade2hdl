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
      prelude = decodeUtf8 $(embedFile "app-data/vhdl/prelude.vhdl")
      outfile = autoTestPath ++ (hashid modname) ++ ".vhdl"
      
  SD.removePathForcibly autoTestPath
  SD.createDirectoryIfMissing False "test-data/auto-vhdl"
  SD.createDirectory autoTestPath
  Right topl <- Decode.decodeTopLevel jadefile
  errlog <- runJIO $ do
    nb $ "spawnOneTest: " ++ modname
    moduleCode <- Vhdl.mkAllMods topl modname
    testCode <- Vhdl.mkTestBench topl modname
    
    return $ do
      TIO.writeFile outfile (T.concat [prelude, moduleCode, testCode])
      
  let sh s = (shell s) { cwd = Just autoTestPath , std_out = CreatePipe , std_err= CreatePipe }
  let cmd1 = sh "ghdl -a -g --std=08 *.vhdl"
      cmd2 = sh (format "ghdl -r --std=08 {0} --vcd={0}.vcd" [tbname])
  
  (ecode, stdout, stderr) <- readCreateProcessWithExitCode cmd1 ""
  case ecode of
    ExitSuccess -> do 
      (ecode', stdout', stderr') <- readCreateProcessWithExitCode cmd2 ""
      case ecode' of
        ExitSuccess -> passes >> return Pass
        ExitFailure err' -> do fails
                               return $ Fail $ unlines [ format "module: {0}" [modname]
                                                       , format "ecode:  {0}" [show ecode']
                                                       , errlog
                                                       , stdout'
                                                       , stderr'
                                                       , show err'
                                                       ]
    ExitFailure err -> do fails
                          return $ Fail $ unlines [ format "module: {0}" [modname]
                                                  , format "ecode:  {0}" [show ecode]
                                                  , format "stdout: {0}" [stdout]
                                                  , format "stdout: {0}" [stderr]
                                                  , errlog 
                                                  , show err
                                                  ]


testTree1 = let node s = TestNode (Case s (spawn (ModPath "./test-data" s)))
            in TestTree "One" $ map node [ "Jumper1" 
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
                                         , "RepAnd2"
                                         , "RepAnd3"
                                         , "RepAnd4"
                                         , "Buffer1"
                                         , "Buffer2"
                                         , "WireConnectMid1"
                                         , "WireConnectMid2"
                                         , "CLA1_notext"
                                         , "CLA1"
                                         , "Mux2to1_1"
                                         , "Buffer3"
                                         , "Buffer6"
                                         , "Buffer4"
                                         , "Buffer5"
                                         , "BuiltInAnd4"
                                         , "BuiltInAnd4Messy"
                                         , "LeReg1"
                                         , "CLwiresAdded"
                                         , "CL"
                                         , "CLA4"
                                         , "fast_and4"
                                         , "CLA32"
                                         , "GarrInc4"
                                         , "MemUnit1"
                                         --, "GarrInc32"
                                         ]
  
spawn (ModPath path filename) =
  let testPath = path ++ "/" ++ filename ++ ".json"
      modname = "/user/" ++ filename
  in spawnOneTest testPath modname

testTree2 =
  let testcase filename modname =
        TestNode (Case (slashesToScores modname) (spawnOneTest filename modname))
  in TestTree "Two" [ testcase "./test-data/SubmodAnd6.json" "/user/submod/and6"
                    , testcase "./test-data/SubmodAnd6.json" "/user/submod/and6"
                    ]


testTree = TestTree "Vhdl" [ testTree1
                           , testTree2 ]


testDUT_UseAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/use-and2-3.json"
  runJIO $ "testDUT" <? do
    let modname = "/user/UseAND2_3"
    m <- TopLevel.getModule topl modname 
    xs <- Vhdl.mkDUT m modname
    return (putStrLn xs)

testGenVhdlUseAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/use-and2-3.json"
  runJIO $ "testGenVhdlUseAnd23" <? do
    let modname = "/user/UseAND2_3"
    liftM TIO.putStrLn $ Vhdl.mkCombinationalTest topl modname

testGenMakeModuleUseAnd23 = do
  Right topl <- Decode.decodeTopLevel "./test-data/use-and2-3.json"
  runJIO $ "testGenVhdlUseAnd23" <? do
    let modname = "/user/UseAND2_3"
    liftM TIO.putStrLn $ Vhdl.mkModule topl modname

testGenMakeModuleAnd41 = do
  Right topl <- Decode.decodeTopLevel "./test-data/And41.json"
  runJIO $ "testGenMakeModuleAnd41" <? do
    let modname = "/user/And41"
    liftM TIO.putStrLn $ Vhdl.mkModule topl modname

testGenMakeModuleAnd41SubMods = do
  Right topl <- Decode.decodeTopLevel "./test-data/And41.json"
  runJIO $ "testGenMakeModuleAnd41" <? do
    let modname = "/user/And41"
    subs <- TopLevel.getSubModules topl modname
    liftM print $ mapM (Vhdl.mkSubModuleInstance topl modname) subs

testGenMakeModuleAnd41NodeDecls = do
  Right topl <- Decode.decodeTopLevel "./test-data/And41.json"
  runJIO $ "testGenMakeModuleAnd41NodeDecls" <? do
    let modname = "/user/And41"
    liftM print $ Vhdl.mkNodeDecls topl modname

testGenMakeModule = do
  Right topl <- Decode.decodeTopLevel "./test-data/And41.json"
  runJIO $ "testGenMakeModuleAnd41NodeDecls" <? do
    let modname = "/user/And41"
        prelude = decodeUtf8 $(embedFile "app-data/vhdl/prelude.vhdl")
    moduleCode <- Vhdl.mkModule topl modname
    testCode <- Vhdl.mkTestBench topl modname 
  
    return $ do
      let outfile = "test-data/vhdl/mod1/" ++ (hashid modname) ++ ".vhdl"
      TIO.writeFile outfile (T.concat [ prelude, moduleCode, testCode ])

testConnectOutputJumper1 = do
  Right topl <- Decode.decodeTopLevel "./test-data/Jumper1.json"
  runJIO $ "testConnectOutputJumper1" <? do
    let modname = "/user/Jumper1"
    liftM print $ Vhdl.connectOutput topl modname (SigSimple "vout")


