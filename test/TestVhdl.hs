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

import System.Process 
import System.Exit      
import Control.Exception.Base as CEB
import Control.Concurrent as CC
       
spawnOneTest jadefile modname = do
  putStrLn $ "Vhdl.Testing: " ++ modname
  
  -- create a test directory
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
    moduleCode <- Vhdl.mkAllMods topl 
    testCode <- Vhdl.mkTestBench topl modname
    mods <- Vhdl.mkAllMods topl
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
        ExitSuccess -> return () --ecode' --putStrLn modname
        ExitFailure err' -> do
          putStrLn errlog
          print err'
          putStrLn stderr'
      return ecode'
    ExitFailure err -> do
      putStrLn errlog
      putStrLn modname
      putStrLn stderr
      print err
      return ecode

spawn s = spawnOneTest ("./test-data/" ++ s ++ ".json") ("/user/" ++ s)

testAll = do  
  mapM_ spawn [ "Jumper1"
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
              ]
    
  --spawn "AndStuff6" -- optimize this, eventually.
  spawnBuiltIn
  spawnBuiltInAnd4Messy
  
spawnBuiltIn = spawnOneTest "./test-data/BuiltInAnd4.json" "/user/BuiltInAnd4"
spawnBuiltInAnd4Messy = spawnOneTest "./test-data/BuiltInAnd4Messy.json" "/user/BuiltInAnd4Messy"

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
