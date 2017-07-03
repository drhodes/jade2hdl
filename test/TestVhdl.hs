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
  -- create a test directory
  let autoTestPath = format "test-data/auto-vhdl/{0}/" [hashid modname]
      tbname = Module.testBenchName modname
      prelude = decodeUtf8 $(embedFile "app-data/vhdl/prelude.vhdl")
      outfile = autoTestPath ++ (hashid modname) ++ ".vhdl"
      
  SD.removePathForcibly autoTestPath
  SD.createDirectory autoTestPath
  Right topl <- Decode.decodeTopLevel jadefile
  runJIO $ modname <? do
    moduleCode <- Vhdl.mkModule topl modname
    testCode <- Vhdl.mkTestBench topl modname 
    return $ do
      TIO.writeFile outfile (T.concat [ prelude, moduleCode, testCode ])

  let cmd1  = (shell $  "ghdl -a -g --std=08 *.vhdl") { cwd = Just autoTestPath
                                                     , std_out = CreatePipe
                                                     , std_err= CreatePipe
                                                     }
  let cmd2  = (shell $  "ghdl -r --std=08 " ++ tbname) { cwd = Just autoTestPath
                                                       , std_out = CreatePipe
                                                       , std_err= CreatePipe
                                                       }
  (ecode, stdout, stderr) <- readCreateProcessWithExitCode cmd1 ""
  case ecode of
    ExitSuccess -> do 
      (ecode', stdout', stderr') <- readCreateProcessWithExitCode cmd2 ""
      case ecode' of
        ExitSuccess -> putStrLn modname
        ExitFailure err' -> do
          print err'
          putStrLn stderr'
      return ecode'
    ExitFailure err -> do
      putStrLn modname
      putStrLn stderr
      print err
      return ecode

fork1 f = CC.forkFinally f $
  \x -> case x of
          Left e -> CEB.throw e
          Right ecode -> print $ "Good, " ++ show ecode

spawnAllTests = do  
  mapM_ fork1 [ spawnOneTest "./test-data/And41.json" "/user/And41"
              , spawnOneTest "./test-data/AndStuff4.json" "/user/AndStuff4"
              , spawnOneTest "./test-data/AndStuff5.json" "/user/AndStuff5"
              , spawnOneTest "./test-data/AndStuff6.json" "/user/AndStuff6"
              , spawnBuiltIn
              , spawnBuiltInAnd4Messy
              ]
  
spawnBuiltIn = spawnOneTest "./test-data/BuiltInAnd4.json" "/user/BuiltInAnd4"
spawnJumper1 = spawnOneTest "./test-data/Jumper1.json" "/user/Jumper1"
  
spawnBuiltInAnd4Messy =
  spawnOneTest "./test-data/BuiltInAnd4Messy.json" "/user/BuiltInAnd4Messy"

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
      -- TIO.
      -- TIO.putStrLn prelude
      -- TIO.putStrLn moduleCode
      -- TIO.putStrLn testCode
