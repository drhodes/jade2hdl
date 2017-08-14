module TestMiddle where

import Jade.Types
import Jade.Middle.Types 
import qualified Data.List as DL
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Modul
import qualified Jade.GComp as GComp
import qualified Jade.Wire as Wire
import Text.Format
import TestUtil

testSkeleton :: String -> (TopLevel -> J Bool) -> IO ()
testSkeleton modname func = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let f = func topl
  case runJ f of
    Right True -> putStrLn $ modname ++ ": PASS"
    Right False -> do putStrLn $ runLog f
    Left msg -> do putStrLn $ msg
                   putStrLn $ runLog f

testReplicationDepth :: String -> Integer -> IO ()
testReplicationDepth modname expDepth = do
  let func topl = do
        let parentModuleName = "/user/" ++ modname
        subs <- TopLevel.getSubModules topl parentModuleName
        let sub = subs !! 0
        d <- TopLevel.replicationDepth topl ("/user/" ++ modname) sub
        nb $ show d
        if (expDepth == d)
          then return True
          else do expected expDepth d
                  return False
  testSkeleton modname func 


checkSubModuleInstances :: String -> IO ()
checkSubModuleInstances modname = do
  let func topl = "testSubModuleInstances" <? do
        let parentModuleName = "/user/" ++ modname
        subs <- TopLevel.getSubModules topl parentModuleName
        let sub = subs !! 0
        instances <- subModuleInstances topl parentModuleName sub
        return False
  testSkeleton modname func

checkConnectOneOutput modname outsig = do
  let func topl = "testSubModuleInstances" <? do
        let modname' = "/user/" ++ modname                
        result <- connectOneOutput topl modname' outsig
        nb $ show result
        return False        
  testSkeleton modname func

testConnectOutputs = do
  checkConnectOneOutput "RepAnd2" (SigRange "out1" 1 0)


testAll = do

  
  --------------------------------------------
  testReplicationDepth "And2Ports" 1 
  testReplicationDepth "And2Ports2" 1
  testReplicationDepth "And2Ports3" 1
  testReplicationDepth "And2Ports4" 1  
  testReplicationDepth "RepAnd2" 2

  -- 
  -- checkSubModuleInstances "RepAnd2" 
  -- checkSubModuleInstances "RepAnd3" 
  -- checkSubModuleInstances "RepAnd4"

  testReplicationDepth "And41" 1

test = do  
  checkSubModuleInstances "And41"
  checkSubModuleInstances "RepAnd2" 
  checkSubModuleInstances "RepAnd3" 
  checkSubModuleInstances "RepAnd4"
 

  
