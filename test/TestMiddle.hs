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

testSubModuleInstances :: String -> IO ()
testSubModuleInstances modname = do
  let func topl = "testSubModuleInstances" <? do
        let parentModuleName = "/user/" ++ modname
        subs <- TopLevel.getSubModules topl parentModuleName
        let sub = subs !! 0
        instances <- subModuleInstances topl parentModuleName sub
        return False
  testSkeleton modname func

testAll = do
  --------------------------------------------
  --testReplicationDepth "RepAnd2" 2  
  testSubModuleInstances "RepAnd3" 
  --testSubModuleInstances "RepAnd4"
 
  -- testReplicationDepth "And2Ports" 1 
  -- testReplicationDepth "And2Ports2" 1
  -- testReplicationDepth "And2Ports3" 1
  -- testReplicationDepth "And2Ports4" 1
