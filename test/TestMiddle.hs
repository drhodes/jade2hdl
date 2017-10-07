module TestMiddle (testTree) where

import Jade.Middle.Middle
import Rawr.Types 
import Jade.Common
import Text.Format
import qualified Data.List as DL
import qualified Jade.Decode.Decode as Decode
import qualified Jade.Module as Modul
import qualified Jade.Net as Net
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Wire as Wire
import qualified Text.PrettyPrint.Leijen as P 
-- (

testSkeleton :: String -> (J Bool) -> IO TestState
testSkeleton modname func = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  case runJ topl func of
    Right True -> return Pass
    Right False -> return $ Fail $ runLog topl func
    Left doc -> return $ Fail $ doc
runLog topl func P.<> doc

-- todo, keep or trash these tests? they don't do anything right now.
checkSubModuleInstances :: String -> IO TestState
checkSubModuleInstances modname = do
  let func = "testSubModuleInstances" <? do
        let parentModuleName = "/user/" ++ modname
        subs <- TopLevel.getSubModules parentModuleName
        let sub = subs !! 0
        instances <- subModuleInstances parentModuleName sub
        return True -- see? always returns true.
  testSkeleton modname func

node s f = TestCase s f
tree s xs = TestTree s $ mapF (map node [s ++ "_" ++ show x | x<- [1..]]) xs

testTree = TestTree "Middle"
  [ tree "InnerSignal" [ checkSubModuleInstances "And41"
                       , checkSubModuleInstances "RepAnd2" 
                       , checkSubModuleInstances "RepAnd3" 
                       , checkSubModuleInstances "RepAnd4"
                       ]
    
  -- , tree "InnerSignal" [ checkConnectOneOutput "BuiltInAnd4Messy" (Bundle [ValIndex "vout" 0])
                         
  --                      ]
  ]


{-
checkConnectOneOutput modname outsig = do
  let func = "testSubModuleInstances" <? do
        let modname' = "/user/" ++ modname                
        result <- assignBundle Out modname' outsig
        nb $ show result
        return True
  testSkeleton modname func

checkConnectOutputs = do
  checkConnectOneOutput "BuiltInAnd4Messy" (Bundle [ValIndex "vout" 0])
-}
