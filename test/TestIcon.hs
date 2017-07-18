module TestIcon where

import qualified Data.Map as DM
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Icon as Icon
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Coord as Coord
import qualified Jade.Vhdl as Vhdl
import qualified Data.Hashable as H
import Text.Format
import Control.Monad


expected :: Show a => a -> a -> J ()
expected exp got = die $ format "Expected {0}, Got: {1}" [show exp, show got]

withTopLevelTest modname f = 
  liftM f $ Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

testIcon tname modname f = do
  let qualModName = "/user/" ++ modname
      testName = tname ++ ": " ++ modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

  let func = testName <? do
        m <- TopLevel.getModule topl qualModName
        case moduleIcon m of
          Nothing -> die $ "No icon found for module: " ++ qualModName
          Just icon -> f icon
              
  case runJ func of
    Right x -> do putStrLn "Pass"
                  return x
    Left msg -> do putStrLn msg
                   putStrLn $ runLog func
                   return undefined

testBoundingBox modname expTopLeft expBottomRight = do 
  testIcon "testCenter" modname $ \icon -> do 
    (topLeft, bottomRight) <- Icon.boundingBox icon
    when (expTopLeft /= topLeft) $ "topLeft" <? do
      expected expTopLeft topLeft              
    when (expBottomRight /= bottomRight) $ "bottomRight" <? do
      expected expBottomRight bottomRight

testCenter modname expCenter = do
  testIcon "testCenter" modname $ \icon -> do 
    center <- Icon.center icon
    when (expCenter /= center) $ "test.func.center" <? do
      expected expCenter center

allTests = do
  testBoundingBox "IconBoundingBox1" (-16, -16) (16, 16)
  testBoundingBox "IconBoundingBox1Rot90" (-16, -16) (16, 16)
  testBoundingBox "IconBoundingBox3" (8,8) (40, 40)
  --
  testCenter "IconBoundingBox1" (0, 0)
  testCenter "IconBoundingBox1Rot90" (0, 0)
    
