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

testBoundingBox modname (BB x1 y1 x2 y2) = do
  testIcon "testCenter" modname $ \icon -> do
    nb $ show icon
    bb@(BB left top right bottom) <- Icon.boundingBox icon
    nb $ show bb
    when (top    /= y1) $ "top"    <? expected y1 top
    when (left   /= x1) $ "left"   <? expected x1 left
    when (bottom /= y2) $ "bottom" <? expected y2 bottom
    when (right  /= x2) $ "left  " <? expected x2 right

testCenter modname expCenter = do
  testIcon "testCenter" modname $ \icon -> do 
    center <- Icon.center icon
    when (expCenter /= center) $ "test.func.center" <? do
      expected expCenter center

allTests = do
  -- passing
  testBoundingBox "IconBoundingBox8" $ BB 8 0 16 16
  testBoundingBox "IconBoundingBox7" $ BB 0 0 24 16
  testBoundingBox "IconBoundingBox1" $ BB (-16) (-16) 16 16
  testBoundingBox "IconBoundingBox1Rot90" $ BB (-16) (-16) 16 16
  testBoundingBox "IconBoundingBox3" $ BB 8 8 40 40
  
  testCenter "IconBoundingBox1" (0, 0)
  testCenter "IconBoundingBox1Rot90" (0, 0)
  testCenter "IconBoundingBox3" (24, 24)

  testBoundingBox "AND2" $ BB (-16) (-16) 16 16
  testBoundingBox "AND2Rot90" $ BB (-16) (-16) 16 16

  -- failing 
 
    
