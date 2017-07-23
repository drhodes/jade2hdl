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
import TestUtil
import Control.Monad


withTopLevelTest modname f = 
  liftM f $ Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

testIcon tname libname modname f = do
  let qualModName = libname ++ modname
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
  testIcon "testBoundingBox" "/user/" modname $ \icon -> do
    nb $ show icon
    bb@(BB left top right bottom) <- Icon.boundingBox icon
    nb $ show bb
    when (top    /= y1) $ "top"    <? expected y1 top
    when (left   /= x1) $ "left"   <? expected x1 left
    when (bottom /= y2) $ "bottom" <? expected y2 bottom
    when (right  /= x2) $ "left  " <? expected x2 right

testCenter modname expCenter = do
  testIcon "testCenter" "/user/" modname $ \icon -> do 
    center <- Icon.center icon
    when (expCenter /= center) $ "test.func.center" <? do
      expected expCenter center

testBuiltInBoundingBox modname (BB x1 y1 x2 y2) = do
  testIcon "testBoundingBox" "/gates/" modname $ \icon -> do
    nb $ show icon
    bb@(BB left top right bottom) <- Icon.boundingBox icon
    nb $ show bb
    when (top    /= y1) $ "top"    <? expected y1 top
    when (left   /= x1) $ "left"   <? expected x1 left
    when (bottom /= y2) $ "bottom" <? expected y2 bottom
    when (right  /= x2) $ "left  " <? expected x2 right





testAll = do
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

  testBuiltInBoundingBox "and2" $ BB {bbLeft=0, bbTop=(-4), bbRight=48, bbBottom=20}
  -- failing 
 
    
