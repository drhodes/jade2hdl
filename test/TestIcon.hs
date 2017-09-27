{-# LANGUAGE FlexibleContexts #-}
module TestIcon (testTree) where

import qualified Data.Map as DM
import Jade.Common
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Icon as Icon
import qualified Jade.Decode.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Decode.Coord as Coord
import qualified Jade.Vhdl as Vhdl
import qualified Data.Hashable as H
import Text.Format
import TestUtil
import Control.Monad
import Rawr.Rawr
import Rawr.Types

withTopLevelTest modname f = 
  liftM f $ Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

testIcon tname libname modname f = do
  let qualModName = libname ++ modname
      testName = tname ++ ": " ++ modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

  let func = testName <? do
        m <- TopLevel.getModule qualModName
        case moduleIcon m of
          Nothing -> die $ "No icon found for module: " ++ qualModName
          Just icon -> f icon
  case runJ topl func of
    Right x -> do return Pass
    Left msg -> do return $ Fail $ unlines [ msg, runLog topl func ]
                     
testBoundingBox modname (BB x1 y1 x2 y2) = do
  testIcon "testBoundingBox" "/user/" modname $ \icon -> do
    nb $ show icon
    bb@(BB left top right bottom) <- Icon.boundingBox icon
    nb $ show bb
    
    let f x y s = when (x /= y) $ do nb s
                                     expected y x
                                     bail
    f top y1 "top"
    f left x1 "left"
    f bottom y2 "bottom"
    f right  x2 "left"

testCenter modname expCenter = do
  testIcon "testCenter" "/user/" modname $ \icon -> do 
    center <- Icon.center icon
    when (expCenter /= center) $ do
      expected expCenter center
      bail

testBuiltInBoundingBox modname (BB x1 y1 x2 y2) = do
  testIcon "testBoundingBox" "/gates/" modname $ \icon -> do
    nb $ show icon
    bb@(BB left top right bottom) <- Icon.boundingBox icon
    nb $ show bb
    when (top    /= y1) $ "top"    <? expected y1 top >> bail
    when (left   /= x1) $ "left"   <? expected x1 left >> bail
    when (bottom /= y2) $ "bottom" <? expected y2 bottom >> bail 
    when (right  /= x2) $ "left  " <? expected x2 right >> bail
    
testTree = let tbbb2 s bb = test s $ testBuiltInBoundingBox s bb
               tbb s bb = test s  $ testBoundingBox s bb
               tc s center = test s $ testCenter s center
           in doTree "TestIcon" $
              do tbbb2 "and2" $ BB {bbLeft=0, bbTop=(-4), bbRight=48, bbBottom=20}
                 tbb "IconBoundingBox3" $ BB 8 8 40 40
                 tbb "IconBoundingBox8" $ BB 8 0 16 16
                 tbb "IconBoundingBox7" $ BB 0 0 24 16
                 tbb "IconBoundingBox1" $ BB (-16) (-16) 16 16
                 tbb "IconBoundingBox1Rot90" $ BB (-16) (-16) 16 16
                 tbb "IconBoundingBox3" $ BB 8 8 40 40 
                 tbb "AND2" $ BB (-16) (-16) 16 16
                 tbb "AND2Rot90" $ BB (-16) (-16) 16 16
                 tc "IconBoundingBox1" (0, 0)
                 tc "IconBoundingBox1Rot90" (0, 0)
                 tc "IconBoundingBox3" (24, 24)
              
 
    
