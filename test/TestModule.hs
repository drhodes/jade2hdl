module TestModule (testTree) where

import qualified Data.Map as DM
import qualified Data.List as DL
import Jade.Common
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Icon as Icon
import qualified Data.Hashable as H
import qualified Jade.Decode.Bundle as Bundle
import Text.Format
import Control.Monad
import TestUtil
import Rawr.Types 

testSubModBoundingBox :: String -> BoundingBox -> IO TestState
testSubModBoundingBox modname exp = do 
  let qualModName = "/user/" ++ modname 
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])

  let func = do
        sub <- liftM head $ TopLevel.getSubModules qualModName
        let (SubModule name c3) = sub
        nb $ format "offset coord: {0}" [show c3]
        nb $ show sub
        m <- TopLevel.getModule name
        bb <- Module.boundingBox m c3
        if bb == exp
          then return ()
          else expected exp bb
          
  case runJ topl func of
    Right x -> return Pass
    Left msg -> return $ Fail $ unlines [msg, runLog topl func]

hasTerminalsAt2 :: String -> [(Integer, Integer)] -> IO TestState
hasTerminalsAt2 modname locs = do
  let qualModName = "/user/" ++ modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  
  let func = "hasTerminalsAt2" <? do
        topm <- TopLevel.getModule qualModName
        sub <- head `liftM` TopLevel.getSubModules qualModName
        let (SubModule name c3) = sub        
        nb $ show sub
        m@(Module _ _ _ maybeIcon) <- TopLevel.getModule name
        bb <- case maybeIcon of
          (Just icon) -> do bb <- Icon.boundingBox icon
                            return bb
          _ -> die "Couldn't find icon."
        nb $ show bb
        terms <- Module.terminals m c3
        let locs' = [(x, y) | (Terminal (Coord3 x y _) _) <- terms]
        expected locs locs'
        return $ DL.sort locs == DL.sort locs'

  case runJ topl func of
    Right True -> return Pass
    Right False -> return $ Fail $ unlines [ modname
                                           , runLog topl func ]
    Left msg -> return $ Fail $ unlines [ modname
                                        , msg ]

testTreeHasTerminalsAt = TestTree "hasTerminalsAt" $
  [ t "IconBoundingBox6" hasTerminalsAt2 [(8,40),(24,40),(16,-8)]
  , t "TermRot1" hasTerminalsAt2 [(16,16), (-16,16), (-16,-16), (16, -16)]
  , t "TermRot2" hasTerminalsAt2 [(8,8),(40,8),(8,40),(40,40)]
  , t "TermRot3" hasTerminalsAt2 [(16,8),(16,40),(48,8),(48,40)]
  , t "TermRot4" hasTerminalsAt2 [(16,8),(16,40),(48,8),(48,40)]
  , t "TermRot5" hasTerminalsAt2 [(16,0),(16,32),(48,0),(48,32)]
  , t "SubBuiltIn1" hasTerminalsAt2 [(0,0), (0, 16), (48, 8)]
  , t "SubBuiltIn1" hasTerminalsAt2 [(0,0), (0, 16), (48, 8)]
  , t "SubBuiltIn2" hasTerminalsAt2 [(16,32), (32, 32), (24, -16)]
  , t "SubBuiltIn3" hasTerminalsAt2 [(0,0), (16,0), (8,-48)]
  , t "SubBuiltIn4" hasTerminalsAt2 [(0,0), (16,0), (8,48)]
  , t "RepAnd2" hasTerminalsAt2 [(0,0), (0,16), (48,8)]
  , t "RepBuffer1" hasTerminalsAt2 [(0,0), (32,0)]
  ]

testTreeSubModBoundingBox = TestTree "subModBoundingBox" $
  [ t "SubBuiltIn3" testSubModBoundingBox BB{bbLeft=(-4), bbTop=(-48), bbRight=20, bbBottom=0}
  , t "SubBuiltIn1" testSubModBoundingBox BB{bbLeft=0, bbTop=(-4), bbRight=48, bbBottom=20}
  , t "SubBuiltIn2" testSubModBoundingBox BB{bbLeft=6, bbTop=(-16), bbRight=36, bbBottom=32}
  ]

testTreeGetSamplesWithName = TestTree "getSamplesWithName" $
  [ t "CLA4" (testGetSamplesWithName "S") [L,L,L,L]
  , t "CLA4" (testGetSamplesWithName "G") [L]
  , t "CLA4" (testGetSamplesWithName "P") [L]
  , t "CLA4" (testGetSamplesWithName "P") [L]
  , t "CL" (testGetSamplesWithName "P") [L]
  , t "CL" (testGetSamplesWithName "G") [L]
  , t "CL" (testGetSamplesWithName "CH") [L]
  , t "CL" (testGetSamplesWithName "CL") [L]
  , t "Buffer5" (testGetSamplesWithName "OUT1") [L,L,L]
 ]

testTree = TestTree "TestModule" [ testTreeSubModBoundingBox
                                 , testTreeHasTerminalsAt
                                 , testTreeGetSamplesWithName
                                 ]
 

t name iofunc exp = TestCase name (iofunc name exp)

testGetSamplesWithName :: String -> String -> [BinVal] -> IO TestState
testGetSamplesWithName sampleName modname exp = do
  let qualModName = "/user/" ++ modname
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  
  let func = "testGetSamplesWithName" <? do
        -- test the first test line
        m <- TopLevel.getModule qualModName
        testline <- head <$> Module.testLines m
        bundle <- Module.getSamplesWithName m testline sampleName
        let bvs = [bv | Lit bv <- Bundle.getLitVals bundle]
        if bvs == exp
          then return ()
          else dief "expected {0}, got {1}" [show exp, show bvs]

  case runJ topl func of
    Right x -> return Pass
    Left msg -> return $ Fail $ unlines [msg, runLog topl func]

  
