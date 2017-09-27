module TestSchematic (testTree) where

import qualified Data.List as DL
import qualified Data.ByteString as DB
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode.Decode as Decode
import qualified Jade.Schematic as Schem
import qualified Jade.Net as Net
import qualified Jade.Wire as Wire
import Text.Format
import TestUtil
import Control.Monad
import Rawr.Types 
import Jade.Common

testExpGot modname expected f = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let func = do
        got <- f
        case got == expected of
          True -> return ()
          False -> let temp = "exp: {0}, got: {1}" 
                   in do enb got
                         die $ format temp [show expected, show got]
  case runJ topl func of
    Right x -> return Pass
    Left msg -> return $ Fail $ unlines [msg ++ runLog topl func]

testNumTerminals modname expected = do
  testExpGot modname expected $ do
    let qualModName = "/user/" ++ modname 
    allSubs <- TopLevel.getSubModules qualModName
    length <$> concatMapM TopLevel.terminals allSubs

testNumPartsAtPoint modname point expected = do
  testExpGot modname expected $ do
    let qualModName = "/user/" ++ modname
    s <- TopLevel.getSchematic qualModName
    ps <- Schem.getAllPartsAtPoint s point
    return $ length ps

testTreeNumPartsAtPoint = TestTree "GetAllPartsAtPoint" $
  let t modname point exp = TestCase modname (testNumPartsAtPoint modname point exp)
  in [ t "Buffer1" (Point (-16) (-32)) 2
     , t "Buffer1" (Point (16) (-32)) 2
     ]

testGetAllParts modname expected = do
  testExpGot modname expected $ do
    let qualModName = "/user/" ++ modname
    s <- TopLevel.getSchematic qualModName
    let ps = Schem.getAllParts s
    return $ length ps

testTreeGetAllParts = TestTree "getAllParts" $
  let t modname exp = TestCase modname (testGetAllParts modname exp)
  in [ t "Buffer1" 3
     , t "Buffer2" 3
     , t "Buffer3" 3
     , t "Buffer4" 3
     , t "Buffer5" 3
     , t "Buffer5_1" 3
     , t "Buffer5_2" 3
     , t "Buffer8" 6
     , t "BuiltInAnd4Messy" 13
     , t "CLA32" 110
     ]

testTree = TestTree "Schematic" [ testTreeNumPartsAtPoint
                                , testTreeGetAllParts
                                ]
