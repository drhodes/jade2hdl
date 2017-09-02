{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestDecode where

import Jade.ModTest
import Jade.Types
import Jade.Decode
import Data.Aeson
import Control.Monad
import Test.QuickCheck.Arbitrary
import TestUtil
import Text.Format
import Jade.Rawr.Types
import Jade.Util
import qualified System.IO as SIO
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import Control.Monad.Writer

testWire1 :: Either String Wire
testWire1 = eitherDecode "[\"wire\", [136, 64, 1, 0, 0], {\"signal\": \"wd\"}]" 

testSignal1 :: Either String Signal
testSignal1 = eitherDecode "{\"signal\": \"0'1\"}" 

testWireRange :: Either String Wire
testWireRange = eitherDecode "[\"wire\", [136, 64, 1, 0, 0], {\"signal\": \"wd[1:0]\"}]" 

testWireIndex :: Either String Wire
testWireIndex = eitherDecode "[\"wire\", [136, 64, 1, 0, 0], {\"signal\": \"wd[1]\"}]" 

testPort1 :: Either String Port
testPort1 = eitherDecode "[\"port\", [136, 64, 1], {\"signal\": \"wd\"}]" 

testPort2 :: Either String Port
testPort2 = eitherDecode "[\"port\", [136, 64, 1], {\"signal\": \"wd\", \"direction\":\"out\"}]" 

testComp1 :: Either String Part
testComp1 = eitherDecode "[\"port\", [136, 64, 1], {\"signal\": \"wd\", \"direction\":\"out\"}]" 

testSub1 :: Either String SubModule
testSub1 = eitherDecode "[\"/user/and\", [136, 64, 1]]"

testSchem1 :: Either String Schematic
testSchem1 = eitherDecode "[[\"port\", [136, 64, 1], {\"signal\": \"wd\"}],[\"port\", [136, 64, 1], {\"signal\": \"wd\"}]]"

schem2 = "[[\"wire\", [136, 64, 1, 0, 0], {\"signal\": \"wd\"}],[\"port\", [136, 64, 1], {\"signal\": \"wd\"}]]" 
testSchem2 :: Either String Schematic
testSchem2 = eitherDecode schem2

expectedStr exp got = unlines [ "Expected : " ++ show exp
                              , "Got      : " ++ show got ]

doTestString name tstring exp =
  case eitherDecode tstring of
    Right got -> if exp == ok got
                 then report name Pass
                 else report name $ Fail $ expectedStr exp got
    Left msg -> report name $ Fail msg

doTestEither name eitherResult =
  case eitherResult of
    Right _ -> report name Pass
    Left msg -> report name $ Fail msg

testLine1 = let tstring = "[ \"line\", [ -8, -8, 0, 0, 16 ] ]"
                exp = ok (Line (Coord5 {c5x = -8, c5y = -8, c5r = Rot0, c5dx = 0, c5dy = 16}))
            in doTestString "testLine1" tstring exp
                                
testTerminal1 = let tstring = "[ \"terminal\", [ 16, 0, 4 ], { \"name\": \"out\" } ]"
                    expected = ok (Terminal (Coord3 {c3x = 16, c3y = 0, c3r = FlipX}) (SigSimple "RESERVED_OUT"))
                in doTestString "testTerminal1" tstring expected

testTerminal2 = let tstring = "[ \"terminal\", [ 16, 0, 4 ], { \"name\": \"out\" } ]"
                    expected = ok (IconTerm (Terminal (Coord3 {c3x = 16, c3y = 0, c3r = FlipX}) (SigSimple "RESERVED_OUT")))
                in doTestString "testTerminal2" tstring expected

testWireConstant1 = do
  let tstring = "[\"wire\",[0,0,0,-8,0],{\"signal\":\"1'1\",\"width\":\"1\"}]"
      jsig = Just $ SigQuote 1 1
      jwidth = Just 1
      jdir = Nothing
      expected = ok (Wire (Coord5 0 0 Rot0 (-8) 0) (Just (Signal jsig jwidth jdir)))
  doTestString "testWireConstant1" tstring expected

testMem1 = 
  let tstring = "[ \"memory\", [ 0, 0, 0 ], { \"name\": \"Mem1\", \"contents\": \"\" } ]"
      expected = ok $ MemUnit "Mem1" (Coord3 0 0 Rot0) "" 1 1 1
  in doTestString "testMem1" tstring expected

testMem2 =
  let s = "[\"memory\",[0,0,0],{\"name\":\"Mem1\",\"contents\":\"\",\"nports\":\"2\"}]"
      expected = ok $ MemUnit "Mem1" (Coord3 0 0 Rot0)  "" 2 1 1
  in doTestString "testMem2" s expected

testMem4x2 =
  let s = "[\"memory\",[0,0,0],{\"name\":\"Mem2\",\"contents\":\"\",\"nports\":\"2\",\"naddr\":\"2\",\"ndata\":\"2\"}]"
      expected = ok $ MemUnit "Mem2" (Coord3 0 0 Rot0) "" 2 2 2
  in doTestString "testMem3" s expected

testWirePair1 =
  let tstring = "[\"wire\", [0, 0, 0, 1, 1], {\"signal\": \"A,B\"}]"
      sig = SigConcat [SigSimple "A", SigSimple "B"]
      exp = ok (Wire (Coord5 0 0 Rot0 1 1) (Just (Signal (Just $ sig) (Just 2) Nothing)))
  in doTestString "testWirePair1" tstring exp

testTree = TestTree "Decode" $ [ testTerminal1
                               , testTerminal2
                               , testWireConstant1
                               , testMem1
                               , testMem2
                               , testMem4x2
                               , testWirePair1
                               , doTestEither "testWire1" testWire1
                               , doTestEither "testSignal1" testSignal1
                               , doTestEither "testWireRange" testWireRange
                               , doTestEither "testWireIndex" testWireIndex
                               , doTestEither "testPort1" testPort1
                               , doTestEither "testPort2" testPort2
                               , doTestEither "testComp1" testComp1
                               , doTestEither "testSub1" testSub1
                               , doTestEither "testSchem1" testSchem1
                               , doTestEither "testSchem2" testSchem2
                               ]
