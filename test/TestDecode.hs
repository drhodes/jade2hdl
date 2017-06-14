{-# LANGUAGE OverloadedStrings #-}

import Jade.ModTest
import Jade.Types
import Jade.Decode
import Data.Aeson
import Control.Monad
import Test.QuickCheck.Arbitrary



main :: IO ()
main = putStrLn "Test suite not yet implemented"

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

testComp1 :: Either String Component
testComp1 = eitherDecode "[\"port\", [136, 64, 1], {\"signal\": \"wd\", \"direction\":\"out\"}]" 

testSub1 :: Either String SubModule
testSub1 = eitherDecode "[\"/user/and\", [136, 64, 1]]"

testSchem1 :: Either String Schematic
testSchem1 = eitherDecode "[[\"port\", [136, 64, 1], {\"signal\": \"wd\"}],[\"port\", [136, 64, 1], {\"signal\": \"wd\"}]]"

schem2 = "[[\"wire\", [136, 64, 1, 0, 0], {\"signal\": \"wd\"}],[\"port\", [136, 64, 1], {\"signal\": \"wd\"}]]" 
testSchem2 :: Either String Schematic
testSchem2 = eitherDecode schem2

testLine1 = do
  let tstring = "[ \"line\", [ -8, -8, 0, 0, 16 ] ]"
      result = Right (Line (Coord5 {c5x = -8, c5y = -8, c5r = Rot0, c5dx = 0, c5dy = 16}))
  dotest "testLine1" tstring result

dotest testname teststring expected = do
  let result = eitherDecode teststring
  
  if (result == expected)
    then putStrLn $ "PASS: " ++ testname
    else mapM_ print [ "------------------------------------------------------------------"
                     , "FAIL:     " ++ testname
                     , "Expected: " ++ (show expected)
                     , "Got:      " ++ (show result)
                     ]
  return result
  
testTerminal1 = do
  let tstring = "[ \"terminal\", [ 16, 0, 4 ], { \"name\": \"out\" } ]"
      expected = Right (Terminal (Coord3 {c3x = 16, c3y = 0, c3r = 4}) (SigSimple "out"))
  dotest "testTerminal1" tstring expected


testTerminal2 :: IO (Either String IconPart)
testTerminal2 = do
  let tstring = "[ \"terminal\", [ 16, 0, 4 ], { \"name\": \"out\" } ]"
      expected = Right (IconTerm (Terminal (Coord3 {c3x = 16, c3y = 0, c3r = 4}) (SigSimple "out")))
  dotest "testTerminal1" tstring expected
  



testall = do
  testLine1
  testTerminal1
  testTerminal2

