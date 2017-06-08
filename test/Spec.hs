{-# LANGUAGE OverloadedStrings #-}

import Jade.ModTest
import Jade.Types
import Jade.Decode
import Data.Aeson

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


