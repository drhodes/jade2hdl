module Main where

import qualified TestDecode as TD
import qualified TestVhdl as TV
import qualified TestModule as TM
import qualified TestIcon as TI
import qualified TestTopLevel as TTL
import qualified TestWire as TW
  
main = do
  putStrLn "Starting test"
  TD.testAll
  TI.testAll  
  TM.testAll
  TV.testAll -- "./test-data/AndStuff6.json" "/user/AndStuff6"
  TTL.testAll
  TW.testAll
  putStrLn "All done."
