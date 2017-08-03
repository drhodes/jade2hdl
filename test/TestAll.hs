module Main where

import qualified TestDecode as TD
import qualified TestVhdl as TV
import qualified TestModule as TM
import qualified TestIcon as TI
import qualified TestTopLevel as TTL
  
main = do
  TD.testAll
  TI.testAll  
  TM.testAll
  TV.testAll -- "./test-data/AndStuff6.json" "/user/AndStuff6"
  TTL.testAll
  putStrLn "All done."
