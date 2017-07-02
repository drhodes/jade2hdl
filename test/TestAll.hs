module Main where

import qualified TestDecode as TD
import qualified TestVhdl as TV
  
main = do
  TD.testAll
  TV.spawnAllTests -- "./test-data/AndStuff6.json" "/user/AndStuff6"
  putStrLn "All done."
