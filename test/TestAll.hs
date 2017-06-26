module Main where

import qualified TestDecode as TD

main = do
  TD.testAll
  putStrLn "All done."
