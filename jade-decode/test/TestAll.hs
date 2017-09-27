module Main where

import qualified TestDecode as TD
import qualified TestSig as TS
import qualified Jade.Rawr.Types as JRT


main = do
  putStrLn ""
  putStrLn "Starting test"
    
  results <- JRT.runTree [] $ JRT.TestTree "T"
             [ --
             , TS.testTree
             , TD.testTree
             ]

  putStrLn "" -- newline
  putStrLn $ "Num Tests: " ++ (show $ length results)
  putStrLn "All done."
    



