module Main where

import qualified Rawr.Types as JRT
import qualified Rawr.Rawr as JRR
import qualified TestDecode
import qualified TestSig

main = do
  putStrLn ""
  putStrLn "Starting test"
    
  results <- JRR.runTree [] $ JRT.TestTree "T"
             [ TestSig.testTree
             , TestDecode.testTree
             ]

  putStrLn "" -- newline
  putStrLn $ "Num Tests: " ++ (show $ length results)
  putStrLn "All done."
    



