module Main where

import qualified TestDecode as TD
import qualified TestVhdl as TV
import qualified TestModule as TM
import qualified TestIcon as TI
import qualified TestTopLevel as TTL
import qualified TestWire as TW
import qualified Jade.Rawr.Types as JRT
import qualified Control.Parallel.Strategies as CPS

testem :: (Traversable t, Monad m) => t (m a) -> m (t a)
testem xs = sequence $ CPS.runEval $ CPS.parTraversable CPS.rpar xs

main = do
  putStrLn ""
  putStrLn "Starting test"
  stuff <- JRT.runTree $ JRT.TestTree ""
           [ TD.testTree
           , TI.testTree
           , TM.testTree
           , TV.testTree
           , TTL.testTree
           ]    
  putStrLn ""
  
  testem
    [ --TTL.testAll
      TW.testAll
      --, TV.testAll -- "./test-data/AndStuff6.json" "/user/AndStuff6"
    ]
  putStrLn "All done."
  print stuff
    



