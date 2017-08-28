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
  JRT.runTree $ JRT.TestTree "TestAll"
    [ TD.testTree
    , TI.testTree
    , TV.testTree
    ]
  putStrLn ""
  testem [ TM.testAll
         , TTL.testAll
         , TW.testAll
         --, TV.testAll -- "./test-data/AndStuff6.json" "/user/AndStuff6"
         ]
  putStrLn "All done."
  
    



