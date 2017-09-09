module Main where

import qualified TestDecode as TD
import qualified TestVhdl as TV
import qualified TestModule as TM
import qualified TestIcon as TI
import qualified TestTopLevel as TTL
import qualified TestSig as TS
import qualified TestWire as TW
import qualified Jade.Rawr.Types as JRT
import qualified Control.Parallel.Strategies as CPS

testem :: (Traversable t, Monad m) => t (m a) -> m (t a)
testem xs = sequence $ CPS.runEval $ CPS.parTraversable CPS.rpar xs

main = do
  putStrLn ""
  putStrLn "Starting test"
  --testem [ TW.testAll ]
    
  JRT.runTree [] $ JRT.TestTree "T"
           [ TS.testTree
           , TD.testTree
           , TI.testTree
           , TM.testTree
           , TTL.testTree
           , TV.testTree
           ]    
  putStrLn ""
  
  putStrLn "All done."
    



