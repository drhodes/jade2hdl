module Main where

-- import qualified TestVhdl as TV
-- import qualified TestModule as TM
-- import qualified TestMiddle
-- import qualified TestIcon as TI
import qualified TestTopLevel as TTL
-- import qualified TestWire as TW
-- import qualified TestSchematic 
import qualified Rawr.Types as RT
import qualified Rawr.Rawr as RR
import qualified Control.Parallel.Strategies as CPS

testem :: (Traversable t, Monad m) => t (m a) -> m (t a)
testem xs = sequence $ CPS.runEval $ CPS.parTraversable CPS.rpar xs

main = do
  putStrLn ""
  putStrLn "Starting test"
    
  results <- RR.runTree [] $ RT.TestTree "T"
             [ --
             --   TW.testTree
             -- , TestSchematic.testTree
             -- , TI.testTree
             -- , TM.testTree
             -- , TestMiddle.testTree
             TTL.testTree             
             --, TV.testTree
             ]

  putStrLn "" -- newline
  putStrLn $ "Num Tests: " ++ (show $ length results)
  putStrLn "All done."
    



