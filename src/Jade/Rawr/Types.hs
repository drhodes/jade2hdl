{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Jade.Rawr.Types where
import Prelude hiding (pow)
import Control.Monad
import qualified System.IO as SIO

import qualified Control.Parallel.Strategies as CPS

-- class Compose a b c where
--   (·) :: (Compose a b) -> (Compose b c) -> (Compose a c)

nofact n = if n <= 0
           then 0
           else n + (nofact (n-1))

test1 = CPS.parList CPS.rseq (map nofact $ take 10 (repeat 10000000))

data Case = Case String (IO TestState)
          | Done String TestState
          
data TestTree = TestTree String [TestTree]
              | TestNode Case

runTree :: TestTree -> IO [TestState]
runTree (TestTree s trees) = do
  putStr $ take 20 $ s ++ ":                    "
  result <- liftM concat $ mapM runTree trees
  putStrLn ""
  return result
  
runTree (TestNode c@(Case s f)) = do
  result <- sequence $ CPS.runEval $ CPS.parList CPS.rseq [f]
  mapM_ (rawrLog s) result
  return result

data TestState = Ready
               | Running
               | Pass
               | Fail String               
               deriving (Show, Eq)
passes = do putStr "·"
            SIO.hFlush SIO.stdout
            
fails = do putStr "X"
           SIO.hFlush SIO.stdout


rawrLog :: String -> TestState -> IO ()
rawrLog string state =  
  case state of
    Pass -> return ()
    Fail msg -> do let logPath = "./logs/" ++ string ++ ".log"
                   writeFile logPath msg
                   --putStrLn $ "writing log: " ++ logPath ++ ", state:  " ++ (show msg)
  
