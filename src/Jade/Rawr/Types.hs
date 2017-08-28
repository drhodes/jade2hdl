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
import Control.Monad.Writer
import Control.Monad.Identity
import qualified System.IO as SIO
import Text.Format
import qualified Control.Parallel.Strategies as CPS

nofact n = if n <= 0
           then 0
           else n + (nofact (n-1))

test1 = CPS.parList CPS.rseq (map nofact $ take 10 (repeat 10000000))

data Case = Case String (IO TestState)
          | Done String TestState
          
instance Show Case where
  show (Case name _) = format "<Case {0} <func>>" [name]
  show (Done name state) = format "<Done {0} {1}>" [name, show state]
          
data TestTree = TestTree String [TestTree]
              | TestNode Case

concatTreeName s (TestTree name subtrees) = TestTree (concat [s, ":", name]) subtrees
concatTreeName _ t = t

runTree :: TestTree -> IO [TestState]
runTree (TestTree s trees) = do
  putStrLn ""
  putStr $ take 20 $ s ++ ":                    "
  result <- liftM concat $ mapM runTree (map (concatTreeName s) trees)
  return result
  
runTree (TestNode c@(Case s f)) = do
  result <- sequence $ CPS.runEval $ CPS.parList CPS.rpar [f]
  mapM_ (rawrLog s) result
  return result

data TestState = Ready
               | Running
               | Pass
               | Fail String               
               deriving (Show, Eq)

passes = putStr "·" >> SIO.hFlush SIO.stdout            
fails = putStr "X" >> SIO.hFlush SIO.stdout

rawrLog :: String -> TestState -> IO ()
rawrLog string state =  
  case state of
    Pass -> return ()
    Fail msg -> do let logPath = "./logs/" ++ string ++ ".log"
                   writeFile logPath msg

doTree :: String -> Writer [TestTree] a -> TestTree
doTree name doblock = TestTree name  $ execWriter doblock

done name x = tell [Done name x]

test :: MonadWriter [TestTree] m => String -> IO TestState -> m ()
test name f = tell [TestNode $ Case name f]

asdf = doTree "MyTree" $ do
  test "asdf" $ return Pass
  test "zxcv" $ return $ Fail "asdf"
