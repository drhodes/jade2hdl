{-# LANGUAGE FlexibleContexts #-}

module Jade.Rawr.Types where
import Prelude hiding (pow)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Functor.Identity
import qualified System.IO as SIO
import Text.Format
import qualified Control.Parallel.Strategies as CPS
import Jade.Util

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
              
data TestState = Pass
               | Fail String
               deriving (Show, Eq)

concatTreeName s (TestTree name subtrees) = TestTree (concat [s, "/", name]) subtrees
concatTreeName _ t = t




runTree :: TestTree -> IO [TestState]
runTree (TestTree s trees) = do
  putStrLn ""
  putStr $ take 40 $ s ++ repeat ' '
  concatMapM runTree (map (concatTreeName s) trees)
  
runTree (TestNode c@(Case s f)) = do
  result <- sequence $ CPS.runEval $ CPS.parList CPS.rpar [f]
  mapM_ (rawrLog s) result
  return result


passes = putStr "·" >> SIO.hFlush SIO.stdout            
fails = putStr "X" >> SIO.hFlush SIO.stdout

rawrLog :: String -> TestState -> IO ()
rawrLog name state =  
  case state of
    Pass -> passes >> return ()
    Fail log -> do fails
                   let logPath = "./logs/" ++ name ++ ".log"
                   writeFile logPath log

doTree :: String -> Writer [TestTree] a -> TestTree
doTree name doblock = TestTree name  $ execWriter doblock

-- reportIO :: MonadWriter [TestTree] IO => String -> IO TestState -> IO ()
-- reportIO name result = tell [TestNode $ Case name $ result]

-- report :: String -> TestState -> Writer [TestTree] (IO ())
-- report name result = tell [TestNode $ Case name $ return result]

report name state = TestNode (Case name $ return state)
reportIO name state = TestNode (Case name $ state)


done name x = tell [Done name x]

test :: MonadWriter [TestTree] m => String -> IO TestState -> m ()
test name f = tell [TestNode $ Case name f]

asdf = doTree "MyTree" $ do
  test "asdf" $ return Pass
  test "zxcv" $ return $ Fail "asdf"
