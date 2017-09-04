{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.List as DL

nofact n = if n <= 0
           then 0
           else n + (nofact (n-1))

test1 = CPS.parList CPS.rseq (map nofact $ take 10 (repeat 10000000))
          
data TestTree = TestTree String [TestTree]
              | TestCase String (IO TestState)
              
data TestState = Pass
               | Fail String
               deriving (Show, Eq)

concatTreeName s (TestTree name subtrees) = TestTree (concat [s, "/", name]) subtrees
concatTreeName _ t = t

type TestPath = [String]
showTestPath tp = DL.intercalate "/" tp

runTree :: TestPath -> TestTree -> IO [TestState]
runTree tp (TestTree s trees) = do
  putStrLn ""
  putStr $ take 40 $ (showTestPath (tp ++ [s])) ++ repeat ' '
  concatMapM (runTree (tp ++ [s])) trees
  
runTree tp (TestCase s f) = do
  result <- sequence $ CPS.runEval $ CPS.parList CPS.rpar [f]
  mapM_ (rawrLog tp s) result
  return result

passes = putStr "·" >> SIO.hFlush SIO.stdout            
fails = putStr "X" >> SIO.hFlush SIO.stdout

rawrLog tp name state =  
  case state of
    Pass -> passes
    Fail log -> do fails
                   let fname = DL.intercalate "_" (tp ++ [name])
                   let logPath = "./logs/" ++ fname ++ ".log"
                   writeFile logPath log

doTree :: String -> Writer [TestTree] a -> TestTree
doTree name doblock = TestTree name  $ execWriter doblock

report name state = TestCase name $ return state
reportIO name state = TestCase name $ state

test :: MonadWriter [TestTree] m => String -> IO TestState -> m ()
test name f = tell [TestCase name f]

asdf = doTree "MyTree" $ do
  test "asdf" $ return Pass
  test "zxcv" $ return $ Fail "asdf"
