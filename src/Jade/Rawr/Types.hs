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
import qualified Data.List as DL
import qualified System.Console.ANSI as SCA

nofact n = if n <= 0
           then 0
           else n + (nofact (n-1))

test1 = CPS.parList CPS.rseq (map nofact $ take 10 (repeat 10000000))
          
data TestTree = TestTree String [TestTree]
              | TestCase String (IO TestState)
              
data TestState = Pass
               | Fail String
               deriving (Show, Eq)

type TestPath = [String]

concatTreeName s (TestTree name subtrees) = TestTree (concat [s, "/", name]) subtrees
concatTreeName _ t = t

isTestCase (TestCase _ _) = True
isTestCase _ = False

showTestPath tp = DL.intercalate "/" tp

runTree tp t = runTree' "?" tp t

parMap f xs = map f xs `CPS.using` CPS.parList CPS.rseq

allPass xs = length [Pass | Pass <- xs] == length xs

runTree' :: String -> TestPath -> TestTree -> IO [TestState]
runTree' c tp (TestTree s trees) = do
  putStrLn ""
  putStr $ take 40 $ (showTestPath (tp ++ [s])) ++ repeat ' '
  let nums = map show [1 .. length trees]
      leaves = filter isTestCase trees :: [TestTree]
      notLeaves = filter (not . isTestCase) trees

  xs <- concat <$> (sequence $ parMap (runLeaf (tp ++ [s])) leaves)
  if allPass xs
    then SCA.clearLine >> SCA.cursorUpLine 1
    else return ()
  
  ys <- join <$> sequence  [runTree' c (tp ++ [s]) t | (c, t) <- zip nums notLeaves]


  return $ xs ++ ys
  
runTree' c tp (TestCase s f) = do
  result <- f
  case result of
    Pass -> passes
    Fail _ -> fails    
  rawrLog tp s result
  return [result]

runLeaf :: TestPath -> TestTree -> IO [TestState]
runLeaf tp tc@(TestCase s f) = runTree' "." tp tc

passes = do
  SCA.setSGR [SCA.SetColor SCA.Foreground SCA.Vivid SCA.Green]
  --putStr "✔" >> SIO.hFlush SIO.stdout
  putStr "·" >> SIO.hFlush SIO.stdout
  SCA.setSGR [SCA.Reset]

backup = putStr "\b" >> SIO.hFlush SIO.stdout

fails = do
  SCA.setSGR [SCA.SetColor SCA.Foreground SCA.Vivid SCA.Red]
  putStr "X" >> SIO.hFlush SIO.stdout
  SCA.setSGR [SCA.Reset]
  
    
rawrLog tp name state =  
  case state of
    Pass -> return ()
    Fail log -> do let fname = DL.intercalate "_" (tp ++ [name])
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
