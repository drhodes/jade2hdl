{-# LANGUAGE FlexibleContexts #-}
module Rawr.Rawr where

import Rawr.Types 
import Prelude hiding (pow)
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Functor.Identity
import qualified System.IO as SIO
import qualified Control.Parallel.Strategies as CPS
import qualified Data.List as DL
import qualified System.Console.ANSI as SCA

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
  putStr "·" >> SIO.hFlush SIO.stdout
  SCA.setSGR [SCA.Reset]

fails = do
  SCA.setSGR [SCA.SetBlinkSpeed SCA.SlowBlink]
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

test :: MonadWriter [TestTree] m => String -> IO TestState -> m ()
test name f = tell [TestCase name f]

