module TestUtil where

import Jade.Types
import Text.Format
import Control.Monad
import qualified System.IO as SIO


expected :: Show a => a -> a -> J ()
expected exp got = do
  nb $ "Expected : " ++ show exp
  nb $ "Got      : " ++ show got

withTest name f = do
  putStr $ take 20 $ format "{0}                              " [name]
  f
  putStrLn ""



expectedEq exp got = unless (got == exp) (expected exp got)

pass = do putStr "."
          SIO.hFlush SIO.stdout
