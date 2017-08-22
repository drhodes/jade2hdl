module TestUtil where

import Jade.Types
import Text.Format
import Control.Monad

expected :: Show a => a -> a -> J ()
expected exp got = do
  nb $ "Expected : " ++ show exp
  nb $ "Got      : " ++ show got

withTest name f = do
  putStr $ take 20 $ format "{0}                              " [name]
  f
  putStrLn ""



expectedEq x y = unless (x == y) (expected x y)

pass = putStr "."
