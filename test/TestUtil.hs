module TestUtil where

import Jade.Types
import Text.Format
import Control.Monad

expected :: Show a => a -> a -> J ()
expected exp got = do
  nb $ "Expected : " ++ show exp
  nb $ "Got      : " ++ show got
