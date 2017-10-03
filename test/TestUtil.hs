module TestUtil where

import Jade.Common
import Text.Format
import Control.Monad
import qualified System.IO as SIO

expected :: Show a => a -> a -> J ()
expected exp got = do
  nb $ "Expected : " ++ show exp
  nb $ "Got      : " ++ show got

writeCallGraph filename topl func = do
  let txt = runCallGraph topl func
  writeFile filename txt

qualifiedModName modname = "/user/" ++ modname
