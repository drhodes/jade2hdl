module TestNet (testTree) where

import qualified Data.List as DL
import qualified Data.ByteString as DB
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Modul
import qualified Jade.Net as Net
import qualified Jade.Wire as Wire
import Text.Format
import TestUtil
import Control.Monad
import Jade.Rawr.Types 
import Jade.Common

testTreeGetInternalSigNames = TestTree "getinternalsigname" $
  let t name exp = TestCase name (testGetInternalSigNames name exp)
  in [ t "Rep1FA2" ["CO"]
     ]


testGetInternalSigNames modname expected = do
  testExpGot modname expected $ do
    TopLevel.getInternalSigNames (qualifiedModName modname)

testGetNumNetsInternalSigNames = TestTree "getinternalsigname" $
  let t name exp = TestCase name (testGetInternalSigNames name exp)
  in [ t "Rep1FA2" ["CO"]
     ]


testTree = TestTree "Net" [ testTreeGetInternalSigNames
                          ]

