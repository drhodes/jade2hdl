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

-- testTreeGetInternalSigNames = TestTree "getinternalsigname" $
--   let t name exp = TestCase name (testGetInternalSigNames name exp)
--   in [ t "Rep1FA2" ["CO"]
--      ]


-- testGetInternalSigNames modname expected = do
--   testExpGot modname expected $ do
--     TopLevel.getInternalSigNames (qualifiedModName modname)

-- testGetNumNetsInternalSigNames = TestTree "getinternalsigname" $
--   let t name exp = TestCase name (testGetInternalSigNames name exp)
--   in [ t "Rep1FA2" ["CO"]
--      ]


-- testNetSig modname exp = do
--   Right topl <- Decode.decodeTopLevel ("./test-data/" ++ modname ++ ".json")
--   let func = do cs <- TopLevel.wireComponents ("/user/" ++ modname) --ok
--                 return (length cs)
--   return $ case runJ topl func of
--     Right x -> if x == exp
--                then Pass
--                else let log = runLog topl func
--                     in Fail $ log <> (P.text $ "Expected: " ++ show exp ++ " got: " ++ show x)
--     Left msg -> Fail msg


-- testTreeNumWireComponents =
--   let t modname exp = TestCase modname (testNetSig modname exp)
--   in TestTree "numWireComponents" $
--      [ t "Buffer3" 4
--      , t "Jumper3" 3
--      , t "Jumper41" 13                          

testTree = TestTree "Net" [ ]

