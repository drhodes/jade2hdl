module Test.ModTest where

import qualified Jade.Decode as Decode
import qualified Jade.TopLevel as TopLevel
import qualified Jade.ModTest as ModTest
import Jade.Types

verify :: IO (Either String a) -> String -> IO ()
verify f testname = do
  x <- f
  case x of
    Left msg ->
      print $ "Fail: " ++ msg
    _ ->
      print $ "Pass: " ++ testname

testLineSignals1 :: IO (Either a ())
testLineSignals1 = do
  topl <- Decode.decodeTopLevel "./test-data/and2.json"
  case topl of
    (Right topl) -> do
      let [(_, (Module schem (Just mt) _))] = TopLevel.modules topl
      let tlines = modTestLines mt
      let result = ModTest.testLineSignals mt (tlines !! 0)
      if result == Right [(SigSimple "in1",[H]),(SigSimple "in2",[H]),(SigSimple "out",[H])]
        then return $ Right ()
        else fail "result"
    (Left msg) -> fail msg


