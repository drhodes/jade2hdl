module Test.ModTest where

import qualified Jade.Decode as Decode
import qualified Jade.TopLevel as TopLevel
import qualified Jade.ModTest as ModTest
import Jade.Types
import Jade.Util
import Control.Monad
import TestUtil

verify :: IO (Either String a) -> String -> IO ()
verify f testname = do
  x <- f
  case x of
    Left msg ->
      print $ "Fail: " ++ msg
    _ ->
      putStr "."

testLineSignals1 = do
  topl <- Decode.decodeTopLevel "./test-data/Tristate1.json"
  case topl of
    Right topl -> runJIO $ do
      Module schem mt _ <- TopLevel.getModule topl "/user/Tristate1"
      case mt of
        Just mt -> do let tlines = modTestLines mt
                      return $ putStrLn $ show tlines
    Left msg -> fail msg

testModTestLeReg1 = do
  topl <- Decode.decodeTopLevel "./test-data/LeReg1.json"
  let expectedTestLines =
        [ TestLine {testLineBinVals = [H,Z,L], testLineComment = Just " LE=0; D undefined, Q should be good."}
        , TestLine {testLineBinVals = [L,L,H], testLineComment = Just " LE=1; D=0 .. Q still latched from last clk."}
        , TestLine {testLineBinVals = [H,Z,H], testLineComment = Just " LE=0; D goes undefined, Q should be good."}
        , TestLine {testLineBinVals = [H,L,H], testLineComment = Just " LE=0; D=0 .. Q output latched in."}
        , TestLine {testLineBinVals = [L,H,H], testLineComment = Just " LE=1; D=1 .. Q=D"}
        , TestLine {testLineBinVals = [L,H,Z], testLineComment = Just " LE=1; D=1 .. init"}
        ] 

      expectedModInputs = Just (Inputs [SigSimple "STALL",SigSimple "D"])
      expectedModOutputs = Just (Outputs [SigSimple "Q"])      
  case topl of
    Right topl -> runJIO $ do
      Module schem mt _ <- TopLevel.getModule topl "/user/LeReg1"

      
      case mt of
        Just mt -> do
          expectedEq (modTestLines mt) expectedTestLines
          expectedEq (modInputs mt) expectedModInputs 
          expectedEq (modOutputs mt) expectedModOutputs
          expectedEq (modPower mt) (Just (Power 1))
          expectedEq (modThresholds mt) (Just (Thresholds 0 0.1 0.9 1))
          expectedEq (modMode mt) (Just Gate)
          
          return $ print (modOutputs mt)
          return pass
    Left msg -> fail msg





