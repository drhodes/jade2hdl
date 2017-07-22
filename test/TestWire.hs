module Test.TestWire where

import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Modul
import qualified Jade.GComp as GComp
import qualified Jade.Wire as Wire
import Text.Format
import Control.Monad


expected :: Show a => a -> a -> J ()
expected exp got = die $ format "Expected {0}, Got: {1}" [show exp, show got]


testWireEnds modname expectedEnds = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let result = "testWireEnds" <? do
        Module schem _ _ <- TopLevel.getModule topl ("/user/" ++ modname)
        case schem of
          Nothing -> die $ "testWireEnds: No schematic found in module " ++ modname
          Just (Schematic parts) -> do
            let ws = [w | WireC w <- parts]
            when (length ws /= 1) (die $ "expected just one wire, found: " ++ (show $ length ws))

            let ends = Wire.ends (head ws)
            if not (expectedEnds == ends)
              then do nb $ show ends
                      nb $ show ws
                      expected expectedEnds ends
              else return ()

  case runJ result of
    Right _ -> print "PASS"
    Left msg -> do putStrLn $ runLog result
                   fail msg

testAll = do
  testWireEnds "TestWire1" ((0, 0), (24,-8))
  testWireEnds "TestWire2" ((16, 8), (8, -16))