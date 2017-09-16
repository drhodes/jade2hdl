module TestWire (testTree) where

import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Modul
import qualified Jade.Net as Net
import qualified Jade.Wire as Wire
import Text.Format
import Control.Monad
import TestUtil
import Jade.Rawr.Types 

check topl result = case runJ topl result of
                      Right _ -> return Pass
                      Left msg -> return $ Fail (msg ++ runLog topl result)

testWireEnds modname expectedEnds = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let result = "testWireEnds" <? do
        Module _ schem _ _ <- TopLevel.getModule ("/user/" ++ modname)
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
  check topl result
  
testWireCount modname expNumWires = do
  Right topl <- Decode.decodeTopLevel (format "./test-data/{0}.json" [modname])
  let result = "testWireEnds" <? do
        Module _ schem _ _ <- TopLevel.getModule ("/user/" ++ modname)
        case schem of
          Nothing -> die $ "testWireEnds: No schematic found in module " ++ modname
          Just (Schematic parts) -> do
            let ws = [w | WireC w <- parts]
            if length ws == expNumWires
              then return ()
              else dief "expected {0}, got {1}" [show $ length ws, show expNumWires]
  check topl result

testTreeWireEnds = let t modname exp = TestCase modname (testWireEnds modname exp)
                   in TestTree "Ends" [ t "TestWire1" ((0, 0), (24,-8))
                                      , t "TestWire2" ((16, 8), (8, -16))
                                      ]

testTreeCountWires = let t modname exp = TestCase modname (testWireCount modname exp)
                     in TestTree "Count" [ t "RepAnd2" 3
                                         , t "RepAnd3" 3
                                         , t "RepAnd4" 3
                                         , t "RepWonkyBuffer1" 3
                                         , t "Ripple32" 7
                                         , t "SmallChain" 9                                         
                                         ]

testTree = TestTree "Wire" [ testTreeWireEnds
                           , testTreeCountWires
                           ]
