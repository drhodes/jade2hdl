module TestCoord where

import qualified Data.Map as DM
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.Vhdl as Vhdl
import qualified Data.Hashable as H
import Text.Format
import Control.Monad
import qualified Jade.Coord as Coord

c00 = Coord3 0 0 Rot0
c10 = Coord3 1 0 Rot0
c01 = Coord3 0 1 Rot0
c11 = Coord3 1 1 Rot0
r90 = Coord3 0 0 Rot90

l1 = Coord5 (-16) 16 Rot90  32 0
l2 = Coord5 (-16) (-16)  Rot90 0 32
l3 = Coord5 16 (-16)  Rot90 (-32) 0
l4 = Coord5 16 16 Rot90 0 (-32) 

-- coord0 = print $ Coord.transform3 c00 c00
-- coord11 = print $ Coord.transform3 c11 c11
-- coordR90 = print $ Coord.transform3 c11 r90

-- TODO: consider adding this test to TestAll module
testAll = do
  print $ Coord.coord5ends l1
  print $ Coord.coord5ends l2
  print $ Coord.coord5ends l3
  print $ Coord.coord5ends l4
  
