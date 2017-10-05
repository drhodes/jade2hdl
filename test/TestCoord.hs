module TestCoord where

import Jade.Common
import qualified Jade.Decode.Coord as Coord

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
l5 = Coord5 10 10 Rot0 (-10) (-10) 
l6 = Coord5 10 10 Rot90 0 10
l7 = Coord5 0 10; Rot90 0 (-10)

testAll = do
  print $ Coord.coord5ends l1
  print $ Coord.coord5ends l2
  print $ Coord.coord5ends l3
  print $ Coord.coord5ends l4
  print $ Coord.coord5ends l5
  print $ Coord.coord5ends l6
  print $ Coord.coord5ends l7
