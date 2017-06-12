module Jade.Wire where

import Jade.Types
import qualified Jade.Graph as G
-- --Wire(Coord5(x, y, rot, dx, dy), SignalName) 
-- rotateWire :: Wire -> Integer -> Wire
-- rotateWire wire rot =
--   case rot of 

transformX rot x y =
  case fromEnum rot of
    0 -> x
    1 -> -y
    2 -> -x
    3 -> y
    4 -> -x
    5 -> -y
    6 -> x
    7 -> y

transformY rot x y =
  case fromEnum rot of
    0 -> y
    1 -> x
    2 -> -y
    3 -> -x
    4 -> y
    5 -> -x
    6 -> -y  
    7 -> x

wireToEdge w@(Wire (Coord5 x y rot dx dy) _) =
  let x' = x + (transformX rot dx dy)
      y' = y + (transformY rot dx dy)
      n1 = G.Node (x, y) (WireC w)
      n2 = G.Node (x', y') (WireC w)
  in G.Edge n1 n2

w1 = Wire (Coord5 0 0 Rot0 8 0) Nothing
w2 = Wire (Coord5 0 0 Rot270 8 0) Nothing

portToEdge p@(Port (Coord3 x y r) _) =
  let n = G.Node (x, y) (PortC p)
  in G.Edge n n
  
