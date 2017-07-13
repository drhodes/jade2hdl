module Jade.Wire where

import Jade.Types
import qualified Jade.Coord as Coord

toEdge w@(Wire (Coord5 x y rot dx dy) _) =
  let x' = x + (Coord.transformX rot dx dy)
      y' = y + (Coord.transformY rot dx dy)
      n1 = Node (x, y) (WireC w)
      n2 = Node (x', y') (WireC w)
  in Edge n1 n2

w1 = Wire (Coord5 0 0 Rot0 8 0) Nothing
w2 = Wire (Coord5 0 0 Rot270 8 0) Nothing

ends w@(Wire c5 _) = Coord.coord5ends c5

hasSameSig (Wire _ Nothing) _ = False
hasSameSig _ (Wire _ Nothing) = False
hasSameSig (Wire _ s1) (Wire _ s2) = s1 == s2

new (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
  in Wire (Coord5 x1 y1 Rot0 dx dy) Nothing


portToEdge p@(Port (Coord3 x y r) _) =
  let n = Node (x, y) (PortC p)
  in Edge n n
  
