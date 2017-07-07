module Jade.Coord where

import Jade.Types 

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


coord5ends (Coord5 x y rot dx dy) =
  let x' = x + (transformX rot dx dy)
      y' = y + (transformY rot dx dy)
      n1 = (x, y)
      n2 = (x', y')
  in (n1, n2)

