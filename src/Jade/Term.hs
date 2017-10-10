module Jade.Term where

import Jade.Common
import qualified Jade.Bundle as Bundle
import qualified Jade.Decode.Sig as Sig

points (Terminal (Coord3 x y _) _) = [Point x y]

width :: Terminal -> Integer
width (Terminal _ sig) = Sig.width sig

atPoint (Terminal (Coord3 x1 y1 _) _) (Point x2 y2) = x1 == x2 && y1 == y2
getSig (Terminal _ s) = s


x (Terminal (Coord3 n _ _) _) = n
y (Terminal (Coord3 _ n _) _) = n

point term = Point (x term) (y term)

