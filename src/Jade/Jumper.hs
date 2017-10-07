module Jade.Jumper where

import Jade.Common
import qualified Jade.Decode.Coord as Coord

getEnds :: Jumper -> ((Integer, Integer), (Integer, Integer))
getEnds (Jumper (Coord3 x y r)) = Coord.coord5ends (Coord5 x y r 8 0)

points jumper = let ((x1, y1), (x2, y2)) = getEnds jumper
                in [Point x1 y1, Point x2 y2]


