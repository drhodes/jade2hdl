module Jade.Jumper where

import Jade.Common
import qualified Jade.Decode.Coord as Coord

getEnds :: Jumper -> ((Integer, Integer), (Integer, Integer))
getEnds (Jumper (Coord3 x y r)) = Coord.coord5ends (Coord5 x y r 8 0)
points (Jumper (Coord3 x y r)) = [Point x y]
