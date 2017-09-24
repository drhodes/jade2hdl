module Jade.Port where

import Jade.Common

points (Port (Coord3 x y _) _) = [Point x y]
