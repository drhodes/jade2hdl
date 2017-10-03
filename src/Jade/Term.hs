module Jade.Term where

import Jade.Common
import qualified Jade.Bundle as Bundle

points (Terminal (Coord3 x y _) _) = [Point x y]

--width (Terminal _ bundle) = Bundle.width bundle

atPoint (Terminal (Coord3 x1 y1 _) _) (Point x2 y2) = x1 == x2 && y1 == y2
getSig (Terminal _ s) = s
