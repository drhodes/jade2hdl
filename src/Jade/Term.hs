module Jade.Term where

import Jade.Common
import qualified Jade.Bundle as Bundle

points (Terminal (Coord3 x y _) _) = [Point x y]

width (Terminal _ bundle) = Bundle.width bundle
