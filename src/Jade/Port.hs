module Jade.Port where

import Control.Monad
import Jade.Common
import Jade.Signal as Signal

points (Port (Coord3 x y _) _) = [Point x y]


getSig :: Port -> Maybe Sig
getSig (Port _ s) = join $ liftM Signal.getSig s
