module Jade.BoundingBox where

import Jade.Types

-- ensure left < right, top < bottom
canonicalize (BB x1 y1 x2 y2) = BB (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)
  
