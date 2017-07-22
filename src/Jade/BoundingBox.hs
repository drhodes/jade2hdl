module Jade.BoundingBox where

import Jade.Types
import qualified Jade.Coord as Coord

-- ensure left < right, top < bottom
canonicalize (BB x1 y1 x2 y2) = BB (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)

-- this may need to be aligned to the 8x8 grid.
center (BB left top right bottom) = let x = (left + right) `div` 2
                                        y = (top + bottom) `div` 2
                                    in (x, y)

rotate bb@(BB left top right bottom) rot =
  let (cx, cy) = center bb
      p1 = Coord3 left top Rot0
      p2 = Coord3 right bottom Rot0
      Coord3 x1 y1 _ = Coord.rotate p1 rot cx cy
      Coord3 x2 y2 _ = Coord.rotate p2 rot cx cy
  in canonicalize (BB x1 y1 x2 y2)

translate bb@(BB left top right bottom) (Coord3 dx dy _) =
  BB (left + dx) (top + dy) (right + dx) (bottom + dy)


transRot bb c3@(Coord3 _ _ r) = rotate (translate bb c3) r
