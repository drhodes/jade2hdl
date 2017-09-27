module Jade.Decode.Coord where

import Jade.Decode.Types

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
    1 -> x
    7 -> x
    2 -> -y
    6 -> -y  
    3 -> -x
    5 -> -x
    0 -> y
    4 -> y

c3ToPoint :: Coord3 -> (Integer, Integer)
c3ToPoint (Coord3 x y r) = (transformX r x y, transformY r x y)

-- https://github.com/6004x/jade/blob/ccb840c91a4248aab1764b1f9d27d832167b32a5/model.js
-- rotation matrix found at the above link, from the JADE project.

composeRot :: Rot -> Rot -> Rot
composeRot r1 r2 = toEnum $
  [ 0, 1, 2, 3, 4, 5, 6, 7 -- NORTH (identity)
  , 1, 2, 3, 0, 7, 4, 5, 6 -- EAST (rot270) rotcw
  , 2, 3, 0, 1, 6, 7, 4, 5 -- SOUTH (rot180)
  , 3, 0, 1, 2, 5, 6, 7, 4 -- WEST (rot90) rotccw
  , 4, 5, 6, 7, 0, 1, 2, 3 -- RNORTH (negx) fliph
  , 5, 6, 7, 4, 3, 0, 1, 2 -- REAST (int-neg)
  , 6, 7, 4, 5, 2, 3, 0, 1 -- RSOUTH (negy) flipy
  , 7, 4, 5, 6, 1, 2, 3, 0 -- RWEST (int-pos)
  ] !! (fromEnum r1 * 8 + fromEnum r2)
  

coord5ends p@(Coord5 x y rot dx dy) =
  let n1 = (x, y)
      Coord3 x' y' _ = rotate (Coord3 dx dy Rot0) rot 0 0
      n2 = (x + x', y + y')
  in (n1, n2)

xMinC5 c5 = let ((x1, _), (x2, _)) = coord5ends c5
            in min x1 x2
xMaxC5 c5 = let ((x1, _), (x2, _)) = coord5ends c5
            in max x1 x2

yMinC5 c5 = let ((_, y1), (_, y2)) = coord5ends c5
            in min y1 y2
yMaxC5 c5 = let ((_, y1), (_, y2)) = coord5ends c5
            in max y1 y2

c3x (Coord3 x _ _) = x --fst . c3ToPoint 
c3y (Coord3 _ y _) = y -- = snd . c3ToPoint 

rotate :: LocRot a => a -> Rot -> Integer -> Integer -> Coord3

locs :: Coord5 -> [Coord3]
locs c5 = let ((x1, y1), (x2, y2)) = coord5ends c5
          in [ Coord3 x1 y1 Rot0
             , Coord3 x2 y2 Rot0]

points :: Coord5 -> [Point]
points c5 = let ((x1, y1), (x2, y2)) = coord5ends c5
            in [ Point x1 y1 
               , Point x2 y2 ]



rotate item rotation cx cy =
  let Coord3 x y r = locrot item
      rx = x - cx
      ry = y - cy
      newX = cx + transformX rotation rx ry
      newY = cy + transformY rotation rx ry
      newR = composeRot r rotation
  in Coord3 newX newY newR

