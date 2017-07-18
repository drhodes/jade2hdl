module Jade.Coord where

import Jade.Types 

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
    0 -> y
    1 -> x
    2 -> -y
    3 -> -x
    4 -> y
    5 -> -x
    6 -> -y  
    7 -> x

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
  

       -- // compute new position and rotation
       -- var new_x = transform_x(rotation, rx, ry) + cx;
       -- var new_y = transform_y(rotation, rx, ry) + cy;
       -- var new_rotation = rotate[old_rotation * 8 + rotation];
  
-- https://github.com/6004x/jade/blob/ccb840c91a4248aab1764b1f9d27d832167b32a5/model.js#L868
-- Component.prototype.rotate
transform3 (Coord3 x y r) (Coord3 dx dy dr) cx cy =
  let oldX = x
      oldY = y
      oldR = r
      rx = oldX - cx
      ry = oldY - cy

      newX = cx + transformX r rx ry
      newY = cy + transformY r rx ry 
      newR = composeRot r dr
 
  in Coord3 newX newY newR

coord5ends (Coord5 x y rot dx dy) =
  let x' = x + (transformX rot dx dy)
      y' = y + (transformY rot dx dy)
      n1 = (x, y)
      n2 = (x', y')
  in (n1, n2)


xMinC5 c5 = let ((x1, _), (x2, _)) = coord5ends c5
            in min x1 x2
xMaxC5 c5 = let ((x1, _), (x2, _)) = coord5ends c5
            in max x1 x2

yMinC5 c5 = let ((_, y1), (_, y2)) = coord5ends c5
            in min y1 y2
yMaxC5 c5 = let ((_, y1), (_, y2)) = coord5ends c5
            in max y1 y2

c3x = fst . c3ToPoint 
c3y = snd . c3ToPoint 


-- xMinC3
-- xMaxC3
-- yMinC3
-- yMaxC3
