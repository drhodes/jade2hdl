module Jade.Wire where

import Jade.Common
import qualified Jade.Signal as Signal
import qualified Jade.Decode.Coord as Coord

ends w@(Wire c5 _) = Coord.coord5ends c5
points (Wire c5 _) = Coord.points c5

isAnon (Wire _ Nothing) = True
isAnon _ = False

getSig (Wire _ Nothing) = Nothing
getSig (Wire _ (Just s1)) = Signal.getSig s1


putSignal (Wire c _) signal = Wire c signal
getSignal (Wire _ s) = s




-- hasSigName (Wire _ Nothing) = False
-- hasSigName (Wire _ (Just s1)) = Signal.hasSigName s1


{-
toEdge w@(Wire (Coord5 x y rot dx dy) _) =
  let x' = x + (Coord.transformX rot dx dy)
      y' = y + (Coord.transformY rot dx dy)
      n1 = Node (x, y) (WireC w)
      n2 = Node (x', y') (WireC w)
  in Edge n1 n2


hasSameSig (Wire _ Nothing) _ = False
hasSameSig _ (Wire _ Nothing) = False
hasSameSig (Wire _ s1) (Wire _ s2) = s1 == s2




width :: Wire -> Int
width (Wire _ (Just signal)) = Signal.width signal
width _ = 1

getBundle (Wire _ (Just signal)) = Signal.getBundle signal
getBundle _ = Bundle []



explode (Wire loc (Just signal)) = [Wire loc (Just subsig) | subsig <- Signal.explode signal]
explode w = [w]

new (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
  in Wire (Coord5 x1 y1 Rot0 dx dy) Nothing


portToEdge p@(Port (Coord3 x y r) _) =
  let n = Node (x, y) (PortC p)
  in Edge n n

mkDegenerate c = new (Coord.c3ToPoint c) (Coord.c3ToPoint c)


getIndexesWithName (Wire _ (Just signal)) name = Signal.getIndexesWithName signal name
getIndexesWithName _ _ = []
-}
