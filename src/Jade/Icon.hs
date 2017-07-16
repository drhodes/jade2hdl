module Jade.Icon where

import Control.Monad
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as DV
import qualified Jade.Decode as D
import qualified Jade.Part as Part
import qualified Jade.Coord as Coord
import Jade.Types
import Jade.Wire



-- data IconPart = IconLine Line
--               | IconTerm Terminal
--               | IconBox Box
--               | IconTxt Txt
--               | IconCircle
--               | IconProperty
--               | IconArc
--                 deriving (Generic, Show, Eq, Hashable, Ord)



xMin :: IconPart -> J Integer
xMin ipart = "Icon.xMin" <? do
  case ipart of
    IconLine (Line c) -> return $ Coord.xMinC5 c
    IconTerm (Terminal c _) -> return $ Coord.c3x c
    IconBox (Box c) -> return $ Coord.xMinC5 c
    IconTxt (Txt c _ _) -> return $ Coord.c3x c
    -- IconTxt Txt
    x -> die $ "This IconPart not supported yet: " ++ show x

xMax :: IconPart -> J Integer
xMax ipart = "Icon.xMax" <? do
  case ipart of
    IconLine (Line c) -> return $ Coord.xMaxC5 c
    IconTerm (Terminal c _) -> return $ Coord.c3x c
    IconBox (Box c) -> return $ Coord.xMaxC5 c
    IconTxt (Txt c _ _) -> return $ Coord.c3x c
    -- IconTxt Txt
    x -> die $ "This IconPart not supported yet: " ++ show x

yMin :: IconPart -> J Integer
yMin ipart = "Icon.yMin" <? do
  case ipart of
    IconLine (Line c) -> return $ Coord.yMinC5 c
    IconTerm (Terminal c _) -> return $ Coord.c3y c
    IconBox (Box c) -> return $ Coord.yMinC5 c
    IconTxt (Txt c _ _) -> return $ Coord.c3y c
    -- IconTxt Txt
    y -> die $ "This IconPart not supported yet: " ++ show y


yMax :: IconPart -> J Integer
yMax ipart = "Icon.yMax" <? do
  case ipart of
    IconLine (Line c) -> return $ Coord.yMaxC5 c
    IconTerm (Terminal c _) -> return $ Coord.c3y c
    IconBox (Box c) -> return $ Coord.yMaxC5 c
    IconTxt (Txt c _ _) -> return $ Coord.c3y c
    y -> die $ "This IconPart not supported yet: " ++ show y


boundingBox (Icon parts) = "iconBoundingBox" <? do
  nb $ show parts
  xMax <- liftM maximum $ mapM xMax parts
  xMin <- liftM minimum $ mapM xMin parts
  yMax <- liftM maximum $ mapM yMax parts
  yMin <- liftM maximum $ mapM yMin parts
  let upperLeft = (xMin, yMin)
      bottomRight = (xMax, yMax)
  return (upperLeft, bottomRight)
  
