{-# LANGUAGE FlexibleContexts #-}
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

xMin :: IconPart -> J (Maybe Integer)
xMin ipart = "Icon.xMin" <? do
  case ipart of
    IconLine (Line c) -> return $ Just $ Coord.xMinC5 c
    IconTerm (Terminal c _) -> return $ Just $ Coord.c3x c
    IconBox (Box c) -> return $ Just $ Coord.xMinC5 c
    IconTxt (Txt c _ _) -> return $ Just $ Coord.c3x c
    x -> do nb $ "xMin is ignoring: " ++ show x
            return Nothing

xMax ipart = "Icon.xMax" <? do
  case ipart of
    IconLine (Line c) -> return $ Just $ Coord.xMaxC5 c
    IconTerm (Terminal c _) -> return $ Just $ Coord.c3x c
    IconBox (Box c) -> return $ Just $ Coord.xMaxC5 c
    IconTxt (Txt c _ _) -> return $ Just $ Coord.c3x c
    x -> do nb $ "xMax is ignoring: " ++ show x
            return Nothing

yMin ipart = "Icon.yMin" <? do
  case ipart of
    IconLine (Line c) -> return $ Just $ Coord.yMinC5 c
    IconTerm (Terminal c _) -> return $ Just $ Coord.c3y c
    IconBox (Box c) -> return $ Just $ Coord.yMinC5 c
    IconTxt (Txt c _ _) -> return $ Just $ Coord.c3y c
    x -> do nb $ "yMin is ignoring: " ++ show x
            return Nothing

yMax ipart = "Icon.yMax" <? do
  case ipart of
    IconLine (Line c) -> return $ Just $ Coord.yMaxC5 c
    IconTerm (Terminal c _) -> return $ Just $ Coord.c3y c
    IconBox (Box c) -> return $ Just $ Coord.yMaxC5 c
    IconTxt (Txt c _ _) -> return $ Just $ Coord.c3y c
    x -> do nb $ "yMax is ignoring: " ++ show x
            return Nothing

boundingBox :: Icon -> J BoundingBox
boundingBox (Icon parts) = "Icon.boundingBox" <? do
  let f property select = do
        xs <- mapM select parts
        when (null xs) (die $ "All nothings found for this property")
        return $ property [x | Just x <- xs]
  
  xMax <- f maximum xMax
  xMin <- f minimum xMin
  yMax <- f maximum yMax
  yMin <- f minimum yMin
  return $ BB xMin yMin xMax yMax

center icon = "Icon.center" <? do
  BB left top right bottom <- boundingBox icon

  --
  let closest x = let m = x `mod` 8
                  in if m <= 3
                     then x - m
                     else x + (8 - m)
  
      cx = closest $ (right + left) `div` 2
      cy = closest $ (bottom + top) `div` 2
      
  return (cx, cy)
