module Jade.Schematic where

import Jade.Common
import qualified Jade.Part as Part
import qualified Jade.Wire as Wire
import Control.Monad

getSubModules :: Schematic -> [SubModule]
getSubModules (Schematic parts) = [s | (SubModuleC s) <- parts]

getJumpers :: Schematic -> [Jumper]
getJumpers (Schematic parts) = [j | (JumperC j) <- parts] --filter Part.isJumper part

getWires (Schematic parts) = [x | (WireC x) <- parts] --filter Part.isJumper part

getAllParts (Schematic parts) = parts

getAllPartsAtPoint :: Schematic -> Point -> J [Part]
getAllPartsAtPoint schem point = "Schematic.getAllPartsAtPoint" <? do  
  filterM (flip Part.hasPoint point) (getAllParts schem)

getAnonWires schem = map Wire.isAnon $ getWires schem



findSig schem wire seen = do
  let [p1, p2] = Wire.points wire
  nbrsL <- getAllPartsAtPoint schem p1
  nbrsR <- getAllPartsAtPoint schem p2
  return $ filter Part.hasSig (nbrsL ++ nbrsR)
