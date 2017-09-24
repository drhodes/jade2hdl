module Jade.Schematic ( getJumpers
                      , getSubModules
                      , getAllPartsAtPoint
                      , getAllParts
                      ) where

import Jade.Common
import qualified Jade.Part as Part
import Control.Monad

getJumpers :: Schematic -> [Jumper]
getJumpers (Schematic parts) = [j | (JumperC j) <- parts] --filter Part.isJumper parts

getSubModules :: Schematic -> [SubModule]
getSubModules (Schematic parts) = [s | (SubModuleC s) <- parts]

getAllParts (Schematic parts) = parts

getAllPartsAtPoint :: Schematic -> Point -> J [Part]
getAllPartsAtPoint schem point = "Schematic.getAllPartsAtPoint" <? do  
  filterM (flip Part.hasPoint point) (getAllParts schem)
