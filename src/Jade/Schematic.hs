module Jade.Schematic where

import Jade.Part as Part
import Jade.Types


getJumpers :: Schematic -> [Jumper]
getJumpers (Schematic parts) = [j | (JumperC j) <- parts] --filter Part.isJumper parts


getSubModules :: Schematic -> [SubModule]
getSubModules (Schematic parts) = [s | (SubModuleC s) <- parts]
