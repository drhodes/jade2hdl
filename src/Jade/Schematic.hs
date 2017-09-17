module Jade.Schematic ( getJumpers
                      , getSubModules
                      ) where

import Jade.Common

getJumpers :: Schematic -> [Jumper]
getJumpers (Schematic parts) = [j | (JumperC j) <- parts] --filter Part.isJumper parts


getSubModules :: Schematic -> [SubModule]
getSubModules (Schematic parts) = [s | (SubModuleC s) <- parts]
