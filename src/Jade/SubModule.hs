module Jade.SubModule where

import Jade.Common 
import Jade.Module as Module
import Jade.Term as Term


points (SubModule modname loc) = "SubModule.locs" <? do
   m <- getModule modname 
   ts <- Module.terminals m loc 
   return $ concat $ map Term.points ts

