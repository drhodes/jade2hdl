module Jade.TopLevel where

import qualified Data.Map as DM
import Jade.Types

modules (TopLevel m) = DM.toList m
getModule (TopLevel m) name = DM.lookup name m
