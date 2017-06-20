module Jade.Component where

import Jade.Types
import Control.Monad

sig :: Component -> Maybe Sig
sig comp =
  case comp of
    PortC (Port _ (Just s)) -> signalName s
    WireC (Wire _ (Just s)) -> signalName s
    WireC (Wire _ Nothing) -> Nothing
    TermC (Terminal _ s) -> Just s    
    x -> error $ "Component.driverSig: Not implemented for: " ++ show x
      
