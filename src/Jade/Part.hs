{-# LANGUAGE FlexibleContexts #-}
module Jade.Part where

import Jade.Types
import Control.Monad
import Data.Maybe

sig :: Part -> Maybe Sig
sig part =
  case part of
    PortC (Port _ (Just s)) -> signalName s
    WireC (Wire _ (Just s)) -> signalName s
    WireC (Wire _ Nothing) -> Nothing
    TermC (Terminal _ s) -> Just s    
    x -> error $ "Partonent.driverSig: Not implemented for: " ++ show x

hasSigName :: Part -> Bool
hasSigName = isJust . sig 



loc part =
  case part of
    PortC (Port (Coord3 x y _) _) -> return (x, y)
    WireC _ -> die "Part.loc doesn't support Wire"
    TermC (Terminal (Coord3 x y _) _) -> return (x, y)
    x -> die $ "Part.loc: doesn't support: " ++ show x
  
