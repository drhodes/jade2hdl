{-# LANGUAGE FlexibleContexts #-}
module Jade.Part where

import Jade.Types
import Control.Monad
import Data.Maybe
import qualified Jade.Sig as Sig
import qualified Jade.Signal as Signal
import qualified Jade.Bundle as Bundle

bundle :: Part -> ValBundle
bundle part =
  case part of
    PortC (Port _ (Just s)) -> Signal.getBundle s
    WireC (Wire _ (Just s)) -> Signal.getBundle s
    WireC (Wire _ Nothing) -> mempty
    TermC (Terminal _ s) -> s  
    x -> error $ "Part.sig: Not implemented for: " ++ show x

getBundleWithIdent part ident = if Bundle.hasName (bundle part) ident
                                then Just $ bundle part
                                else Nothing 


getLitVals part = Bundle.getLitVals (bundle part)

containsIdentifier :: Part -> String -> Bool
containsIdentifier part ident = Bundle.containsIdentifier (bundle part) ident

hasAnySigName :: Part -> Bool
hasAnySigName part = Bundle.hasAnyValName (bundle part)

hasVal part val = Bundle.hasVal (bundle part) val

isJumper (JumperC _) = True
isJumper _ = False

isSubModule (SubModuleC _) = True
isSubModule _ = False

isTerm (TermC _) = True
isTerm _ = False

getValsWithIdent :: Part -> String -> [Val]
getValsWithIdent part ident = Bundle.getValsWithIdent (bundle part) ident

getNames part = Bundle.getNames (bundle part)

loc part =
  case part of
    PortC (Port (Coord3 x y _) _) -> return (x, y)
    WireC _ -> die "Part.loc doesn't support Wire"
    TermC (Terminal (Coord3 x y _) _) -> return (x, y)
    x -> die $ "Part.loc: doesn't support: " ++ show x

width :: Part -> J (Maybe Int)
width part = do
  nb $ show part
  case part of 
    PortC (Port _ (Just s)) -> return $ Signal.width s
    PortC (Port _ Nothing) -> return Nothing
    WireC (Wire _ (Just s)) -> do
      nb $ "For this wire: " ++ show part
      nb $ "Found width:   " ++ (show $ Signal.width s)
      return $ Signal.width s
    WireC (Wire _ Nothing) -> return $ Just 1
    TermC (Terminal _ s) -> return $ Just $ Bundle.width s
    x -> die $ "Part.width: Not implemented for: " ++ show x


