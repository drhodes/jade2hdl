{-# LANGUAGE FlexibleContexts #-}
module Jade.Part where

import Jade.Common
import Control.Monad
import Data.Maybe
import qualified Jade.Port as Port
import qualified Jade.Jumper as Jumper
import qualified Jade.Term as Term
import qualified Jade.Wire as Wire
import qualified Jade.SubModule as SubModule
import qualified Jade.Decode.Sig as Sig
import qualified Jade.Signal as Signal

{-
import qualified Jade.Module as Moduile
import qualified Jade.Signal as Signal
import qualified Jade.Decode.Bundle as Bundle
-}

loc part =
  case part of
    PortC (Port (Coord3 x y _) _) -> return (x, y)
    WireC _ -> die "Part.loc doesn't support Wire"
    TermC (Terminal (Coord3 x y _) _) -> return (x, y)
    x -> die $ "Part.loc: doesn't support: " ++ show x

points :: Part -> J [Point]
points part = "Part.points" <? do
  case part of 
    PortC x -> return $ Port.points x
    SubModuleC x -> SubModule.points x 
    WireC x -> return $ Wire.points x
    JumperC x -> return $ Jumper.points x
    TermC x -> return $ Term.points x
    UnusedPart -> return []

hasPoint :: Part -> Point -> J Bool
hasPoint part point = "Part.hasPoint" <? do
  (point `elem`) <$> (points part)

getSig :: Part -> Maybe Sig
getSig part = case part of
                PortC x -> Port.getSig x
                WireC x -> Wire.getSig x
                TermC x -> Just $ Term.getSig x
                _ -> Nothing

getSignal :: Part -> Maybe Signal
getSignal part = case part of
                   PortC x -> Port.getSignal x
                   WireC x -> Wire.getSignal x
                   _ -> Nothing

putSignal part signal =
  case part of
    WireC w -> WireC $ Wire.putSignal w signal
    x -> x
  
hasSig = isNothing . getSig

isJumper (JumperC _) = True
isJumper _ = False

isWire (WireC _) = True
isWire _ = False

isPort (PortC _) = True
isPort _ = False

isTerm (TermC _) = True
isTerm _ = False

isSubModule (SubModuleC _) = True
isSubModule _ = False

isNamedConnector :: Part -> Bool
isNamedConnector = forSome [isWire, isJumper, isPort] 

filterConnectors = filter isNamedConnector 

width :: Part -> J Int
width part = do
  case part of
    PortC (Port _ (Just s)) -> return $ Signal.width s
    PortC (Port _ Nothing) -> return 1
    WireC (Wire _ (Just s)) -> return $ Signal.width s
    WireC (Wire _ Nothing) -> return 1
    JumperC _ -> return (-1)
    TermC (Terminal _ s) -> return $ fromIntegral $ Sig.width s
    x -> die $ "Part.width: Not implemented for: " ++ show x



{-
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
getBundleWithLit part = if Bundle.hasLit (bundle part)
                        then Just $ bundle part
                        else Nothing

getLitVals part = Bundle.getLitVals (bundle part)

containsIdentifier :: Part -> String -> Bool
containsIdentifier part ident = Bundle.containsIdentifier (bundle part) ident

-- hasAnySigName :: Part -> Bool
-- hasAnySigName part = Bundle.hasAnyValName (bundle part)

hasVal part val = Bundle.hasVal (bundle part) val


toWire (WireC w) = Just w
toWire _ = Nothing


getValsWithIdent :: Part -> String -> [Val]
getValsWithIdent part ident = Bundle.getValsWithIdent (bundle part) ident

removeTerms parts = filter (not . isTerm) parts

getNames part = Bundle.getNames (bundle part)


width :: Part -> J Int
width part = do
  nb $ show part
  case part of
    PortC (Port _ (Just s)) -> return $ Signal.width s
    PortC (Port _ Nothing) -> return 1
    WireC (Wire _ (Just s)) -> do
      nb $ "For this wire: " ++ show part
      nb $ "Found width:   " ++ (show $ Signal.width s)
      return $ Signal.width s
    WireC (Wire _ Nothing) -> return 1
    TermC (Terminal _ s) -> return $ Bundle.width s
    x -> die $ "Part.width: Not implemented for: " ++ show x
-}


