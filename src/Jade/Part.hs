{-# LANGUAGE FlexibleContexts #-}
module Jade.Part ( isTerm
                 , isSubModule
                 , removeTerms
                 , bundle
                 , getValsWithIdent
                 , getBundleWithIdent
                 , getBundleWithLit
                 , width
                 , hasVal
                 , getLitVals
                 , containsIdentifier
                 , getNames
                 , loc
                 , toWire
                 , hasPoint
                 ) where

import Jade.Common
import Control.Monad
import Data.Maybe
import qualified Jade.Port as Port
import qualified Jade.Jumper as Jumper
import qualified Jade.SubModule as SubModule
import qualified Jade.Module as Moduile
import qualified Jade.Wire as Wire
import qualified Jade.Signal as Signal
import qualified Jade.Decode.Bundle as Bundle
import qualified Jade.Term as Term

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

-- isJumper (JumperC _) = True
-- isJumper _ = False

-- isWire (WireC _) = True
-- isWire _ = False

toWire (WireC w) = Just w
toWire _ = Nothing

isSubModule (SubModuleC _) = True
isSubModule _ = False


isTerm (TermC _) = True
isTerm _ = False

getValsWithIdent :: Part -> String -> [Val]
getValsWithIdent part ident = Bundle.getValsWithIdent (bundle part) ident

removeTerms parts = filter (not . isTerm) parts

getNames part = Bundle.getNames (bundle part)

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
  enb ("PART!", part)
  asdf <- points part
  enb (point, asdf)
  (point `elem`) <$> (points part)

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
