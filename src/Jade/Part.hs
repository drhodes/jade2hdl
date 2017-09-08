{-# LANGUAGE FlexibleContexts #-}
module Jade.Part where

import Jade.Types
import Control.Monad
import Data.Maybe
import qualified Jade.Sig as Sig
import qualified Jade.Signal as Signal

sig :: Part -> Maybe Sig
sig part =
  case part of
    PortC (Port _ (Just s)) -> signalName s
    WireC (Wire _ (Just s)) -> signalName s
    WireC (Wire _ Nothing) -> Nothing
    TermC (Terminal _ s) -> Just s    
    x -> error $ "Parse.sig: Not implemented for: " ++ show x


containsIdentifier :: Part -> String -> J Bool
containsIdentifier part ident = case sig part of
                                  Just s -> (ident `elem`) <$> Sig.getNames s
                                  Nothing -> return False

hasSigName :: Part -> Bool
hasSigName = isJust . sig 

isJumper (JumperC _) = True
isJumper _ = False

isSubModule (SubModuleC _) = True
isSubModule _ = False

isTerm (TermC _) = True
isTerm _ = False


loc part =
  case part of
    PortC (Port (Coord3 x y _) _) -> return (x, y)
    WireC _ -> die "Part.loc doesn't support Wire"
    TermC (Terminal (Coord3 x y _) _) -> return (x, y)
    x -> die $ "Part.loc: doesn't support: " ++ show x
  


width :: Part -> J (Maybe Integer)
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
    TermC (Terminal _ s) -> return $ Just $ Sig.width s
    x -> die $ "Part.width: Not implemented for: " ++ show x
