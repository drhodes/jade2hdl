{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Jade.Decode where

import GHC.Generics
import Control.Monad
import Data.Traversable
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified System.Environment as SE
import qualified Jade.Sig as Sig
import Jade.Types

{-
A JADE exported module is encoded as JSON.

list length 2.
 First item is a string "Jade"
 Snd item is map from String module name to module structure

 the export is a map with keys: [test, properties, schematic, icon]

this project aims to generate HDL from jade schematics, also to emit
the tests as HDL tests.

 module.test        is a list pair, ["test", "the text of the test"].
 module.properties  is a map with key "name" to a simple key/val map.
 module.schematic   is a list of schematic components.
 module.icon        is a list of icon components

 components in the schematic:
 Port(Location, SignalName)
 Module(ModuleName, Coord3(x, y, rot)
 Wire(Coord5(x, y, rot, dx, dy), SignalName) 
-}

instance FromJSON Direction where
  parseJSON (String txt) =
    return $ case txt of
               "in" -> In
               "out" -> Out
               "inout" -> InOut

instance FromJSON Signal where
  parseJSON (Object o) = do
    sigString <- o .:? "signal"
    w <-  o .:? "width" 
    dir <-  o .:? "direction"

    case sigString of
      Nothing -> return $ Signal Nothing w dir
      Just s -> case Sig.parseSig s of
                  Right sig -> return $ Signal (Just sig) w dir
                  Left msg -> fail (show msg)

instance FromJSON Coord3 where
  parseJSON jsn = do
    [x, y, rot] <- parseJSON jsn
    return $ Coord3 x y rot

instance FromJSON Coord5 where
  parseJSON jsn = do
    [x, y, rot, dx, dy] <- parseJSON jsn
    return $ Coord5 x y (toEnum (fromInteger rot)) dx dy

instance FromJSON Wire where
  parseJSON (Array v) =
    if V.length v == 2
    then do
      c5 <- parseJSON $ v V.! 1
      return $ Wire c5 Nothing -- no signal 
    else do
      c5 <- parseJSON $ v V.! 1
      sig <- parseJSON $ v V.! 2
      return $ Wire c5 (Just sig)
               
instance FromJSON Port where
  parseJSON (Array v) = do
    c3 <- parseJSON $ v V.! 1
    if V.length v == 3
      then do sig <- parseJSON $ v V.! 2
              return $ Port c3 (Just sig)
      else return $ Port c3 Nothing

instance FromJSON SubModule where
  parseJSON (Array v) = do
    name <- parseJSON $ v V.! 0
    sub <- parseJSON $ v V.! 1
    return $ SubModule name sub

instance FromJSON Jumper where
  parseJSON (Array v) = do
    c3 <- parseJSON $ v V.! 1
    return $ Jumper c3 

instance FromJSON Component where
  parseJSON v@(Array arr) = do
    ctype <- parseJSON $ arr V.! 0 :: Parser String
    case ctype of
      "wire" ->
        do w <- parseJSON v
           return $ WireC w
      "port" ->
        do p <- parseJSON v
           return $ PortC p
      "jumper" ->
        do p <- parseJSON v
           return $ JumperC p
      _ -> -- this is probably not safe.
        do sub <- parseJSON v
           return $ SubModuleC sub

instance FromJSON Schematic where
  parseJSON (Array v) = do
    cs <- mapM parseJSON v
    return $ Schematic cs

instance FromJSON Module where
  parseJSON (Object o) = do
    schem <- o .: "schematic"
    return $ Module schem 

instance FromJSON TopLevel where
  parseJSON (Array arr) = do
    mods <- parseJSON $ arr V.! 1
    return $ TopLevel mods

decodeTopLevel :: String -> IO (Either String TopLevel)
decodeTopLevel filename = do
  top <- DBL.readFile filename
  return $ eitherDecode top
  
