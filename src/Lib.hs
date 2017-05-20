{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lib where

import Control.Monad
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified System.Environment as SE
import qualified Sig as Sig


{-
A JADE exported module is encoded as JSON.

list length 2.
 First item is a string "Jade"
 Snd item is map from String module name to module structure

 the export is a map with keys: [test, properties, schematic, icon]

this project aims to generate HDL from the module code and emit HDL.
also to emit the tests as HDL tests.

 module.test        is a list pair, ["test", "the text of the test"].
 module.properties  is a map with key "name" to a simple key/val map.
 module.schematic   is a list of schematic components.
 module.icon        is a list of icon components

 components in the schematic:
 Port(Location, SignalName)
 Module(ModuleName, Coord3(x, y, rot)
 Wire(Coord5(x, y, rot, dx, dy), SignalName) # check that Coord5 is correct.
-}

data Direction = In | Out | InOut deriving (Show, Eq)

instance FromJSON Direction where
  parseJSON (String txt) =
    return $ case txt of
               "in" -> In
               "out" -> Out
               "inout" -> InOut

data Signal = Signal { signalName :: Maybe Sig.Sig
                     , signalWidth :: Maybe String
                     , signalDirection :: Maybe Direction
                     } deriving (Show, Eq)

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

data Coord3 = Coord3 { c3x :: Integer
                     , c3y :: Integer
                     , c3r :: Integer
                     } deriving (Show, Eq)

instance FromJSON Coord3 where
  parseJSON jsn = do
    [x, y, rot] <- parseJSON jsn
    return $ Coord3 x y rot

data Coord5 = Coord5 { c5x :: Integer
                     , c5y :: Integer
                     , c5r :: Integer
                     , c5dx :: Integer
                     , c5dy :: Integer
                     } deriving (Show, Eq)

instance FromJSON Coord5 where
  parseJSON jsn = do
    [x, y, rot, dx, dy] <- parseJSON jsn
    return $ Coord5 x y rot dx dy

data Wire = Wire { wireCoord5 :: Coord5
                 , wireSignal :: Maybe Signal
                 } deriving (Show, Eq)

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

data Port = Port { portCoord3 :: Coord3                 
                 , portSignal :: Maybe Signal
                 } deriving (Show, Eq)
               
instance FromJSON Port where
  parseJSON (Array v) = do
    c3 <- parseJSON $ v V.! 1
    if V.length v == 3
      then do sig <- parseJSON $ v V.! 2
              return $ Port c3 (Just sig)
      else return $ Port c3 Nothing
      
data SubModule = SubModule { subName :: String
                           , subCoord3 :: Coord3
                           } deriving (Show, Eq)

instance FromJSON SubModule where
  parseJSON (Array v) = do
    name <- parseJSON $ v V.! 0
    sub <- parseJSON $ v V.! 1
    return $ SubModule name sub

data Component = PortC Port
               | SubModuleC SubModule
               | WireC Wire
               | Nop
                 deriving (Show, Eq)

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
      _ -> -- this is probably not safe.
        do sub <- parseJSON v
           return $ SubModuleC sub

type Test = String
data Schematic = Schematic (V.Vector Component) deriving (Show, Eq)

instance FromJSON Schematic where
  parseJSON (Array v) = do
    cs <- mapM parseJSON v
    return $ Schematic cs

data Module = Module Schematic deriving (Show, Eq) -- todo add test

instance FromJSON Module where
  parseJSON (Object o) = do
    schem <- o .: "schematic"
    return $ Module schem 

data TopLevel = TopLevel (DM.Map String (Maybe Module))
              deriving  (Show, Eq)

instance FromJSON TopLevel where
  parseJSON (Array arr) = do
    mods <- parseJSON $ arr V.! 1
    return $ TopLevel mods

decodeTopLevel :: String -> IO (Either String TopLevel)
decodeTopLevel filename = do
  top <- DBL.readFile filename
  return $ eitherDecode top

  
testWire1 :: Either String Wire
testWire1 = eitherDecode "[\"wire\", [136, 64, 1, 0, 0], {\"signal\": \"wd\"}]" 

testSignal1 :: Either String Signal
testSignal1 = eitherDecode "{\"signal\": \"0'1\"}" 


testWireRange :: Either String Wire
testWireRange = eitherDecode "[\"wire\", [136, 64, 1, 0, 0], {\"signal\": \"wd[1:0]\"}]" 

testWireIndex :: Either String Wire
testWireIndex = eitherDecode "[\"wire\", [136, 64, 1, 0, 0], {\"signal\": \"wd[1]\"}]" 

testPort1 :: Either String Port
testPort1 = eitherDecode "[\"port\", [136, 64, 1], {\"signal\": \"wd\"}]" 

testPort2 :: Either String Port
testPort2 = eitherDecode "[\"port\", [136, 64, 1], {\"signal\": \"wd\", \"direction\":\"out\"}]" 

testComp1 :: Either String Component
testComp1 = eitherDecode "[\"port\", [136, 64, 1], {\"signal\": \"wd\", \"direction\":\"out\"}]" 

testSub1 :: Either String SubModule
testSub1 = eitherDecode "[\"/user/and\", [136, 64, 1]]"

testSchem1 :: Either String Schematic
testSchem1 = eitherDecode "[[\"port\", [136, 64, 1], {\"signal\": \"wd\"}],[\"port\", [136, 64, 1], {\"signal\": \"wd\"}]]"

schem2 = "[[\"wire\", [136, 64, 1, 0, 0], {\"signal\": \"wd\"}],[\"port\", [136, 64, 1], {\"signal\": \"wd\"}]]" 
testSchem2 :: Either String Schematic
testSchem2 = eitherDecode schem2

