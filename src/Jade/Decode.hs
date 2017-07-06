{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Jade.Decode where

import qualified Data.Scientific as DS
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
import qualified Jade.ModTest as MT
import Jade.Types
import Data.FileEmbed
import qualified Data.String.Class as DSC
import Data.Text.Encoding

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
                  Left msg -> fail (show msg ++ "\n" ++ s)

instance FromJSON Coord3 where
  parseJSON jsn = do
    [x, y, rot] <- parseJSON jsn
    return $ Coord3 x y (toEnum (fromIntegral rot))

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

instance FromJSON Line where
  parseJSON (Array v) =
    if V.length v == 2
    then do
      lineLabel <- parseJSON $ v V.! 0
      when (lineLabel /= ("line" :: String)) $ fail "Not a line"
      c5 <- parseJSON $ v V.! 1
      return $ Line c5
    else fail "Not a line"

instance FromJSON Box where
  parseJSON (Array v) =
    if V.length v == 2
    then do
      boxLabel <- parseJSON $ v V.! 0
      when (boxLabel /= ("box" :: String)) $ fail "Not a box"
      c5 <- parseJSON $ v V.! 1
      return $ Box c5
    else fail "Not a box"

--"text", [ 42, -5, 0 ], { "text": "nd", "font": "4pt sans-serif" } ],
instance FromJSON Txt where
  parseJSON (Array v) = do
    c3 <- parseJSON $ v V.! 1
    Object o <- parseJSON $ v V.! 2
    txt <- o .: "text"
    font <- o .:? "font"
    return $ Txt c3 txt font

-- [ "terminal", [ 16, 0, 4 ], { "name": "out" } ]
instance FromJSON Terminal where
  parseJSON (Array v) = do
    label <- parseJSON $ v V.! 0
    when ((label :: String) /= "terminal") $ fail "Not a terminal"
    
    c3 <- parseJSON $ v V.! 1
    Object o <- parseJSON $ v V.! 2
    
    s <- o .: "name" -- signal name
    
    case Sig.parseSig s of
      Right sig -> return $ Terminal c3 sig 
      Left msg -> fail (show msg)

instance FromJSON IconPart where
  parseJSON arr@(Array v) = do
    kind <- parseJSON $ v V.! 0

    case (kind :: String) of
      "line" -> IconLine <$> parseJSON arr
      "terminal" -> IconTerm <$> parseJSON arr
      "text" -> IconTxt <$> parseJSON arr
      "box" -> IconBox <$> parseJSON arr
      "circle" -> return IconCircle -- todo
      "property" -> return IconProperty -- todo
      "arc" -> return IconArc -- todo
      _ -> fail $ "Unknown IconPart: " ++ kind
      
instance FromJSON Icon where
  parseJSON (Array v) = Icon <$> mapM parseJSON (V.toList v)
        
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

instance FromJSON Part where
  parseJSON v@(Array arr) = do
    ctype <- parseJSON $ arr V.! 0 :: Parser String
    case ctype of
      "wire" ->
        do w <- parseJSON v
           return $ WireC w
      "port" -> PortC <$> parseJSON v
      "jumper" ->
        do p <- parseJSON v
           return $ JumperC p
      _ -> -- this is probably not safe.
        do sub <- parseJSON v
           return $ SubModuleC sub

instance FromJSON Schematic where
  parseJSON (Array v) = do
    cs <- mapM parseJSON v
    return $ Schematic (V.toList cs)

instance FromJSON Module where
  parseJSON (Object o) = do
    schem <- o .:? "schematic"
    icon <- o .:? "icon"
    t <- o .:? "test" -- todo make this safer.
    --let (Just [["test", tstring]]) = t

    case t of
      (Just [["test", tstring]]) ->
        case MT.parseModTestString tstring of
          Right mt -> return $ Module schem (Just mt) icon
          Left msg -> fail msg
      Nothing -> return $ Module schem Nothing icon

instance FromJSON TopLevel where
  parseJSON (Array arr) = do
    mods <- parseJSON $ arr V.! 1
    return $ TopLevel mods

  parseJSON (Object o) = do
    fail $ show o

builtInTxt = decodeUtf8 $(embedFile "app-data/gates.json")
builtInGates :: Either String TopLevel
builtInGates = eitherDecode (DSC.toLazyByteString builtInTxt) 

decodeTopLevel :: FilePath -> IO (Either String TopLevel)
decodeTopLevel filename = do
  case builtInGates of
    Right (TopLevel gates) -> do 
      top <- DBL.readFile filename
      case eitherDecode top of
        Right (TopLevel mods) -> return $ Right $ TopLevel (DM.union gates mods)
        Left msg -> fail msg
    Left msg -> fail $ msg ++ "\nDecode.decodeTopLevel fails to decode 'app-data/gates.json'"

