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
import qualified Text.Read as TR
import qualified Data.Maybe as M
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
  parseJSON (String txt) = "Decode.Direction.String" <?? do
    return $ case txt of
               "in" -> In
               "out" -> Out
               "inout" -> InOut

instance FromJSON Signal where
  parseJSON (Object o) = "Decode.Signal.Object" <?? do
    sigString <- o .:? "signal"
    w <- (o .:? "width")  -- ?? "ASDFASDF" -- this could fail obstrusively.
    dir <-  o .:? "direction"

    let width = case w of
                  Nothing -> Just 1
                  Just str -> TR.readMaybe str
      
    when (M.isNothing width) (fail "Couldn't read width in Decode.Signal")

    case sigString of
      Nothing -> return $ Signal Nothing width dir
      Just s -> case Sig.parseSig s of
                  Right sig -> return $ Signal (Just sig) width dir
                  Left msg -> fail (show msg ++ "\n" ++ s)

instance FromJSON Coord3 where
  parseJSON jsn = do
    [x, y, rot] <- parseJSON jsn
    return $ Coord3 x y (toEnum (fromIntegral rot))

instance FromJSON Coord5 where
  parseJSON jsn = do
    [x, y, rot, dx, dy] <- parseJSON jsn
    return $ Coord5 x y (toEnum (fromInteger rot)) dx dy

(??) p s = modifyFailure ((" " ++ s ++ " ") ++ ) p
(<??) s p = p ?? ("\n" ++ s)

instance FromJSON Wire where
  parseJSON (Array v) = "Decode.Wire" <?? do
    if V.length v == 2
    then do
      c5 <- parseJSON $ v V.! 1
      return $ Wire c5 Nothing -- no signal 
    else do
      c5 <- (parseJSON $ v V.! 1)
      sig <- (parseJSON $ v V.! 2)
      return $ Wire c5 (Just sig)

-- [ "line", [ 40, 8, 0, -4, 0 ] ]
instance FromJSON Line where
  parseJSON (Array v) = "Decode.Line.Array" <??
    if V.length v == 2
    then do
      lineLabel <- parseJSON $ v V.! 0
      when (lineLabel /= ("line" :: String)) $ fail "Not a line"
      c5 <- parseJSON $ v V.! 1
      return $ Line c5
    else fail "Not a line"

instance FromJSON Box where
  parseJSON (Array v) = "Decode.Box.Array" <??
    if V.length v == 2
    then do
      boxLabel <- parseJSON $ v V.! 0
      when (boxLabel /= ("box" :: String)) $ fail "Not a box"
      c5 <- parseJSON $ v V.! 1
      return $ Box c5
    else fail "Not a box"

instance FromJSON Circle where
  -- [ "circle", [ x, y, r, filled ] ]
  parseJSON (Array v) = "Decode.Circle.Array" <?? do
    if V.length v == 2
    then do 
      lbl <- parseJSON (v V.! 0) :: (Parser String)
      when (lbl /= "circle") (fail "Not a circle")

      ints <- parseJSON (v V.! 1) :: Parser [Double]
      case ints of
        -- there is a filled number in this vector that may be
        -- floating, so above the array is parsed as doubles otherwise
        -- parsec complains about floats being there. Here they get
        -- floored to integers.        
        [x, y, r, _] -> return $ Circle (floor x) (floor y) (floor r)
        _ -> fail "Not a circle"
    else fail "Not a circle"


--"text", [ 42, -5, 0 ], { "text": "nd", "font": "4pt sans-serif" } ],
instance FromJSON Txt where
  parseJSON (Array v) = "Decode.Txt.Array" <?? do
    case (v V.!? 1, v V.!? 2) of
      (Just v1, Just v2) -> do
        c3 <- parseJSON v1
        Object o <- parseJSON v2
        txt <- o .: "text"
        font <- o .:? "font"
        return $ Txt c3 txt font
      otherwise -> fail "Decode.FromJSON Txt got unexpected array"

-- [ "terminal", [ 16, 0, 4 ], { "name": "out" } ]
instance FromJSON Terminal where
  parseJSON (Array v) = "Decode.Terminal.Array" <?? do
    case (v V.!? 0, v V.!? 1, v V.!? 2) of
      (Just v0, Just v1, Nothing) -> fail $ "Terminal is missing a name: " ++ (show v)
      (Just v0, Just v1, Just v2) -> do 
        label <- parseJSON v0
        when ((label :: String) /= "terminal") $ fail "Not a terminal"
    
        c3 <- parseJSON v1
        Object o <- parseJSON v2
    
        s <- o .: "name" -- signal name
    
        case Sig.parseSig s of
          Right sig -> return $ Terminal c3 sig 
          Left msg -> fail (show msg)
      otherwise -> fail $ "Decode.FromJSON Terminal got unexpected array" ++ (show v)

instance FromJSON IconPart where
  parseJSON arr@(Array v) = "Decode.IconPart.Array" <?? do
    kind <- parseJSON $ v V.! 0
    
    case (kind :: String) of
      "line" -> IconLine <$> parseJSON arr
      "terminal" -> IconTerm <$> parseJSON arr
      "text" -> IconTxt <$> parseJSON arr
      "box" -> IconBox <$> parseJSON arr
      "circle" -> IconCircle <$> parseJSON arr
      "property" -> return IconProperty -- todo
      "arc" -> return IconArc -- todo
      _ -> fail $ "Unknown IconPart: " ++ kind
      
instance FromJSON Icon where
  parseJSON (Array v) = "Decode.Icon" <?? do
    Icon <$> mapM parseJSON (V.toList v)
        
instance FromJSON Port where
  parseJSON (Array v) = "Decode.Port" <?? do
    c3 <- parseJSON $ v V.! 1
    if V.length v == 3
      then do sig <- parseJSON $ v V.! 2
              return $ Port c3 (Just sig)
      else return $ Port c3 Nothing

instance FromJSON SubModule where
  parseJSON (Array v) = "Decode.SubModule" <?? do
    name <- parseJSON $ v V.! 0
    sub <- parseJSON $ v V.! 1
    return $ SubModule name sub

instance FromJSON Jumper where
  parseJSON (Array v) = "Decode.Jumper" <?? do
    c3 <- parseJSON $ v V.! 1
    return $ Jumper c3 

instance FromJSON Part where
  parseJSON v@(Array arr) = "Decode.Part" <?? do
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
  parseJSON (Array v) = "Decode.Schematic.Array" <?? do
    cs <- mapM parseJSON v
    return $ Schematic (V.toList cs)

instance FromJSON Module where
  parseJSON (Object o) = "Decode.Module.Object" <?? do
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
  parseJSON (Array arr) = "Decode.TopLevel:Array" <?? do
    when (V.length arr < 2) (fail $ "Decode.TopLevel.parseJson fails because array not long enough: " ++ show arr)
    mods <- parseJSON $ arr V.! 1
    return $ TopLevel mods

  parseJSON (Object o) = "Decode.TopLevel:Object" <?? do
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
        Right (TopLevel mods) -> do
          return $ Right $ TopLevel (DM.union gates mods)
        Left msg -> fail ("Decode.decodeTopLevel: " ++ msg)
    Left msg -> fail $ msg ++ "\nDecode.decodeTopLevel fails to decode 'app-data/gates.json'"

