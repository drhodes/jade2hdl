{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

module Jade.Decode.Types where

import GHC.Generics hiding (from, to)
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBL8 
import Jade.Decode.Util
import Data.Hashable
import Text.Printf
import Data.Aeson
import Control.Monad
import Data.Maybe

------------------------------------------------------------------
-- Icon Types
newtype Line = Line Coord5 deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Terminal = Terminal Coord3 Sig deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

newtype Box = Box Coord5 deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Txt = Txt { txtLoc :: Coord3
               , txtText :: String
               , txtFont :: Maybe String
               }  deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Circle = Circle { circleX :: Integer
                     , circleY :: Integer
                     , circleR :: Integer
                     } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data IconPart = IconLine Line
              | IconTerm Terminal
              | IconBox Box
              | IconTxt Txt
              | IconCircle Circle
              | IconProperty
              | IconArc
                deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

type ModuleName = String

data Icon = Icon { iconParts :: [IconPart]
                   -- ^ A flat list of icon elements found in the icon view
                 } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data ModPath = ModPath { modPath :: FilePath
                       , modFile :: String
                       } deriving (Generic, Show, Eq, ToJSON)

newtype Vdd = Vdd Coord3 deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Direction = In | Out | InOut
               deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Sig = SigSimple String
         | SigIndex String Integer
         | SigHash String Integer
         | SigRange String Integer Integer
         | SigRangeStep String Integer Integer Integer
         | SigQuote Integer Integer
         | SigConcat [Sig]
         deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

type Index = Integer

--newtype SignalName = SignalName String deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

data Signal = Signal { signalName :: Maybe Sig
                     , signalWidth :: Int
                     , signalDirection :: Maybe Direction
                     } deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

data Rot = Rot0 
         | Rot270
         | Rot180
         | Rot90
         | FlipX
         | TransposeNeg
         | FlipY
         | TransposePos
           deriving (Show, Enum, Eq, Generic, Hashable, Ord, ToJSON)
  
data Coord3 = Coord3 { c3x :: Integer
                     , c3y :: Integer
                     , c3r :: Rot
                     } deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

data Point = Point Integer Integer deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

data Wire = Wire { wireCoord5 :: Coord5
                 , wireSignal :: Maybe Signal
                 } deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

data Coord5 = Coord5 { c5x :: Integer
                     , c5y :: Integer
                     , c5r :: Rot
                     , c5dx :: Integer
                     , c5dy :: Integer
                     } deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

data Port = Port { portCoord3 :: Coord3                 
                 , portSignal :: Maybe Signal
                 } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data SubModule = SubModule { subName :: String
                           , subCoord3 :: Coord3 } 
               | SubMemUnit MemUnit
               deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)


data Jumper = Jumper Coord3 deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data MemUnit = MemUnit { memName :: String
                       , memCoord3 :: Coord3
                       , memContents :: String
                       , memNumPorts :: Integer
                         -- ^ number of memory ports in this unit.
                       , memNumAddr :: Integer
                         -- ^ width of the address terminal
                       , memNumData :: Integer 
                         -- ^ width of the output data terminal
                       } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

type PartId = Integer

data Part = PortC Port
          | SubModuleC SubModule
          | WireC Wire
          | JumperC Jumper
          | TermC Terminal 
          | UnusedPart
  deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

type Test = String

data Schematic = Schematic [Part] deriving (Generic, Show, Eq, Ord, ToJSON)

instance Hashable Schematic where
  hash (Schematic v) = hash v

data Module = Module { moduleName :: String
                     , moduleSchem :: Maybe Schematic
                     , moduleTest :: Maybe ModTest
                     , moduleIcon :: Maybe Icon
                     } 
            | BuiltInModule String
            deriving (Generic, Show, Eq, Hashable, Ord, ToJSON) -- todo add test

newtype TopLevel = TopLevel (DM.Map String Module) deriving  (Generic, Show, Eq, ToJSON)

data BoundingBox = BB { bbLeft :: Integer
                      , bbTop :: Integer
                      , bbRight :: Integer
                      , bbBottom :: Integer } deriving (Generic, Show, Eq, ToJSON)

class LocRot a where
  locrot :: a -> Coord3

instance LocRot Coord3 where locrot x = x
instance LocRot Coord5 where locrot (Coord5 x y r _ _) = Coord3 x y r

------------------------------------------------------------------
-- Test Types

newtype Power = Power { powerVdd :: Double } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Thresholds = Thresholds { thVol :: Double
                             , thVil :: Double
                             , thVih :: Double
                             , thVoh :: Double
                             } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

newtype Inputs = Inputs [Sig] deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)
  
newtype Outputs = Outputs [Sig] deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Mode = Device | Gate deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Duration = Nanosecond Double
              | Millisecond Double
                deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Action = Assert String
            | Deassert String
            | Sample String
            | Tran Duration
            | SetSignal Sig Double
              deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

newtype CycleLine = CycleLine [Action] deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data BinVal = L | H | Z deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data TestLine = TestLine { testLineBinVals :: [BinVal] 
                         , testLineComment :: Maybe String
                         } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data PlotDef = PlotDef Sig [String] deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data PlotStyle = BinStyle Sig
               | HexStyle Sig
               | DecStyle Sig
               | SimplePlot Sig
               | PlotDefStyle String Sig
                 deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data ModTest = ModTest { modPower :: Maybe Power
                       , modThresholds :: Maybe Thresholds
                       , modInputs :: Maybe Inputs
                       , modOutputs :: Maybe Outputs
                       , modMode :: Maybe Mode
                       , modCycleLine :: Maybe CycleLine
                       , modTestLines :: [TestLine]
                       , modPlotDef :: [PlotDef]
                       , modPlotStyles :: [PlotStyle]
                       } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)
