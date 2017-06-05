{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Jade.Types where

import GHC.Generics
import Control.Monad
import Data.Traversable
import Data.ByteString.Lazy.Internal
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified System.Environment as SE

------------------------------------------------------------------
-- Schematic Types

data Direction = In | Out | InOut deriving (Show, Eq)

data SigNum = Bin String
            | Num Integer

data Sig = SigSimple String
         | SigIndex String Integer
         | SigHash String Integer
         | SigRange String Integer Integer
         | SigRangeStep String Integer Integer Integer
         | SigQuote Integer Integer
         deriving (Show, Eq)

data Signal = Signal { signalName :: Maybe Sig
                     , signalWidth :: Maybe String
                     , signalDirection :: Maybe Direction
                     } deriving (Show, Eq)

data Rot = Rot0
         | Rot270
         | Rot180
         | Rot90
         | FlipX
         | TransposeNeg
         | FlipY
         | TransposePos
           deriving (Show, Enum, Eq)

data Coord3 = Coord3 { c3x :: Integer
                     , c3y :: Integer
                     , c3r :: Integer
                     } deriving (Show, Eq)

data Wire = Wire { wireCoord5 :: Coord5
                 , wireSignal :: Maybe Signal
                 } deriving (Show, Eq)

data Coord5 = Coord5 { c5x :: Integer
                     , c5y :: Integer
                     , c5r :: Rot
                     , c5dx :: Integer
                     , c5dy :: Integer
                     } deriving (Show, Eq)

data Port = Port { portCoord3 :: Coord3                 
                 , portSignal :: Maybe Signal
                 } deriving (Show, Eq)
      
data SubModule = SubModule { subName :: String
                           , subCoord3 :: Coord3
                           } deriving (Show, Eq)

data Jumper = Jumper Coord3 deriving (Show, Eq)

data Component = PortC Port
               | SubModuleC SubModule
               | WireC Wire
               | JumperC Jumper
               | Nop
                 deriving (Show, Eq)

type Test = String

data Schematic = Schematic (V.Vector Component) deriving (Show, Eq)

data Module = Module Schematic ModTest deriving (Show, Eq) -- todo add test

data TopLevel = TopLevel (DM.Map String (Maybe Module))
              deriving  (Show, Eq)


------------------------------------------------------------------
-- Test Types

data Power = Power { powerVdd :: Double } deriving (Show, Eq)
  
data Thresholds = Thresholds { thVol :: Double
                             , thVil :: Double
                             , thVih :: Double
                             , thVoh :: Double
                             } deriving (Show, Eq)

data Inputs = Inputs [Sig] deriving (Show, Eq)
data Outputs = Outputs [Sig] deriving (Show, Eq)

data Mode = Device | Gate deriving (Show, Eq)

data Duration = Nanosecond Double
              | Millisecond Double
                deriving (Show, Eq)

data Action = Assert String
            | Deassert String
            | Sample String
            | Tran Duration
            | SetSignal Sig Double
              deriving (Show, Eq)

data CycleLine = CycleLine [Action] deriving (Show, Eq)

data BinVal = L | H | Z deriving (Show, Eq)

data TestLine = TestLine { testLineAsserts :: [BinVal]                         
                         , testLineSamples :: [BinVal]
                         , testLineComment :: Maybe String
                         } deriving (Show, Eq)

data PlotDef = PlotDef Sig [String] deriving (Show, Eq)

data PlotStyle = BinStyle Sig
               | HexStyle Sig
               | DecStyle Sig
               | SimplePlot Sig
               | PlotDefStyle String Sig
                 deriving (Show, Eq)

data ModTest = ModTest { modPower :: Maybe Power
                       , modThresholds :: Maybe Thresholds
                       , modInputs :: Maybe Inputs
                       , modOutputs :: Maybe Outputs
                       , modMode :: Maybe Mode
                       , modCycleLine :: Maybe CycleLine
                       , modTestLines :: [TestLine]
                       , modPlotDef :: [PlotDef]
                       , modPlotStyles :: [PlotStyle]
                       } deriving (Show, Eq)
