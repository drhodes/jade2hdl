{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Jade.Types where

import Data.Vector.Instances
import GHC.Generics
import Control.Monad
import Data.Traversable
import Data.ByteString.Lazy.Internal
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified System.Environment as SE
import Data.Hashable
import Test.QuickCheck
--import Data.Functor.Identity
import Control.Monad.Except as E


------------------------------------------------------------------
type J a = Except String a      

runJ = runExcept
printJ x = case runJ x of
             Left msg -> putStrLn msg
             Right val -> putStrLn $ show val
             
die msg = E.throwError ("! Oops" ++ "\n" ++ "! " ++ msg)

(?) x msg = x `catchError` (\e -> (throwError $ e ++ "\n" ++ "! " ++ msg))

------------------------------------------------------------------
-- Icon Types
data Line = Line Coord5 deriving (Generic, Show, Eq, Hashable, Ord)
data Terminal = Terminal Coord3 Sig deriving (Generic, Show, Eq, Hashable, Ord)
data Box = Box Coord5 deriving (Generic, Show, Eq, Hashable, Ord)
data Txt = Txt { txtLoc :: Coord3
               , txtText :: String
               , txtFont :: Maybe String
               }  deriving (Generic, Show, Eq, Hashable, Ord)

data IconPart = IconLine Line
              | IconTerm Terminal
              | IconBox Box
              | IconTxt Txt
                deriving (Generic, Show, Eq, Hashable, Ord)

data Icon = Icon { iconParts :: [IconPart]
                   -- ^ A flat list of icon elements found in the icon view
                 } deriving (Generic, Show, Eq, Hashable, Ord)

------------------------------------------------------------------
-- Schematic Types
type ModuleName = String


data Direction = In | Out | InOut
               deriving (Generic, Show, Eq, Hashable, Ord)

data SigNum = Bin String
            | Num Integer
            deriving (Generic, Show, Eq, Hashable, Ord)

data Sig = SigSimple String
         | SigIndex String Integer
         | SigHash String Integer
         | SigRange String Integer Integer
         | SigRangeStep String Integer Integer Integer
         | SigQuote Integer Integer
         deriving (Show, Eq, Generic, Hashable, Ord)

data Signal = Signal { signalName :: Maybe Sig
                     , signalWidth :: Maybe String
                     , signalDirection :: Maybe Direction
                     } deriving (Show, Eq, Generic, Hashable, Ord)

data Rot = Rot0 
         | Rot270
         | Rot180
         | Rot90
         | FlipX
         | TransposeNeg
         | FlipY
         | TransposePos
           deriving (Show, Enum, Eq, Generic, Hashable, Ord)

data Coord3 = Coord3 { c3x :: Integer
                     , c3y :: Integer
                     , c3r :: Integer
                     } deriving (Eq, Generic, Hashable, Ord)

instance Show Coord3 where
  show (Coord3 x y r) = concat [ "<C3 ", show x
                               , ", ", show y
                               , ", ", show r, ">"]

data Wire = Wire { wireCoord5 :: Coord5
                 , wireSignal :: Maybe Signal
                 } deriving (Show, Eq, Generic, Hashable, Ord)

data Coord5 = Coord5 { c5x :: Integer
                     , c5y :: Integer
                     , c5r :: Rot
                     , c5dx :: Integer
                     , c5dy :: Integer
                     } deriving (Eq, Generic, Hashable, Ord)

instance Show Coord5 where
  show (Coord5 x y r dx dy) = concat [ "<C5 ", show x
                                     , ", ", show y
                                     , ", ", show r
                                     , ", ", show dx
                                     , ", ", show dy
                                     , ">"]
  

data Port = Port { portCoord3 :: Coord3                 
                 , portSignal :: Maybe Signal
                 } deriving (Generic, Show, Eq, Hashable, Ord)
      
data SubModule = SubModule { subName :: String
                           , subCoord3 :: Coord3
                           } deriving (Generic, Show, Eq, Hashable, Ord)

data Jumper = Jumper Coord3 deriving (Generic, Show, Eq, Hashable, Ord)

data Part = PortC Port
          | SubModuleC SubModule
          | WireC Wire
          | JumperC Jumper
          | TermC Terminal
          | Nop
          deriving (Generic, Show, Eq, Hashable, Ord)






type Test = String

data Schematic = Schematic (V.Vector Part) deriving (Generic, Show, Eq, Ord)

instance Hashable Schematic where
  hash (Schematic v) = 123 --hash $ V.toList v

data Module = Module { moduleSchem :: Maybe Schematic
                     , moduleTest :: Maybe ModTest
                     , moduleIcon :: Maybe Icon
                     } deriving (Generic, Show, Eq, Hashable, Ord) -- todo add test

data TopLevel = TopLevel (DM.Map String Module)
              deriving  (Show, Eq)


------------------------------------------------------------------
-- Test Types

data Power = Power { powerVdd :: Double } deriving (Generic, Show, Eq, Hashable, Ord)
  
data Thresholds = Thresholds { thVol :: Double
                             , thVil :: Double
                             , thVih :: Double
                             , thVoh :: Double
                             } deriving (Generic, Show, Eq, Hashable, Ord)

data Inputs = Inputs [Sig] deriving (Generic, Show, Eq, Hashable, Ord)
data Outputs = Outputs [Sig] deriving (Generic, Show, Eq, Hashable, Ord)

data Mode = Device | Gate deriving (Generic, Show, Eq, Hashable, Ord)

data Duration = Nanosecond Double
              | Millisecond Double
                deriving (Generic, Show, Eq, Hashable, Ord)

data Action = Assert String
            | Deassert String
            | Sample String
            | Tran Duration
            | SetSignal Sig Double
              deriving (Generic, Show, Eq, Hashable, Ord)

data CycleLine = CycleLine [Action] deriving (Generic, Show, Eq, Hashable, Ord)

data BinVal = L | H | Z deriving (Generic, Show, Eq, Hashable, Ord)

data TestLine = TestLine { testLineAsserts :: [BinVal]                         
                         , testLineSamples :: [BinVal]
                         , testLineComment :: Maybe String
                         } deriving (Generic, Show, Eq, Hashable, Ord)

data PlotDef = PlotDef Sig [String] deriving (Generic, Show, Eq, Hashable, Ord)

data PlotStyle = BinStyle Sig
               | HexStyle Sig
               | DecStyle Sig
               | SimplePlot Sig
               | PlotDefStyle String Sig
                 deriving (Generic, Show, Eq, Hashable, Ord)

data ModTest = ModTest { modPower :: Maybe Power
                       , modThresholds :: Maybe Thresholds
                       , modInputs :: Maybe Inputs
                       , modOutputs :: Maybe Outputs
                       , modMode :: Maybe Mode
                       , modCycleLine :: Maybe CycleLine
                       , modTestLines :: [TestLine]
                       , modPlotDef :: [PlotDef]
                       , modPlotStyles :: [PlotStyle]
                       } deriving (Generic, Show, Eq, Hashable, Ord)


------------------------------------------------------------------

data Node a = Node { nodeElement :: a
                   , nodePart :: Part
                   } deriving (Eq, Generic, Show, Hashable, Ord)

-- type GComp = (DS.Set (Node (Integer, Integer)))

-- data Graph a = Graph (DM.Map (Node a) (DS.Set (Node a)))
--              deriving (Generic, Show)

data Edge a = Edge (Node a) (Node a)
            deriving (Generic, Show, Hashable, Ord, Eq)


data QuickUnionUF a = QuickUnionUF { ids :: V.Vector Int
                                   , store :: DM.Map a Int
                                   , curId :: Int
                                   } deriving (Show)


------------------------------------------------------------------

data AssnI = AssnI { assniSrc :: Sig
                   , assniTgt :: Sig
                   } deriving (Show, Eq)

data ModuleI = ModuleI { modiName :: String
                       , modiWireAssn :: [AssnI]
                         -- component decls
                         -- component instantiations.
                       } deriving (Show, Eq)
