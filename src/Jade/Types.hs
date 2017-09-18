{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

module Jade.Types where

import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBL8 
import Jade.Util
import Data.Hashable
import Text.Format
import Data.Aeson
import Jade.Note

------------------------------------------------------------------
-- Icon Types
data Line = Line Coord5 deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Terminal = Terminal Coord3 ValBundle deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Box = Box Coord5 deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

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

data Icon = Icon { iconParts :: [IconPart]
                   -- ^ A flat list of icon elements found in the icon view
                 } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data ModPath = ModPath { modPath :: FilePath
                       , modFile :: String
                       } deriving (Generic, Show, Eq, ToJSON)

data Vdd = Vdd Coord3 deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)


------------------------------------------------------------------
-- Schematic Types
type ModuleName = String

data Direction = In | Out | InOut
               deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data SigNum = Bin String
            | Num Integer
            deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data SigType = OneSig Sig
             | ManySig [Sig]
             deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

newtype Bundle a = Bundle [a] deriving (Show, Eq, Generic, Hashable, Ord, Foldable, ToJSON)

instance Functor Bundle where
  fmap f (Bundle xs) = Bundle (map f xs) 
instance Applicative Bundle where
  pure x = Bundle [x]
  (<*>) (Bundle fs) (Bundle xs) = Bundle (fs <*> xs)
instance Monad Bundle where
  (>>=) (Bundle xs) f = Bundle $ concat [ys | Bundle ys <- map f xs]
instance Monoid (Bundle a) where
  mconcat bs = Bundle $ concat [x | Bundle x <- bs]
  mappend (Bundle x) (Bundle y) = Bundle (x ++ y)
  mempty = Bundle []
  

data Sig = SigSimple String
         | SigIndex String Integer
         | SigHash String Integer
         | SigRange String Integer Integer
         | SigRangeStep String Integer Integer Integer
         | SigQuote Integer Integer
         deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)


-- TODO consider this phantom type.
-- data Val a = ValIndex String Integer
data Val = ValIndex { valIdxName :: String
                    , valIdxIdx :: Integer
                    } 
         | Lit { litBinVal :: BinVal }
         deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

type ValBundle = Bundle Val

data Signal = Signal { signalName :: Maybe ValBundle
                     , signalWidth :: Maybe Int
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

data Wire = Wire { wireCoord5 :: Coord5
                 , wireSignal :: Maybe Signal
                 } deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

data Coord5 = Coord5 { c5x :: Integer
                     , c5y :: Integer
                     , c5r :: Rot
                     , c5dx :: Integer
                     , c5dy :: Integer
                     } deriving (Eq, Generic, Hashable, Ord, ToJSON)

instance Show Coord5 where
  show (Coord5 x y r dx dy) = fmt "<C5 {0}, {1}, {2}, {3}, {4}>" (x, y, r, dx, dy)

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

data TopLevel = TopLevel (DM.Map String Module)
              deriving  (Generic, Show, Eq, ToJSON)

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

data Power = Power { powerVdd :: Double } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)
  
data Thresholds = Thresholds { thVol :: Double
                             , thVil :: Double
                             , thVih :: Double
                             , thVoh :: Double
                             } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Inputs = Inputs [ValBundle] deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)
data Outputs = Outputs [ValBundle] deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Mode = Device | Gate deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Duration = Nanosecond Double
              | Millisecond Double
                deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data Action = Assert String
            | Deassert String
            | Sample String
            | Tran Duration
            | SetSignal ValBundle Double
              deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data CycleLine = CycleLine [Action] deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data BinVal = L | H | Z deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data TestLine = TestLine { testLineBinVals :: [BinVal]                         
                         , testLineComment :: Maybe String
                         } deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data PlotDef = PlotDef ValBundle [String] deriving (Generic, Show, Eq, Hashable, Ord, ToJSON)

data PlotStyle = BinStyle ValBundle
               | HexStyle ValBundle
               | DecStyle ValBundle
               | SimplePlot ValBundle
               | PlotDefStyle String ValBundle
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

------------------------------------------------------------------
data Node = Node { nodeLocation :: (Integer, Integer)
                 , nodePart :: Part 
                 } deriving (Eq, Show, Generic, Hashable, Ord, ToJSON)

-- instance Show Node where
--   show node = DBL8.unpack $ encode node

  
type NetId = Integer

data Net = Net { netId :: NetId
               , netNodes :: [Node]
               } deriving (Generic, Show, Eq, Ord, ToJSON)

data Edge = Edge { edgeNode1 :: Node
                 , edgeNode2 :: Node
                 } deriving (Generic, Show, Hashable, Ord, Eq, ToJSON)

data QuickUnionUF a = QuickUnionUF { ids :: V.Vector Int
                                   , store :: DM.Map a Int
                                   , curId :: Int
                                   } deriving (Show)

