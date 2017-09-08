{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Jade.Types where

import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.ByteString as DB
import Jade.Util
import Data.Hashable
import Text.Format

import Control.Monad.Except 
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

--         Exception handling.
--         |               Global state for memoization.
--         |               |      State type, global
--         |               |      |      Log handling.
--         |               |      |      |                  return val.
--         |               |      |      |                  |
type J a = ExceptT String (StateT Global (Writer [String])) a 

data Global = Global { globalTopLevel :: TopLevel
                     , globalMemo :: Memo
                     }

data Memo = Memo { memoComps :: DM.Map String [Net] }

emptyMemo = Memo DM.empty


getMemo :: J Memo
getMemo = globalMemo <$> get

putMemo memo = do
  Global x _ <- get
  put $ Global x memo

getTop :: J TopLevel
getTop = globalTopLevel <$> get

globalInit topl = Global topl emptyMemo

runX :: TopLevel -> J a -> (Either String a, [String])
runX topl x = let stateV = runExceptT x
                  writerV = evalStateT stateV (globalInit topl)
              in runWriter writerV

runLog :: TopLevel -> J a -> String
runLog topl x = let log = snd $ runX topl x
                in DL.intercalate "\n" ("Cool Story": uniq log)

runJ topl x = fst (runX topl x)

printJ topl x = case runJ topl x of
                  Left msg -> putStrLn msg
                  Right val -> print val

putStrJ topl x = case runJ topl x of
                   Left msg -> putStrLn msg
                   Right val -> putStr val

runJIO :: TopLevel -> J (IO a) -> IO String
runJIO topl x =
  case runX topl x of
    (Left msg, log) -> return $ DL.intercalate "\n" ("Cool Story" : uniq log ++ [msg])
    (Right f, log) -> f >> return (DL.intercalate "\n" ("Cool Story" : uniq log))

die msg = throwError ("! Oops" ++ "\n" ++ "! " ++ msg)
dief msg xs = die (format msg xs)

impossible msg = die $ "The impossible happened: " ++ msg

unimplemented :: J a
unimplemented = die "unimplemented."

nb s = tell [s]
nbf s xs = nb $ format s xs

list x = nb $ DL.intercalate "\n" $ map show x
--lists s xs = nb s >> list xs

bail :: J a
bail = die "bailing!"
bailWhen cond = when cond bail

(?) x msg = let crash e = throwError $ e ++ "\n" ++ "! " ++ msg
            in x `catchError` crash

-- | for building nice stack traces.
(<?) msg x = nb msg >> x ? msg

------------------------------------------------------------------
-- Icon Types
data Line = Line Coord5 deriving (Generic, Show, Eq, Hashable, Ord)

data Terminal = Terminal Coord3 Sig deriving (Generic, Show, Eq, Hashable, Ord)

data Box = Box Coord5 deriving (Generic, Show, Eq, Hashable, Ord)

data Txt = Txt { txtLoc :: Coord3
               , txtText :: String
               , txtFont :: Maybe String
               }  deriving (Generic, Show, Eq, Hashable, Ord)

data Circle = Circle { circleX :: Integer
                     , circleY :: Integer
                     , circleR :: Integer
                     } deriving (Generic, Show, Eq, Hashable, Ord)

data IconPart = IconLine Line
              | IconTerm Terminal
              | IconBox Box
              | IconTxt Txt
              | IconCircle Circle
              | IconProperty
              | IconArc
                deriving (Generic, Show, Eq, Hashable, Ord)

data Icon = Icon { iconParts :: [IconPart]
                   -- ^ A flat list of icon elements found in the icon view
                 } deriving (Generic, Show, Eq, Hashable, Ord)

data ModPath = ModPath { modPath :: FilePath
                       , modFile :: String
                       } deriving (Show, Eq)

data Vdd = Vdd Coord3 deriving (Generic, Show, Eq, Hashable, Ord)

------------------------------------------------------------------
-- Schematic Types
type ModuleName = String

data Direction = In | Out | InOut
               deriving (Generic, Show, Eq, Hashable, Ord)

data SigNum = Bin String
            | Num Integer
            deriving (Generic, Show, Eq, Hashable, Ord)

data SigType = OneSig Sig
             | ManySig [Sig]
             deriving (Show, Eq, Generic, Hashable, Ord)

data Sig = SigSimple String
         | SigIndex String Integer
         | SigHash String Integer
         | SigRange String Integer Integer
         | SigRangeStep String Integer Integer Integer
         | SigQuote Integer Integer
         | SigConcat [Sig]
         deriving (Show, Eq, Generic, Hashable, Ord)

data Signal = Signal { signalName :: Maybe Sig
                     , signalWidth :: Maybe Integer
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
                     , c3r :: Rot
                     } deriving (Show, Eq, Generic, Hashable, Ord)

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
  show (Coord5 x y r dx dy) = fmt "<C5 {0}, {1}, {2}, {3}, {4}>" (x, y, r, dx, dy)

data Port = Port { portCoord3 :: Coord3                 
                 , portSignal :: Maybe Signal
                 } deriving (Generic, Show, Eq, Hashable, Ord)
      
data SubModule = SubModule { subName :: String
                           , subCoord3 :: Coord3 } 
               | SubMemUnit MemUnit
               deriving (Generic, Show, Eq, Hashable, Ord)

data Jumper = Jumper Coord3 deriving (Generic, Show, Eq, Hashable, Ord)

data MemUnit = MemUnit { memName :: String
                       , memCoord3 :: Coord3
                       , memContents :: String
                       , memNumPorts :: Integer
                         -- ^ number of memory ports in this unit.
                       , memNumAddr :: Integer
                         -- ^ width of the address terminal
                       , memNumData :: Integer 
                         -- ^ width of the output data terminal
                       } deriving (Generic, Show, Eq, Hashable, Ord)

data Part = PortC Port
          | SubModuleC SubModule
          | WireC Wire
          | JumperC Jumper
          | TermC Terminal
          | UnusedPart
          deriving (Generic, Show, Eq, Hashable, Ord)

type Test = String

data Schematic = Schematic [Part] deriving (Generic, Show, Eq, Ord)

instance Hashable Schematic where
  hash (Schematic v) = hash v

data Module = Module { moduleName :: String
                     , moduleSchem :: Maybe Schematic
                     , moduleTest :: Maybe ModTest
                     , moduleIcon :: Maybe Icon
                     }
            | BuiltInModule String
            deriving (Generic, Show, Eq, Hashable, Ord) -- todo add test

data TopLevel = TopLevel (DM.Map String Module)
              deriving  (Show, Eq)

data BoundingBox = BB { bbLeft :: Integer
                      , bbTop :: Integer
                      , bbRight :: Integer
                      , bbBottom :: Integer } deriving (Show, Eq)

class LocRot a where
  locrot :: a -> Coord3

instance LocRot Coord3 where locrot x = x
instance LocRot Coord5 where locrot (Coord5 x y r _ _) = Coord3 x y r

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

data TestLine = TestLine { testLineBinVals :: [BinVal]                         
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
data Node = Node { nodeLocation :: (Integer, Integer)
                 , nodePart :: Part                   
                 } deriving (Eq, Generic, Show, Hashable, Ord)

type NetId = Integer 
data Net = Net NetId [Node] deriving (Show, Eq, Ord)

data Edge = Edge Node Node deriving (Generic, Show, Hashable, Ord, Eq)

-- data UfInstruction = LinkEdge Edge
--                    | LinkNodes Node Node

data QuickUnionUF a = QuickUnionUF { ids :: V.Vector Int
                                   , store :: DM.Map a Int
                                   , curId :: Int
                                   } deriving (Show)


