{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Jade.Types where

import Control.Monad.Identity
import Data.Vector.Instances
import GHC.Generics
import Control.Monad
import Data.Traversable
import Data.ByteString.Lazy.Internal
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.Set as DS
import qualified System.Environment as SE
import Data.Hashable
import Test.QuickCheck
import Control.Monad.Except 
import qualified Data.Hashable as DH
import qualified Web.Hashids as WH
import qualified Data.ByteString.Char8 as B
import Jade.Util

import Control.Monad.Writer
import Control.Monad.State.Lazy
debug = True

hashid :: Hashable a => a -> String
hashid x =
  let ctx = WH.hashidsSimple "salt"
      in B.unpack $ WH.encode ctx . abs . DH.hash $ x


type J = ExceptT String (Writer [String])


runX x = 
  let result = runExceptT x
      (a, b) = runWriter result -- :: Writer String (Either String Integer)
  in (a, b)

runLog :: J a -> String
runLog x = let log = snd $ runX x
           in DL.intercalate "\n" ("Cool Story": uniq log)

runJ :: J a -> Either String a
runJ = fst . runX

printJ x = case runJ x of
             Left msg -> putStrLn msg
             Right val -> putStrLn $ show val

putStrJ x = case runJ x of
              Left msg -> putStrLn msg
              Right val -> putStr val

runJIO :: J (IO a) -> IO String
runJIO x =
  case runX x of
    (Left msg, log) -> do putStrLn msg
                          return $ DL.intercalate "\n" ("Cool Story" : uniq log)
    (Right f, log) -> do f
                         return ""

die msg = throwError ("! Oops" ++ "\n" ++ "! " ++ msg)

impossible msg = die $ "The impossible happened: " ++ msg
unimplemented s = die $ "unimplemented: " ++ s

nb :: String -> J ()
nb s = if debug == True
       then tell [s]
       else return ()

bail :: J a
bail = die "bailing!"

(?) x msg = x `catchError` (\e -> (throwError $ e ++ "\n" ++ "! " ++ msg))

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
                     } deriving (Eq, Generic, Hashable, Ord)

instance Show Coord3 where
  show (Coord3 x y r) = fmt "<C3 {0} {1} {2}>" (x, y, r)

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
                           , subCoord3 :: Coord3
                           } deriving (Generic, Show, Eq, Hashable, Ord)

data Replicated a = Rep a

data Jumper = Jumper Coord3 deriving (Generic, Show, Eq, Hashable, Ord)

data Part = PortC Port
          | SubModuleC SubModule
          | WireC Wire
          | JumperC Jumper
          | TermC Terminal
          | Nop
          deriving (Generic, Show, Eq, Hashable, Ord)



type Test = String

data Schematic = Schematic [Part] deriving (Generic, Show, Eq, Ord)

instance Hashable Schematic where
  hash (Schematic v) = hash v

data Module = Module { moduleSchem :: Maybe Schematic
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
data Node = Node { nodeLocation :: (Integer, Integer)
                 , nodePart :: Part                   
                 } deriving (Eq, Generic, Show, Hashable, Ord)

data GComp = GComp [Node]
           deriving (Show, Eq, Ord)

data ComponentType = CtNoTerms
                   | CtSubModIns
                   | CtSybModOuts



data Edge = Edge Node Node
          deriving (Generic, Show, Hashable, Ord, Eq)

data UfInstruction = LinkEdge Edge
                   | LinkNodes Node Node

data QuickUnionUF a = QuickUnionUF { ids :: V.Vector Int
                                   , store :: DM.Map a Int
                                   , curId :: Int
                                   } deriving (Show)




list x = nb $ DL.intercalate "\n" $ map show x

