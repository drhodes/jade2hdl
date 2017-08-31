{-# LANGUAGE FlexibleContexts #-}
module Jade.MemUnit where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import qualified Data.Vector as DV
import qualified Jade.UnionFindST as UF
import qualified Jade.Wire as Wire
import qualified Data.Maybe as Maybe
import qualified Jade.Decode as D
import qualified Jade.Module as Module
import qualified Jade.Part as Part
import qualified Jade.Sig as Sig
import qualified Jade.GComp as GComp
import qualified Jade.Schematic as Schem
import qualified Jade.Jumper as Jumper
import qualified Jade.Coord as Coord
import Jade.Types
import Jade.Util
import Control.Monad
import Control.Monad.State
import qualified Web.Hashids as WH


-- | Get a list of input and output terminals in a memunit submodule
terminals (MemUnit name loc contents nports naddr ndata) = "MemUnit.terminals" <? do
  concatMapM (buildPort naddr ndata) [1 .. nports]

buildPort naddr ndata portno = "MemUnit.buildPort" <? do
  let sigport name = name ++ "_PORT" ++ show portno
      simple name = SigSimple (sigport name)
  
      addrSig = if naddr == 1
                then simple "ADDR"
                else SigRange (sigport "ADDR") (naddr - 1) 0

      dataSig = if naddr == 1
                then simple "DATA"
                else SigRange (sigport "DATA") (naddr - 1) 0
      
      offsetY x = x + (portno - 1) * 40
      
  return [ Terminal (Coord3 0 (offsetY 0) Rot0 )  addrSig
         , Terminal (Coord3 72 (offsetY 0) Rot0)  dataSig
         , Terminal (Coord3 0 (offsetY 8) Rot0) (simple "OE")
         , Terminal (Coord3 0 (offsetY 16) Rot0) (simple "WE")
         , Terminal (Coord3 0 (offsetY 24) Rot0) (simple "CLK")
         ]
