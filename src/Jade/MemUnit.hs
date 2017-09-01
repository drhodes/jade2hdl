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
import Text.Format

-- Lesson 101 - Example 68: A VHDL ROM
-- https://www.youtube.com/watch?v=hQm8FO-SrCc

{- how to make the machine understand the protocols of 2 or 3 port
 memory.  Can idris know about this, can the protocol be described in
 the type system?


-} 
-- | Get a list of input and output terminals in a memunit submodule

terminals :: MemUnit -> J [Terminal]
terminals (MemUnit name loc contents nports naddr ndata) = "MemUnit.terminals" <? do
  concatMapM (buildPort name naddr ndata) [1 .. nports]

getInputTerminals memunit = terminals memunit >>= filterM isInputTerm
getOutputTerminals memunit = terminals memunit >>= filterM isOutputTerm

isOutputTerm :: Terminal -> J Bool
isOutputTerm (Terminal _ s) = do
  names <- concat `liftM` Sig.getNames s
  return $ DL.isInfixOf "DATA_PORT" names 

isInputTerm t = liftM not (isOutputTerm t)

buildPort unitName naddr ndata portno = "MemUnit.buildPort" <? do
  let sigport name = format "{1}_PORT{2}" [unitName, name, show portno]
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


