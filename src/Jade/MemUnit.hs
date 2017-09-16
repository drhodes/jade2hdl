{-# LANGUAGE FlexibleContexts #-}
module Jade.MemUnit where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import qualified Data.Vector as DV
import qualified Data.Maybe as Maybe
import qualified Jade.Decode as D
import qualified Jade.Sig as Sig
import qualified Jade.Coord as Coord
import qualified Jade.Bundle as Bundle
import Jade.Common
import Jade.Util
import Control.Monad
import Control.Monad.State
import qualified Web.Hashids as WH
import Text.Format

-- Lesson 101 - Example 68: A VHDL ROM
-- https://www.youtube.com/watch?v=hQm8FO-SrCc

{- how to make the machine understand the protocols of 2 or 3 port
 memory.  Can idris know about this, can the protocol be described in
 the type system? -}
  
-- | Get a list of input and output terminals in a memunit submodule

terminals :: MemUnit -> J [Terminal]
terminals mem@(MemUnit name loc contents nports naddr ndata) = "MemUnit.terminals" <? do
  concatMapM (buildPort mem) [1 .. nports]

getInputTerminals memunit = terminals memunit >>= filterM isInputTerm
getOutputTerminals memunit = terminals memunit >>= filterM isOutputTerm

isOutputTerm :: Terminal -> J Bool
isOutputTerm (Terminal _ s) = do
  let names = concat $ Bundle.getNames s
  return $ DL.isInfixOf "DATA_PORT" names 

isInputTerm t = not <$> isOutputTerm t

buildPort mem@(MemUnit name loc _ nports naddr ndata) portno = "MemUnit.buildPort" <? do
  let sigport name = format "{0}_PORT{1}" [name, show portno]
      simple name = SigSimple (sigport name)
  

      
      offsetY y = y + (portno - 1) * 40

      Coord3 x y r = loc
      withOffset x' y' = Coord.rotate (Coord3 (x+x') (offsetY(y+y')) Rot0) r (x+0) (y+0)

  addrSig <- Sig.explode $ if naddr == 1
                           then simple "ADDR"
                           else SigRange (sigport "ADDR") (naddr - 1) 0
  dataSig <- Sig.explode $ if naddr == 1
                           then simple "DATA"
                           else SigRange (sigport "DATA") (naddr - 1) 0

      
  nb $ format "memunit location {0}, {1}" [show x, show y]
  
  let terms =  [ Terminal (withOffset 0 0)  addrSig
               , Terminal (withOffset 72 0)  dataSig
               , Terminal (withOffset 0 8) (Bundle [ValIndex "OE" 0])
               , Terminal (withOffset 0 16) (Bundle [ValIndex "WE" 0])
               , Terminal (withOffset 0 24) (Bundle [ValIndex "CLK" 0])
               ]

  list terms
  return terms


