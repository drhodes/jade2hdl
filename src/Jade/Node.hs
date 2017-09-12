{-# LANGUAGE FlexibleContexts #-}
module Jade.Node where

import qualified Data.List as DL
import Jade.Types
import qualified Jade.Part as Part
import qualified Jade.Signal as Signal
import qualified Jade.Sig as Sig
import qualified Jade.Wire as Wire
import Data.Maybe
import Control.Monad
import Jade.Util

isTerm (Node _ part) = Part.isTerm part
getBundle (Node _ part) = Part.bundle part

getValsWithIdent :: Node -> String -> [Val]
getValsWithIdent (Node _ part) ident = Part.getValsWithIdent part ident

getBundleWithName (Node _ part) ident = Part.getBundleWithIdent part ident
getBundleWithLit (Node _ part) = Part.getBundleWithLit part 

width (Node _ part) = Part.width part
hasVal (Node _ part) sig = Part.hasVal part sig

getLitVals (Node _ part) = Part.getLitVals part
