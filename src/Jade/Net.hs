{-# LANGUAGE FlexibleContexts #-}
module Jade.Net where

import qualified Data.List as DL
import Jade.Types
import qualified Jade.Part as Part
import qualified Jade.Signal as Signal
import qualified Jade.Sig as Sig
import qualified Jade.Wire as Wire
import Data.Maybe
import Control.Monad
import Jade.Util

hasSig :: Net -> Sig -> Bool
hasSig net sig  = sig `elem` (getSigs net)

hasAnyTerm :: Net -> Bool
hasAnyTerm (Net gid nodes) = or [True | Node _ (TermC _) <- nodes]

getTerminals (Net gid nodes) = [t | Node _ (TermC t) <- nodes]

hasTerm :: Net -> Terminal -> Bool
hasTerm (Net _ nodes) term1 = or [term1 == term2 | Node _ (TermC term2) <- nodes]

getSigs :: Net -> [Sig]
getSigs (Net _ nodes) =
  let parts = map nodePart nodes
  in DL.nub $ [s | (Just s) <- map Part.sig parts]

getQuotedSigs net = [ s | s@(SigQuote _ _) <- getSigs net]

getSigsWithIdent :: Net -> String -> J [Sig]
getSigsWithIdent net ident = "Net.getSigsWithIdent" <? do
  -- find the signals in net that shares the ident. There might be
  -- more than one!
  flattened <- concatMapM Sig.flatten $ getSigs (removeTerms net)
  filterM (flip Sig.hasIdent ident) flattened

getSigsWithIdentNoFlatten :: Net -> String -> J [Sig]
getSigsWithIdentNoFlatten net ident = "Net.getSigsWithIdent" <? do
  -- find the signals in net that shares the ident. There might be
  -- more than one!
  filterM (flip Sig.hasIdent ident) $ getSigs (removeTerms net)

getWires :: Net -> [Wire]
getWires (Net _ nodes) = [w | (Node _ (WireC w)) <- nodes]

removeTerms (Net gid nodes) = Net gid [n | n@(Node _ part) <- nodes, not $ Part.isTerm part]

width net = "Net.width" <? do
  -- first check the wire signals, maybe the wires have user specified width
  let ws = getWires net
  case [w | Just w <- map Wire.width ws] of    
      [] -> let sigs = getSigs net
            in if length sigs == 0
               then return 1
               else return $ maximum $ map Sig.width sigs
      -- at least one wire width was specified by the user, so use that.
      widths -> let w = maximum widths in return $ if w < 1 then 1 else w
           

parts net = let (Net _ nodes) = removeTerms net
              in map nodePart nodes 

name :: Net -> J String
name net@(Net gid nodes) = "Net.name" <? do
  let genNameLen = 10
  return $ take genNameLen $ "net_" ++ show gid

containsSigIdent :: Net -> String -> J Bool
containsSigIdent net sigIdent = "Net.containsSigIdent" <? do
  -- does this net contain a sig with name sigName?
  or `liftM` mapM (flip Part.containsIdentifier sigIdent) (parts net)
