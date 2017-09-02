{-# LANGUAGE FlexibleContexts #-}
module Jade.GComp where

import qualified Data.List as DL
import Jade.Types
import qualified Jade.Part as Part
import qualified Jade.Signal as Signal
import qualified Jade.Sig as Sig
import qualified Jade.Wire as Wire
import Data.Maybe
import Control.Monad
import Jade.Util

hasSig :: GComp -> Sig -> Bool
hasSig gcomp sig  = sig `elem` (getSigs gcomp)

hasAnyTerm :: GComp -> Bool
hasAnyTerm (GComp gid nodes) = or [True | Node _ (TermC _) <- nodes]

hasTerm :: GComp -> Terminal -> Bool
hasTerm (GComp _ nodes) term1 = or [term1 == term2 | Node _ (TermC term2) <- nodes]

getSigs :: GComp -> [Sig]
getSigs (GComp _ nodes) =
  let parts = map nodePart nodes
  in  DL.nub [s | (Just s) <- map Part.sig parts]

getQuotedSigs gcomp = [ s | s@(SigQuote _ _) <- getSigs gcomp]

getSigsWithIdent :: GComp -> String -> J [Sig]
getSigsWithIdent gcomp ident = do
  -- find the signals in gcomp that shares the ident. There might be
  -- more than one!
  liftM DL.nub $ filterM (flip Sig.hasIdent ident) (getSigs (removeTerms gcomp))

getWires :: GComp -> [Wire]
getWires (GComp _ nodes) = [w | (Node _ (WireC w)) <- nodes]

removeTerms (GComp gid nodes) = GComp gid [n | n@(Node _ part) <- nodes, not $ Part.isTerm part]

width gcomp = "GComp.width" <? do
  -- first check the wire signals, maybe the wires have user specified width
  let ws = getWires gcomp
  case [w | Just w <- map Wire.width ws] of    
      [] -> let sigs = getSigs gcomp
            in if length sigs == 0
               then return 1
               else return $ maximum $ map Sig.width sigs
      -- at least one wire width was specified by the user, so use that.
      widths -> return $ maximum widths

parts gcomp = let (GComp _ nodes) = removeTerms gcomp
              in map nodePart nodes 

name :: GComp -> J String
name comp@(GComp gid nodes) = "UnionFind.nameComp" <? do
  let signals1 = [signal | WireC (Wire _ (Just signal)) <- parts comp]
      genNameLen = 10
  return $ take genNameLen $ "wire_" ++ show gid

containsSigIdent :: GComp -> String -> J Bool
containsSigIdent gcomp sigIdent = "GComp.containsSigIdent" <? do
  -- does this component contain a sig with name sigName?
  or `liftM` mapM (flip Part.containsIdentifier sigIdent) (parts gcomp)
