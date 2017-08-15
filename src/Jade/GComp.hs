{-# LANGUAGE FlexibleContexts #-}
module Jade.GComp where

import qualified Data.List as DL
import Jade.Types
import qualified Jade.Part as Part
import qualified Jade.Signal as Signal
import qualified Jade.Sig as Sig
import Data.Maybe
import Control.Monad

hasSig :: GComp -> Sig -> Bool
hasSig gcomp sig  = sig `elem` (getSigs gcomp)

hasAnyTerm :: GComp -> Bool
hasAnyTerm (GComp nodes) = or [True | Node _ (TermC _) <- nodes]

hasTerm :: GComp -> Terminal -> Bool
hasTerm (GComp nodes) term1 = or [term1 == term2 | Node _ (TermC term2) <- nodes]

getSigs :: GComp -> [Sig]
getSigs (GComp nodes) =
  let parts = map nodePart nodes
  in  [s | (Just s) <- map Part.sig parts]

getSigsWithIdent :: GComp -> String -> J [Sig]
getSigsWithIdent gcomp ident = do
  -- find the signals in gcomp that shares the ident. There might be
  -- more than one!
  liftM DL.nub $ filterM (flip Sig.hasIdent ident) (getSigs gcomp)

getWires (GComp nodes) = [w | (Node _ (WireC w)) <- nodes]

removeTerms (GComp nodes) = GComp [n | n@(Node _ part) <- nodes, not $ Part.isTerm part]

width (GComp nodes) = "GComp.width" <? do
  ws <- sequence [Part.width p | (Node _ p) <- nodes]
  return $ DL.nub ws

parts (GComp nodes) = map nodePart nodes 

name :: GComp -> J String
name comp = "UnionFind.nameComp" <? do
  let signals1 = [signal | WireC (Wire _ (Just signal)) <- parts comp]
      genNameLen = 10
  Just names <- liftM sequence $ liftM (filter isJust) $ mapM Signal.getName signals1
  
  return $ take genNameLen $ "wire_" ++ hashid (parts comp)

containsSigIdent :: GComp -> String -> J Bool
containsSigIdent gcomp sigIdent = "GComp.containsSigIdent" <? do
  -- does this component contain sig with name sigName?
  nb $ show ("HEY", parts gcomp)
  or `liftM` mapM (flip Part.containsIdentifier sigIdent) (parts (removeTerms gcomp))
  
