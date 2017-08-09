{-# LANGUAGE FlexibleContexts #-}
module Jade.GComp where

import qualified Data.List as DL
import Jade.Types
import qualified Jade.Part as Part

hasSig :: GComp -> Sig -> Bool
hasSig gcomp sig  = sig `elem` (getSigs gcomp)

hasAnyTerm :: GComp -> Bool
hasAnyTerm (GComp nodes) = or [True | Node _ (TermC _) <- nodes]

hasTerm :: GComp -> Terminal -> Bool
hasTerm (GComp nodes) term1 = or [term1 == term2 | Node _ (TermC term2) <- nodes]

getSigs (GComp nodes) =
  let parts = map nodePart nodes
  in  [s | (Just s) <- map Part.sig parts]

getWires (GComp nodes) = [w | (Node _ (WireC w)) <- nodes]

removeTerms (GComp nodes) = GComp [n | n@(Node _ part) <- nodes, not $ Part.isTerm part]

width (GComp nodes) = "GComp.width" <? do
  ws <- sequence [Part.width p | (Node _ p) <- nodes]
  return $ DL.nub ws

name :: GComp -> J String
name (GComp nodes) = "UnionFind.nameComp" <? do
  let parts = map nodePart nodes
      signals1 = [signal | WireC (Wire _ (Just signal)) <- parts]
      names = [n | Signal (Just (SigSimple n)) _ _ <- signals1] -- ++ signals2]
      genNameLen = 10
  
  return $ if length names > 0
           then head names
           else take genNameLen $ "wire_" ++ hashid parts
