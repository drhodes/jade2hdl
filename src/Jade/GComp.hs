{-# LANGUAGE FlexibleContexts #-}
module Jade.GComp where

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




