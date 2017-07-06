{-# LANGUAGE FlexibleContexts #-}
module Jade.GComp where

import Jade.Types
import qualified Jade.Part as Part

hasSig :: GComp -> Sig -> Bool
hasSig gcomp sig  = sig `elem` (getSigs gcomp)

hasTerm :: GComp -> Bool
hasTerm (GComp nodes) = or [True | Node _ (TermC _) <- nodes]

getSigs (GComp nodes) =
  let parts = map nodePart nodes
  in  [s | (Just s) <- map Part.sig parts]


