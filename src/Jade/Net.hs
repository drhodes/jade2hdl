{-# LANGUAGE FlexibleContexts #-}
module Jade.Net where

import qualified Data.List as DL
import Jade.Types
import qualified Jade.Part as Part
import qualified Jade.Signal as Signal
import qualified Jade.Sig as Sig
import qualified Jade.Wire as Wire
import qualified Jade.Node as Node
import Data.Maybe
import Control.Monad
import Jade.Util

hasAnyTerm :: Net -> Bool
hasAnyTerm (Net gid nodes) = or [True | Node _ (TermC _) <- nodes]

getTerminals (Net gid nodes) = [t | Node _ (TermC t) <- nodes]

hasTerm :: Net -> Terminal -> Bool
hasTerm (Net _ nodes) term1 = or [term1 == term2 | Node _ (TermC term2) <- nodes]

getLitVals (Net gid nodes) = concat $ map Node.getLitVals nodes

getValsWithIdent :: Net -> String -> J [Val]
getValsWithIdent (Net _ nodes) ident = "Net.getSigsWithIdent" <? do
  -- the DL.nub is there because .. hackery need to happen in TopLevel
  -- to make union find connect up correctly. I don't think this going
  -- to cause any problems...
  return $ DL.nub $ concat $ map (flip Node.getValsWithIdent ident) nodes

getWires :: Net -> [Wire]
getWires (Net _ nodes) = [w | (Node _ (WireC w)) <- nodes]

hasVal :: Net -> Val -> Bool
hasVal (Net _ nodes) val = or $ map (flip Node.hasVal val) nodes

removeTerms (Net gid nodes) = Net gid (filterOut Node.isTerm nodes)

width (Net _ nodes) = "Net.width" <? do
  maximum <$> mapM Node.width nodes

parts net = let (Net _ nodes) = removeTerms net
            in map nodePart nodes 
  
name :: Net -> J String
name net@(Net gid nodes) = "Net.name" <? do
  let genNameLen = 10
  return $ take genNameLen $ "net" ++ show gid

containsIdent :: Net -> String -> J Bool
containsIdent net ident = "Net.containsSigIdent" <? do
cd   return $ or $ map (flip Part.containsIdentifier ident) (parts net)
