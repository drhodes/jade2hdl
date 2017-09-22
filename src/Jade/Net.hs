{-# LANGUAGE FlexibleContexts #-}
module Jade.Net where

import qualified Data.List as DL
import Jade.Common
import qualified Jade.Part as Part
import qualified Jade.Node as Node
import qualified Jade.Wire as Wire
import Data.Maybe
import Control.Monad
import Jade.Util

-- | About Net.  A Net has an ID and a collection of nodes that  
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

nodes (Net _ nodes) = nodes

getBundlesWithName :: Net -> String -> [ValBundle]
getBundlesWithName net name =
  DL.nub $ catMaybes $ map (flip Node.getBundleWithName name) (nodes $ removeTerms net)

getBundlesWithLits :: Net -> [ValBundle]
getBundlesWithLits net =
  DL.nub $ catMaybes $ map Node.getBundleWithLit (nodes $ removeTerms net)


getWires :: Net -> [Wire]
getWires (Net _ nodes) = [w | (Node _ (WireC w)) <- nodes]

hasVal :: Net -> Val -> Bool
hasVal (Net _ nodes) val = or $ map (flip Node.hasVal val) nodes

removeTerms (Net gid nodes) = Net gid (filterOut Node.isTerm nodes)


width :: Net -> J Int
width (Net _ nodes) = "Net.width" <?
  do w <- maximum <$> mapM Node.width nodes
     case w of
       Just 0 -> return 1 -- todo find out why Node.width is returning 0.
       Just w -> return w
       Nothing -> die "Got nothing for width? How does that happen?"

parts net = let (Net _ nodes) = removeTerms net
            in map nodePart nodes 

getBundle :: Net -> J ValBundle 
getBundle net@(Net netid _) = "Net.getBundle" <? do
  w <- width net
  return $ Bundle $ map (NetIndex netid)  $ downFrom (fromIntegral (w-1))

containsIdent :: Net -> String -> J Bool
containsIdent net ident = "Net.containsSigIdent" <? do
  return $ or $ map (flip Part.containsIdentifier ident) (parts net)


--isDriven (Net netid nodes) = "Net.isDriven" <? do
getIndexesWithName net name = concat $ map (flip Wire.getIndexesWithName name) (getWires net)



