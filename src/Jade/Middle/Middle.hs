{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

module Jade.Middle.Middle ( assignConstantNet
                          , subModuleInstances
                          , memUnitInstance
                          , assignBundle
                          -- , assignInternalBundle
                          , assignInternalSigFromRep
                          , getAllOutputNetIdsFromRep
                          , getAllInputNetIdsFromRep
                          
                          , module Jade.Middle.Types
                          ) where

import GHC.Generics
import Data.Aeson
import Control.Monad
import Text.Format
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as V
import qualified Jade.Decode as Decode
import qualified Jade.Net as Net
import qualified Jade.Module as Module
import qualified Jade.MemUnit as MemUnit
import qualified Jade.Val as Val
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Bundle as Bundle

import Jade.Common
import Jade.Middle.Types


  
flipAssign (ValAssign src tgt) = ValAssign tgt src


-----------------------------------------------------------------------------

                                      
-- | take .input(s) then find the nets they belong to and assign them.
assignBundle :: Direction -> String -> ValBundle -> J [ValAssign]
assignBundle dir modname inputBundle = "Middle/Middle.assignOneInputBundle" <? do
  case uniq $ Bundle.getNames inputBundle of
    [] -> dief "No input name found for this input bundle {0}" [show inputBundle]
    [inputName] -> do
      -- find nets with signame
      nets <- TopLevel.getNetsWithName modname inputName
      -- for each net, assign the signame
      concatMapM (assignSig dir modname inputName) nets
    xs -> impossible "A Jade module input has more than one name?? Can't happen"
  
-- is this net connected to the output terminal of a submodule or
-- an input terminal?

-- | assign the valname to the net it's connected to 
assignSig :: Direction -> String -> String -> Net -> J [ValAssign]
assignSig dir modname valname net = "Middle/Middle.assignSig" <? do
  let bundles = DL.nub $ Net.getBundlesWithName net valname
  when (length bundles /= 1) $ die "why is there more than one bundle here?"
  let valBundle = head bundles
  netBundle <- Net.getBundle net
  assignments <- pickNetValsWithName valname valBundle netBundle
  return $ case dir of
    In -> assignments
    Out -> map flipAssign assignments
    
pickNetValsWithName valname (Bundle inVals) (Bundle netVals) = "Middle.pickNetValsWithName" <? do
  return [ValAssign v1 v2 | (v1@(ValIndex n _), v2) <- zip inVals netVals, n == valname]



assignConstantNet :: Net -> J [ValAssign]
assignConstantNet net = "Middle.Types/connectConstantNet" <? do 
  let bundles = Net.getBundlesWithLits net
  if length bundles == 0 then
    return []
    else do when (length bundles /= 1) $ die "why is there more than one bundle here?"
            let valBundle = head bundles 
            netBundle <- Net.getBundle net
            return $ pickNetValsWithLit valBundle netBundle

pickNetValsWithLit :: Bundle Val -> Bundle Val -> [ValAssign]
pickNetValsWithLit (Bundle inVals) (Bundle netVals) =
  [ValAssign v1 v2 | (v1@(Lit n1), v2) <- zip inVals netVals, Val.isLit v1]
  
replicateOneTerminal :: Int -> Direction -> Terminal -> Net -> J TermMap
replicateOneTerminal numReplications dir term@(Terminal _ bndl) net@(Net nid _) =
  "Middle/Middle.replicateOneTerminal" <? do
  netWidth <- fromIntegral <$> Net.width net
  --netId <- Net.name net
  
  let termWidth = Bundle.width bndl 
      Bundle termSigs = bndl
      totalWidth = fromIntegral (termWidth * numReplications) :: Int
  case compare netWidth totalWidth of
    GT -> do {
      ; cnb "case netWidth > totalWidth"
      ; impossible "This can't happen if the JADE modules tests pass in JADE"
      }
    _ -> do {
      ; cnb "case netWidth <= totalWidth"
      ; let singles = map (NetIndex nid) $ downFrom (fromIntegral (netWidth - 1))
            srcs = concat $ DL.transpose $ chunk numReplications singles
      ; enb ("srcs", srcs)
      ; enb ("netwidth", netWidth)
      ; enb ("singles", singles)
      ; enb ("termSigs", termSigs)
      ; safeSrcs <- safeCycle srcs ? "safeSrcs"
      ; safeTerms <- safeCycle termSigs ? "termSigs"
      ; let tmap = take totalWidth (zipWith (TermAssoc In) safeSrcs safeTerms)
      ; return $ case dir of
                   In -> tmap
                   Out -> flipTermMap tmap
      }

buildSubModuleReps :: [[TermMap]] -> [[TermMap]] -> SubModule -> Int -> J [SubModuleRep]
buildSubModuleReps inputTermMaps outputTermMaps submod zidx =
  "Middle/Middle.buildSubModuleRep" <? do
  if zidx < 0 then return []
    else do let itms = map head inputTermMaps
                otms = map head outputTermMaps
                smr = SubModuleRep itms otms submod zidx
            liftM (smr:) (buildSubModuleReps
                           (map tail inputTermMaps)
                           (map tail outputTermMaps) submod (zidx-1))

subModuleInstances :: String -> SubModule -> J [SubModuleRep]
subModuleInstances modname submod@(SubModule name loc) = "Middle.Types.subModuleInstances" <? do
  repd <- TopLevel.replicationDepth modname submod
  m <- TopLevel.getModule name 
  
  inputTerms <- Module.getInputTerminals m loc
  inputNets <- mapM (TopLevel.netWithTerminal modname) inputTerms
 
  outputTerms <- Module.getOutputTerminals m loc
  outputNets <- mapM (TopLevel.netWithTerminal modname) outputTerms

  inputTermMaps <- zipWithM (replicateOneTerminal repd In) inputTerms (map Net.removeTerms inputNets)
  outputTermMaps <- zipWithM (replicateOneTerminal repd Out) outputTerms (map Net.removeTerms outputNets)

  let itms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- inputTermMaps] :: [[TermMap]]
      otms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- outputTermMaps] :: [[TermMap]]

  buildSubModuleReps itms otms submod (repd - 1)

subModuleInstances modname (SubMemUnit memunit) = "Middle.Types.subModuleInstances" <?
  die "This should not be called on SubMemUnit, instead call memUnitInstance"

memUnitInstance :: String -> MemUnit -> J SubModuleRep
memUnitInstance modname memunit = "Middle/Middle.memUnitInstance" <? do
  let repd = 1
  m <- TopLevel.getModule modname
  
  inputTerms <- MemUnit.getInputTerminals memunit
  inputNets <- mapM (TopLevel.netWithTerminal modname) inputTerms

  outputTerms <- MemUnit.getOutputTerminals memunit
  outputNets <- mapM (TopLevel.netWithTerminal modname) outputTerms

  inputTermMaps <- zipWithM (replicateOneTerminal repd In) inputTerms (map Net.removeTerms inputNets)
  outputTermMaps <- zipWithM (replicateOneTerminal repd Out) outputTerms (map Net.removeTerms outputNets)

  let itms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- inputTermMaps] :: [[TermMap]]
      otms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- outputTermMaps] :: [[TermMap]]
  
  rep <- buildSubModuleReps itms otms (SubMemUnit memunit) 0
  when (length rep /= 1) (impossible "This should contain one memunit representation")
  return $ head rep


-- | OK. for determining the drivers go through the outputs, and for
-- each output this is the internal name for the module, this name
-- will be targeting a (NetIndex nid idx).  Find that net, then see
-- which 


assignInternalSigFromRep :: String -> SubModuleRep -> J TermMap
assignInternalSigFromRep signame smr = "Middle.assignInternalSigFromRep" <? do
  let outputs = smrTermMapOutput smr
  enb "LOL -----------------------------------------------------------------------------"

 
  
  
  enb outputs
  return []


getAllOutputNetIdsFromRep :: SubModuleRep -> J [NetId]
getAllOutputNetIdsFromRep rep = "Middle.getOutputsNetIdsFromRep" <? do
  return $ DL.nub $ concat $ map getOutputNetIdsFromTermMap (smrTermMapOutput rep)

getOutputNetIdsFromTermMap outs = [nid | TermAssoc _ src tgt@(NetIndex nid _) <- outs]

getAllInputNetIdsFromRep :: SubModuleRep -> J [NetId]
getAllInputNetIdsFromRep rep = "Middle.getInputsNetIdsFromRep" <? do
  return $ DL.nub $ concat $ map getInputNetIdsFromTermMap (smrTermMapInput rep)

getInputNetIdsFromTermMap ins = [nid | TermAssoc _ src@(NetIndex nid _) tgt <- ins]



-- getAllOutputNetIdsFromRep :: SubModuleRep -> J [NetId]
-- getAllOutputNetIdsFromRep rep = "Middle.getOutputsNetIdsFromRep" <? do
--   return $ DL.nub $ concat $ map getOutputNetIdsFromTermMap (smrTermMapOutput rep)

-- getOutputNetIdsFromTermMap outs = [nid | TermAssoc _ src tgt@(NetIndex nid _) <- outs]






-- oneOfEach :: [[a]] -> ([a], [[a]])
-- oneOfEach xs =  
--   if 0 `elem` (map length xs)
--   then ([], [])
--   else (map head xs, map tail xs)
