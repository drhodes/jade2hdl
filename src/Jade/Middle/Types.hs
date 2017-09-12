{-# LANGUAGE FlexibleContexts #-}
module Jade.Middle.Types where

import Control.Monad
import Jade.Types
import Jade.Util
import Text.Format
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as V
import qualified Jade.Decode as Decode
import qualified Jade.Net as Net
import qualified Jade.Module as Module
import qualified Jade.MemUnit as MemUnit
import qualified Jade.Part as Part
import qualified Jade.Val as Val
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Bundle as Bundle

data TermAssoc = TermAssoc { taDir :: Direction
                           , taSrc :: Val
                           , taTgt :: Val
                           } deriving (Show, Eq)

data ValAssign = ValAssign { sigAssignSrc :: Val
                           , sigAssignTgt :: Val
                           } deriving (Show, Eq)

flipDir In = Out
flipDir Out = In

flipTermAssoc (TermAssoc dir src tgt) = TermAssoc (flipDir dir) tgt src
flipTermMap xs = map flipTermAssoc xs

type TermMap = [TermAssoc]

-- replicated submodule.
data SubModuleRep = SubModuleRep { smrTermMapInput :: [TermMap]
                                   -- ^ all inputs for this slice of the 
                                 , smrTermMapOutput :: [TermMap]
                                 , smrSubModule :: SubModule
                                 , smrZIndex :: Int
                                 } deriving (Show, Eq)

data ModOutput = ModOutput TermMap deriving (Show, Eq)

flipAssign (ValAssign src tgt) = ValAssign tgt src

assignOutputBundle :: String -> ValBundle -> J [ValAssign]
assignOutputBundle modname outSig = "Middle/Types.assignOneOutput" <? do
  nb "assignOneInput is reused here, the assignments are flipped around"
  map flipAssign <$> assignInputBundle modname outSig 

-- | take .input(s) then find the nets they belong to and assign them.
assignInputBundle :: String -> ValBundle -> J [ValAssign]
assignInputBundle modname inputBundle = "Middle/Types.assignOneInputBundle" <? do
  case uniq $ Bundle.getNames inputBundle of
    [] -> dief "No input name found for this input bundle {0}" [show inputBundle]
    [inputName] -> do
      -- find nets with signame
      nets <- TopLevel.getNetsWithName modname inputName
      nbf "EE NETS: found {0}" [show $ length nets]
      nbf "NET: {0}" [show nets]
      -- for each net, assign the signame
      concatMapM (assignSig modname inputName) nets
    xs -> impossible "A Jade module input has more than one name?? Can't happen"
  
-- is this net connected to the output terminal of a submodule or
-- an input terminal?

-- | assign the valname to the net it's connected to 
assignSig :: String -> String -> Net -> J [ValAssign]
assignSig modname valname net = "Middle/Types.assignSig" <? do
  let bundles = DL.nub $ Net.getBundlesWithName net valname
  when (length bundles /= 1) $ die "why is there more than one bundle here?"
  let valBundle = head bundles
  
  netBundle <- Net.getBundle net
  return $ pickNetValsWithName valname valBundle netBundle

pickNetValsWithName valname (Bundle inVals) (Bundle netVals) =
  [ValAssign v1 v2 | (v1@(ValIndex n1 _), v2) <- zip inVals netVals, n1 == valname]


  -- workingSigs <- Net.getValsWithIdent net valname
  -- nb "FF Working Sigs"
  -- list workingSigs
  -- let idxs = downFrom $ length workingSigs - 1
  --     indexedNames = zip workingSigs idxs

  -- let matchedVals = filter (\(val, idx) -> Val.hasIdent val valname) indexedNames
  -- netName <- Net.name net

  -- let assigns = [ValAssign val (ValIndex netName (fromIntegral idx)) | (val, idx) <- matchedVals]
  -- return assigns

connectConstantNet modname net = "Middle.Types/connectConstantNet" <? do unimplemented
  
---------------------------------------------------------------------------------------------------
replicateOneTerminal :: Int -> Direction -> Terminal -> Net -> J TermMap
replicateOneTerminal numReplications dir term@(Terminal _ bndl) net =
  "Middle/Types.replicateOneTerminal" <? do
  netWidth <- fromIntegral <$> Net.width net
  netId <- Net.name net
  let termWidth = Bundle.width bndl 
  nb $ format "NetWidth: {0}, netId: {1}, termWidth: {2}, repd: {3}" [ show netWidth
                                                                     , show netId
                                                                     , show termWidth
                                                                     , show numReplications ]

    
  let Bundle termSigs = bndl
  
  let totalWidth = fromIntegral (termWidth * numReplications) :: Int
  case compare netWidth totalWidth of
    GT -> do {
      ; nb "case netWidth > totalWidth"
      ; impossible "This can't happen if the JADE modules tests pass in JADE"
      }
    _ -> do {
      ; nb "case netWidth <= totalWidth"
      ; let singles = map (ValIndex netId) $ downFrom (fromIntegral (netWidth - 1))
            srcs = concat $ DL.transpose $ chunk numReplications singles

      ; safeSrcs <- safeCycle srcs
      ; safeTerms <- safeCycle termSigs
            
      ; let tmap = take totalWidth (zipWith (TermAssoc In) safeSrcs safeTerms)
      ; return $ case dir of
                   In -> tmap
                   Out -> flipTermMap tmap
      }

oneOfEach xs =  
  if 0 `elem` (map length xs)
  then ([], [])
  else (map head xs, map tail xs)

buildSubModuleReps :: [[TermMap]] -> [[TermMap]] -> SubModule -> Int -> J [SubModuleRep]
buildSubModuleReps inputTermMaps outputTermMaps submod zidx =
  "Middle/Types.buildSubModuleRep" <? do
  if zidx < 0 then return []
    else do let itms = map head inputTermMaps
                otms = map head outputTermMaps
                smr = SubModuleRep itms otms submod zidx
            liftM (smr:) (buildSubModuleReps
                           (map tail inputTermMaps)
                           (map tail outputTermMaps) submod (zidx-1))

subModuleInstances :: String -> SubModule -> J [SubModuleRep]
subModuleInstances modname submod@(SubModule name loc) = do
  nb "Middle.Types.subModuleInstances"
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

subModuleInstances modname (SubMemUnit memunit) = do
  nb "Middle.Types.subModuleInstances"
  die "This should not be called on SubMemUnit, instead call memUnitInstance"
  

memUnitInstance :: String -> MemUnit -> J SubModuleRep
memUnitInstance modname memunit = "Middle/Types.memUnitInstance" <? do
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

  



-- sharesInputP modname net = "Middle.Types.sharesInputP" <? do
--   let ts = Net.getTerminals net
--   drivers <- mapM (TopLevel.getInputTermDriver modname) ts
--   nb "Drivers!!"
--   list drivers
  
--   if length (filterOut (==Nothing) drivers) == 0
--     then return False
--     else return True


-- sigNameDirection modname net = "Middle.Types.isSigNameDriver" <? do
--   -- does signames share a net with a terminal that belongs to a submodules Input?
--   sharesInput <- sharesInputP modname net  
--   nbf "sharesInput? {0}" [show sharesInput]
--   return $ if sharesInput
--            then In
--            else Out
                
----
{-
connectSigName modname sigName = "Middle/Types.connectSigName" <? do  
  -- find nets with signame
  nets <- TopLevel.getNetsWithName modname sigName

  nbf "connectSigName:sigName = {0}" [sigName]
  dirs <- mapM (sigNameDirection modname) nets
  -- for each net, locate the z-index of the signame  
  assigns <- concatMapM (assignSig modname sigName) nets
  
  return [case dir of
            In -> flipAssign assn
            Out -> assn
         | (assn, dir) <- zip assigns dirs]
-}
