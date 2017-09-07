{-# LANGUAGE OverloadedStrings #-}
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
import qualified Jade.Sig as Sig
import qualified Jade.TopLevel as TopLevel

data TermAssoc = TermAssoc { taDir :: Direction
                           , taSrc :: Sig
                           , taTgt :: Sig
                           } deriving (Show, Eq)

data SigAssign = SigAssign { sigAssignSrc :: Sig
                           , sigAssignTgt :: Sig
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
                                 , smrZIndex :: Integer
                                 } deriving (Show, Eq)

data ModOutput = ModOutput TermMap deriving (Show, Eq)

flipAssign (SigAssign src tgt) = SigAssign tgt src

connectOneOutput modname outSig = "Middle/Types.connectOneOutput" <? do
  nb "connectOneInput is reused here, the assignments are flipped around"
  map flipAssign `liftM` connectOneInput modname outSig 

connectOneInput :: String -> Sig -> J [SigAssign]
connectOneInput modname (SigConcat sigs) = "Middle/Types.connectOneInput@(SigConcat)" <? do
  unimplemented

connectOneInput modname inSig = "Middle/Types.connectOneInput" <? do  
  inSigNames <- Sig.getNames inSig
  case inSigNames of
    [] -> die "No signames found"
    [signame] -> do
      -- find nets with signame
      nets <- TopLevel.getNetsWithName modname signame
      -- for each net, assign the signame
      concatMapM (assignSig modname signame) nets
    xs -> unimplemented


-- is this net connected to the output terminal of a submodule or
-- an input terminal?



-- | assign the signame to the net it's connected to 
assignSig modname signame net = "Middle/Types.assignSig" <? do
  workingSigs <- Net.getSigsWithIdentNoFlatten net signame
  exploded <- concatMapM Sig.explode workingSigs
  
  let idxs = downFrom $ length exploded - 1
      indexedNames = zip exploded idxs

  matchedSigs <- filterM (\(sig, idx) -> Sig.hasIdent sig signame) indexedNames
  netName <- Net.name net

  let assigns = [SigAssign sig (SigIndex netName (fromIntegral idx))
                | (sig, idx) <- matchedSigs]
  return assigns

sharesInputP modname net = "Middle.Types.sharesInputP" <? do
  let ts = Net.getTerminals net
  drivers <- mapM (TopLevel.getInputTermDriver modname) ts
  nb "Drivers!!"
  list drivers
  
  if length (filterOut (==Nothing) drivers) == 0
    then return False
    else return True


sigNameDirection modname net = "Middle.Types.isSigNameDriver" <? do
  -- does signames share a net with a terminal that belongs to a submodules Input?
  sharesInput <- sharesInputP modname net  
  nbf "sharesInput? {0}" [show sharesInput]
  return $ if sharesInput
           then In
           else Out
                
----
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

  
genbits n | n == 0 = []
          | n `mod` 2 == 0 = 0 : (genbits next)
          | otherwise = 1 : (genbits next)
  where next = n `div` 2

connectConstantNet modname net = "Middle.Types/connectConstantNet" <? do
  let quotedSigs = Net.getQuotedSigs net
  netWidth <- Net.width net
  netName <- Net.name net
  case quotedSigs of
    [SigQuote val numBits] -> do
      when (netWidth /= numBits) $ do
        let msg = "width mismatch in signals net width: {0} and constant width: {1}"
        die $ format msg [show netWidth, show numBits]

      -- this is a twos complement representation.
      let bits = reverse $ take (fromInteger numBits) $ (genbits val) ++ repeat 0
          
      nb "The targets are the net replicated"
      let tgts = map (SigIndex netName) $ reverse [0 .. netWidth - 1]
      let srcs = [SigSimple (format "'{0}'" [show b]) | b <- bits]

      return $ zipWith SigAssign srcs tgts
    [] -> return []
    xs -> die $ "unhandled case in connectConstantNet: " ++ show xs
    
---------------------------------------------------------------------------------------------------
replicateOneTerminal :: Integer -> Direction -> Terminal -> Net -> J TermMap
replicateOneTerminal numReplications dir term@(Terminal _ sig) net =
  "Middle/Types.replicateOneTerminal" <? do
  netWidth <- Net.width net
  netId <- Net.name net
  termWidth <- Part.width (TermC term)
  nb $ format "NetWidth: {0}, netId: {1}, termWidth: {2}, repd: {3}" [ show netWidth
                                                                     , show netId
                                                                     , show termWidth
                                                                     , show numReplications ]
  termSigs <- Sig.explode sig
  bailWhen (length termSigs == 0)

  
  case termWidth of
    Just termWidth ->
      let totalWidth = fromInteger $ termWidth * numReplications
          -- ^ interfunction comment.
      in if netWidth == totalWidth
            -- the net has wirewidth equal to the full
            -- replication width, so the net will need to be
            -- sliced in slices equal to the width of the terminal.
         then do nb "case netWidth == totalWidth"
                 let singles = map (SigIndex netId) $ downFrom (netWidth - 1)                     
                     srcs = concat $ DL.transpose $ chunk numReplications singles
                     tmap = take (fromInteger totalWidth) (zipWith (TermAssoc In)
                                                            (cycle srcs)
                                                            (cycle termSigs))
                 return $ case dir of
                            In -> tmap
                            Out -> flipTermMap tmap
         else case netWidth < totalWidth of                
                 -- problem in here.
                True -> do
                  nb "case netWidth < totalWidth"
                  nb $ format "netWidth: {0}, totalWidth {1}" [show netWidth, show totalWidth]
                  -- the net width is less than the total width
                  -- of the replicated submodules. The net input
                  -- signal will have to be replicated to match the
                  -- width of the replicated submodules.
                  let singles = map (SigIndex netId) $ downFrom (netWidth - 1)
                      srcs = concat $ DL.transpose $ chunk numReplications singles
                      tmap = take (fromIntegral totalWidth) (cycle $ zipWith (TermAssoc In)
                                                              (cycle srcs)
                                                              (cycle termSigs))
                  
                  return $ case dir of
                             In -> tmap
                             Out -> flipTermMap tmap
                ---                
                False -> do
                  nb "case netWidth > totalWidth"
                  impossible "This can't happen if the JADE modules tests pass in JADE"                  
    y -> do nb $ "got: " ++ show y
            die $ "Couldn't determine width of terminal."

oneOfEach xs =  
  if 0 `elem` (map length xs)
  then ([], [])
  else (map head xs, map tail xs)

buildSubModuleReps :: [[TermMap]] -> [[TermMap]] -> SubModule -> Integer -> J [SubModuleRep]
buildSubModuleReps inputTermMaps outputTermMaps submod zidx =
  "Middle/Types.buildSubModuleRep" <? do
  if zidx < 0 then return []
    else do let itms = map head inputTermMaps
                otms = map head outputTermMaps
                smr = SubModuleRep itms otms submod zidx
            liftM (smr:) (buildSubModuleReps
                           (map tail inputTermMaps)
                           (map tail outputTermMaps) submod (zidx-1))

--here.
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
  let zidx = 0
      repd = 1
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

  
