{-# LANGUAGE OverloadedStrings #-}
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
import qualified Jade.GComp as GComp
import qualified Jade.Module as Module
import qualified Jade.Part as Part
import qualified Jade.Sig as Sig
import qualified Jade.TopLevel as TopLevel

data TermAssoc = TermAssoc { taDir :: Direction
                           , taSrc :: Sig
                           , taTgt :: Sig
                           } deriving (Show, Eq)
flipDir In = Out
flipDir Out = In

flipTermAssoc (TermAssoc dir src tgt) = TermAssoc (flipDir dir) tgt src
flipTermMap xs = map flipTermAssoc xs

type TermMap = [TermAssoc] 

-- replicated submodule.
data SubModuleRep = SubModuleRep { smrTermMapInput :: [TermMap]
                                 , smrTermMapOutput :: [TermMap]
                                 , smrSubModule :: SubModule
                                 , smrZIndex :: Integer
                                 } deriving (Show, Eq)


data ModOutput = ModOutput TermMap deriving (Show, Eq)



--connectOneOutput :: TopLevel -> String -> Sig -> J ModOutput
connectOneOutput topl modname outSig = "Middle.Types.connectOneOutput" <? do
  -- find the components with the name from sig.
  outSigNames <- Sig.getNames outSig
  comps <- concatMapM (TopLevel.getComponentsWithName topl modname) outSigNames
  
  -- determine width of outSig, this is absolutely known and defined
  -- in the jade module, this width can be used to deduce the width of other parts.
  let outputWidth = Sig.width outSig 
  
  -- from the components that have the name from sig, figure out which
  -- slice of which components to take and stack them up
  let getSlices comp = "<inner function>/getSlice" <? do
        compName <- GComp.name comp
        compWidth <- GComp.width comp        
        -- find outSig in comp.
        matchingSigs <- concatMapM (GComp.getSigsWithIdent comp) outSigNames
        
        -- for each sig create a termmap, use the
        singles <- mapM Sig.explode matchingSigs

        let tgts = reverse $ DL.sort $ concat singles
            srcs = map (SigIndex compName) $ reverse [0 .. compWidth - 1]
        
        when (length tgts /= length srcs) $ do
          nb "The lengths of the targets and sources are not the same"
          nb "SRCS" >> list srcs
          nb "TGTS" >> list tgts
          bail
            
        return $ zipWith (TermAssoc Out) srcs tgts
        
  liftM concat $ mapM getSlices comps

connectOneInput :: TopLevel -> String -> Sig -> J [TermAssoc]
connectOneInput topl modname inSig = "connectOneInput" <? do
  -- find the components with the name from sig.
  inSigNames <- Sig.getNames inSig
  
  comps <- concat `liftM` mapM (TopLevel.getComponentsWithName topl modname) inSigNames
  
  -- determine width of inSig, this is absolutely known and defined
  -- in the jade module, this width can be used to deduce the width of other parts.
  let inputWidth = Sig.width inSig 
  
  -- from the components that have the name from sig, figure in which
  -- slice of which components to take and stack them up
  let getSlices comp = "getSlice" <? do
        compName  <- GComp.name comp
        compWidth <- GComp.width comp
        
        something <- mapM (GComp.getSigsWithIdent comp) inSigNames
        -- find inSig in comp.
        let matchingSigs = concat something
        nb $ "MATCHING SIGS"
        list matchingSigs
        -- for each sig create a termmap, use the
        singles <- mapM Sig.explode matchingSigs
        nb $ "SINGLES"
        list singles
        -- let tgts = reverse $ DL.sort $ concat singles
        --     srcs = map (SigIndex compName) $ reverse [0 .. compWidth - 1]
        -- let compareByIdx (SigIndex _ i) (SigIndex _ j) = compare j i
        --     tgts = DL.sortBy (compareByIdx) $ concat singles
        let tgts = DL.sort $ concat singles
            srcs = map (SigIndex compName) [0 .. compWidth - 1]
        nb "SRCS"
        list srcs
        nb "TGTS"
        list tgts
        when (length tgts /= length srcs) $ do
          nb "The lengths of the targets and sources are not the same"
          bail
        nb $ show ("Termmap", zipWith (TermAssoc In) srcs tgts)
        nb $ show ("INSIG", inSig)
        return $ zipWith (TermAssoc In) srcs tgts
  liftM concat $ mapM getSlices (reverse comps)


-- dork matchingSig = do
--   singles <- Sig.explode matchingSigs
--   list singles
--   return $ DL.sort $ concat singles



  
genbits n | n == 0 = []
          | n `mod` 2 == 0 = 0 : (genbits next)
          | otherwise = 1 : (genbits next)
  where next = n `div` 2


connectConstantComp topl modname comp = "connectConstantComp" <? do
  let quotedSigs = GComp.getQuotedSigs comp
  compWidth <- GComp.width comp
  -- TODO investigate moving the "removeTerms" function to GComp
  compName <- GComp.name comp
  case quotedSigs of
    [SigQuote val numBits] -> do
      when (compWidth /= numBits) $ do
        let txt = "width mismatch in signals component width: {0} and constant width: {1}"
        die $ format txt [show compWidth, show numBits]

      -- this is a twos complement representation.
      let bits = reverse $ take (fromInteger numBits) $ (genbits val) ++ repeat 0
          
      nb "The targets are the comp replicated"
      let tgts = map (SigIndex compName) $ reverse [0 .. compWidth - 1]
      let srcs = map (SigSimple . (\x -> "'" ++ show x ++ "'")) bits 

      return $ zipWith (TermAssoc Out) srcs tgts
    
    [] -> return []
    





---------------------------------------------------------------------------------------------------
replicateOneTerminal :: Integer -> Direction -> Terminal -> GComp -> J TermMap
replicateOneTerminal numReplications dir term@(Terminal _ sig) comp = "replicateOneTerminal" <? do
  compWidth <- GComp.width comp
  compId <- GComp.name comp
  termWidth <- Part.width (TermC term)
  nb $ format "CompWidth: {0}, compId: {1}, termWidth: {2}, repd: {3}" [ show compWidth
                                                                       , show compId
                                                                       , show termWidth
                                                                       , show numReplications ]
  termSigs <- Sig.explode sig
  
  case termWidth of
    Just termWidth ->
      let totalWidth = fromInteger $ termWidth * numReplications
      in if compWidth == totalWidth
            -- the component has wirewidth equal to the full
            -- replication width, so the component will need to be
            -- sliced in slices equal to the width of the terminal.
         then do nb "case compWidth == totalWidth"
                 let singles = reverse [0 .. totalWidth - 1]
                     srcs = map (SigIndex compId) singles
                     tmap = zipWith (TermAssoc In) srcs (concat $ repeat termSigs)
                 return $ case dir of
                            In  -> tmap
                            Out -> flipTermMap tmap
                     
         else case compWidth < totalWidth of
                True -> do
                  nb "case compWidth < totalWidth"
                  nb $ format "compWidth: {0}, totalWidth {1}" [show compWidth, show totalWidth]
                  -- the component width is less than the total width
                  -- of the replicated submodules. The component input
                  -- signal will have to be replicated to match the
                  -- width of the replicated submodules.
                  let srcs = map (SigIndex compId) (reverse [0 .. compWidth -1])
                      srcReplication = cycle srcs
                      tmap = take (fromIntegral totalWidth) (cycle $ zipWith (TermAssoc In)
                                                              (cycle srcs)
                                                              (cycle termSigs))
                  nb $ show ("Srcs", take 4 srcReplication)
                  nb $ show ("tmap", tmap)
                  return $ case dir of
                             In -> tmap
                             Out -> flipTermMap tmap
                ---                
                False -> do
                  nb "case compWidth > totalWidth"
                  impossible "This can't happen if the JADE modules tests pass in JADE"                  
    y -> do nb $ "got: " ++ show ( y)
            die $ "Couldn't determine width of componenent: " ++ show comp

oneOfEach xs =  
  if 0 `elem` (map length xs)
  then ([], [])
  else (map head xs, map tail xs)

buildSubModuleReps :: [[TermMap]] -> [[TermMap]] -> SubModule -> Integer -> J [SubModuleRep]
buildSubModuleReps inputTermMaps outputTermMaps submod zidx = "buildSubModuleRep" <? do
  nb $ show $ length inputTermMaps
  nb $ show $ length outputTermMaps
  if zidx < 0 then return []
    else do let itms = map head inputTermMaps
                otms = map head outputTermMaps
                smr = SubModuleRep itms otms submod zidx
  
            liftM (smr:) (buildSubModuleReps
                           (map tail inputTermMaps)
                           (map tail outputTermMaps) submod (zidx-1))


subModuleInstances :: TopLevel -> String -> SubModule -> J [SubModuleRep]
subModuleInstances topl modname submod@(SubModule name loc) = do
  nb "Middle.Types.subModuleInstances"
  repd <- TopLevel.replicationDepth topl modname submod
  m <- TopLevel.getModule topl name 
  
  inputTerms <- Module.getInputTerminals m loc
  inputComps <- mapM (TopLevel.componentWithTerminal topl modname) inputTerms
 
  outputTerms <- Module.getOutputTerminals m loc
  outputComps <- mapM (TopLevel.componentWithTerminal topl modname) outputTerms

  inputTermMaps <- zipWithM (replicateOneTerminal repd In) inputTerms (map GComp.removeTerms inputComps)
  outputTermMaps <- zipWithM (replicateOneTerminal repd Out) outputTerms (map GComp.removeTerms outputComps)

  nb "inputTermMaps"

  mapM_ (\(xs) -> list xs >> nb "----") inputTermMaps
  mapM_ (\(xs) -> list xs >> nb "----") outputTermMaps
  nb $ "REPD: " ++ show repd  
  nb $ show repd
  let itms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- inputTermMaps] :: [[TermMap]]
      otms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- outputTermMaps] :: [[TermMap]]

  smr <- buildSubModuleReps itms otms submod (repd - 1)
  list smr
  nb $ show $ length smr
  return smr
  
