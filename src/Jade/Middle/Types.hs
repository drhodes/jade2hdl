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
connectOneOutput topl modname outSig = "connectOneOutput" <? do
  -- find the components with the name from sig.
  outSigName <- Sig.getName outSig
  comps <- TopLevel.getComponentsWithName topl modname outSigName
  
  -- determine width of outSig, this is absolutely known and defined
  -- in the jade module, this width can be used to deduce the width of other parts.
  let outputWidth = Sig.width outSig 
  
  -- from the components that have the name from sig, figure out which
  -- slice of which components to take and stack them up
  let getSlices comp = "getSlice" <? do
        compName <- GComp.name (GComp.removeTerms comp)
        compWidth <- GComp.width comp        
        -- find outSig in comp.
        matchingSigs <- GComp.getSigsWithIdent comp outSigName
        -- for each sig create a termmap, use the
        singles <- mapM Sig.explode matchingSigs

        let tgts = reverse $ DL.sort $ concat singles
            srcs = map (SigIndex compName) $ reverse [0 .. compWidth - 1]

        when (length tgts /= length srcs) $ do
          nb "The lengths of the targets and sources are not the same"
          bail
            
        return $ zipWith (TermAssoc Out) srcs tgts
        
  liftM concat $ mapM getSlices comps



connectOneInput topl modname inSig = "connectOneInput" <? do
  -- find the components with the name from sig.
  inSigName <- Sig.getName inSig
  comps <- TopLevel.getComponentsWithName topl modname inSigName
  
  -- determine width of inSig, this is absolutely known and defined
  -- in the jade module, this width can be used to deduce the width of other parts.
  let inputWidth = Sig.width inSig 
  
  -- from the components that have the name from sig, figure in which
  -- slice of which components to take and stack them up
  let getSlices comp = "getSlice" <? do
        compName <- GComp.name (GComp.removeTerms comp)
        compWidth <- GComp.width comp        
        -- find inSig in comp.
        matchingSigs <- GComp.getSigsWithIdent comp inSigName
        -- for each sig create a termmap, use the
        singles <- mapM Sig.explode matchingSigs

        let tgts = reverse $ DL.sort $ concat singles
            srcs = map (SigIndex compName) $ reverse [0 .. compWidth - 1]

        when (length tgts /= length srcs) $ do
          nb "The lengths of the targets and sources are not the same"
          bail
            
        return $ zipWith (TermAssoc In) srcs tgts
  liftM concat $ mapM getSlices comps


-- connectConstantComp topl modname comp = "connectConstantComp" <? do
--   let quotedSigs = GComp.getQuotedSigs comp
--   case quotedSigs of
--     [x] -> do nb "found a quoted sig, need to convert it to binary and carve it into \
--                  \an integral divisor of the width of comp"
--               compWidth <- liftM maximum $ GComp.width comp -- again, this should be refactored.
--               case compWidth of
--                 Nothing -> die $ format "Couldn't determine the width of component: {0}" [compName]
--                 Just compWidth -> do
--                   nb "The targets are the comp replicated"
--                   let tgts = map (SigIndex compName) $
                        
    
--     [] -> return []
    





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
                      srcReplication = concat $ repeat srcs
                      tmap = take (fromIntegral totalWidth) (cycle $ zipWith (TermAssoc In) srcReplication termSigs)
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
  
