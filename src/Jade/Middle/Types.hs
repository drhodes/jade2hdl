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

-----------------------------------------------------------------------------
-- need a list of replicated sub modules that are ready to transform into vhdl.

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
data SubModuleRep = SubModuleRep { smrTermMapInput :: TermMap
                                 , smrTermMapOutput :: TermMap
                                 , smrSubModule :: SubModule
                                 , smrZIndex :: Integer
                                 } deriving (Show, Eq)

firstAndLast xs = (head xs, last xs)
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
  case (compWidth, termWidth) of
    ([Just compWidth], Just termWidth) ->
      let totalWidth = termWidth * numReplications
      in if compWidth == totalWidth
            -- the component has wirewidth equal to the full
            -- replication width, so the component will need to be
            -- sliced in slices equal to the width of the terminal.
         then do nb "case compWidth == totalWidth"
                 let ranges = map firstAndLast $ chunk termWidth (reverse [0 .. totalWidth - 1])
                     srcs = [SigRange compId i j | (i, j) <- ranges]
                     tmap = zipWith (TermAssoc In) srcs (repeat sig)
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
                  let numSlices = fromIntegral $ totalWidth `div` compWidth
                      
                      srcs = take numSlices $ repeat (SigRange compId (compWidth - 1) 0)
                      tmap = zipWith (TermAssoc In) srcs (repeat sig)
                  return $ case dir of
                             In -> tmap
                             Out -> flipTermMap tmap
                ---                
                False -> do
                  nb "case compWidth > totalWidth"
                  impossible "This can't happen if the JADE modules tests pass in JADE"                  
    (_, Nothing) -> die $ "Couldn't determine width of terminal: " ++ show term
    (_, _) -> die $ "Couldn't determine width of componenent: " ++ show comp

allSameLen xs = length (DL.nub $ map length xs) == 1

oneOfEach xs =  
  if 0 `elem` (map length xs)
  then ([], [])
  else (map head xs, map tail xs)

buildSubModuleReps :: [TermMap] -> [TermMap] -> SubModule -> Integer -> J [SubModuleRep]
buildSubModuleReps inputTermMaps outputTermMaps submod zidx = "buildSubModuleRep" <? do
  let (inputTermMap, restITM) = oneOfEach inputTermMaps
      (outputTermMap, restOTM) = oneOfEach outputTermMaps
      smr = SubModuleRep inputTermMap outputTermMap submod zidx

  -- nb $ show zidx
  -- nb (subName submod)
  -- list inputTermMap
  -- list outputTermMap
  
  if zidx < 0
    then return []
    else liftM (smr:) (buildSubModuleReps restITM restOTM submod (zidx-1))


subModuleInstances :: TopLevel -> String -> SubModule -> J [SubModuleRep]
subModuleInstances topl modname submod@(SubModule name loc) = do
  nb "Jade.Vhdl.mkSubModuleInstance"
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

  unless (allSameLen inputTermMaps) $ do nb "input term maps are not all the same length" >> bail
  unless (allSameLen outputTermMaps) $ do nb "output term maps are not all the same length" >> bail
  
  x <- buildSubModuleReps inputTermMaps outputTermMaps submod (repd-1)
  --nb $ show x
  return x
