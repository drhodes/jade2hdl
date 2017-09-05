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
import qualified Jade.GComp as GComp
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

connectOneOutput topl modname outSig = "Middle/Types.connectOneOutput" <? do
  nb "connectOneInput is reused here, the assignments are flipped around"
  map flipAssign `liftM` connectOneInput topl modname outSig 

connectOneInput :: TopLevel -> String -> Sig -> J [SigAssign]
connectOneInput topl modname (SigConcat sigs) = "Middle/Types.connectOneInput@(SigConcat)" <? do
  unimplemented

connectOneInput topl modname inSig = "Middle/Types.connectOneInput" <? do  
  inSigNames <- Sig.getNames inSig
  case inSigNames of
    [] -> die "No signames found"
    [signame] -> do
      -- find components with signame
      comps <- TopLevel.getComponentsWithName topl modname signame
      -- for each component, assign the signame
      concatMapM (assignSig topl modname signame) comps
    xs -> unimplemented


-- is this comp connected to the output terminal of a submodule or
-- an input terminal?



-- | assign the signame to the net it's connected to 
assignSig topl modname signame comp = "Middle/Types.assignSig" <? do
  workingSigs <- GComp.getSigsWithIdentNoFlatten comp signame
  exploded <- concatMapM Sig.explode workingSigs
  
  let idxs = downFrom $ length exploded - 1
      indexedNames = zip exploded idxs

  matchedSigs <- filterM (\(sig, idx) -> Sig.hasIdent sig signame) indexedNames
  compName <- GComp.name comp

  let assigns = [SigAssign sig (SigIndex compName (fromIntegral idx))
                | (sig, idx) <- matchedSigs]
  return assigns

sharesInputP topl modname comp = "Middle.Types.sharesInputP" <? do
  let ts = GComp.getTerminals comp
  drivers <- mapM (TopLevel.getInputTermDriver topl modname) ts
  nb "Drivers!!"
  list drivers
  
  if length (filterOut (==Nothing) drivers) == 0
    then return False
    else return True


sigNameDirection topl modname comp = "Middle.Types.isSigNameDriver" <? do
  -- does signames share a comp with a terminal that belongs to a submodules Input?
  sharesInput <- sharesInputP topl modname comp  
  nbf "sharesInput? {0}" [show sharesInput]
  return $ if sharesInput
           then In
           else Out
                
----
connectSigName topl modname sigName = "Middle/Types.connectSigName" <? do  
  -- find components with signame
  comps <- TopLevel.getComponentsWithName topl modname sigName

  nbf "connectSigName:sigName = {0}" [sigName]
  dirs <- mapM (sigNameDirection topl modname) comps
  -- for each component, locate the z-index of the signame  
  assigns <- concatMapM (assignSig topl modname sigName) comps
  
  return [case dir of
            In -> flipAssign assn
            Out -> assn
         | (assn, dir) <- zip assigns dirs]

  
genbits n | n == 0 = []
          | n `mod` 2 == 0 = 0 : (genbits next)
          | otherwise = 1 : (genbits next)
  where next = n `div` 2

connectConstantComp topl modname comp = "Middle.Types/connectConstantComp" <? do
  let quotedSigs = GComp.getQuotedSigs comp
  compWidth <- GComp.width comp
  compName <- GComp.name comp
  case quotedSigs of
    [SigQuote val numBits] -> do
      when (compWidth /= numBits) $ do
        let msg = "width mismatch in signals component width: {0} and constant width: {1}"
        die $ format msg [show compWidth, show numBits]

      -- this is a twos complement representation.
      let bits = reverse $ take (fromInteger numBits) $ (genbits val) ++ repeat 0
          
      nb "The targets are the comp replicated"
      let tgts = map (SigIndex compName) $ reverse [0 .. compWidth - 1]
      let srcs = [SigSimple (format "'{0}'" [show b]) | b <- bits]

      return $ zipWith SigAssign srcs tgts
    [] -> return []
    xs -> die $ "unhandled case in connectConstantComp: " ++ show xs
    
---------------------------------------------------------------------------------------------------
replicateOneTerminal :: Integer -> Direction -> Terminal -> GComp -> J TermMap
replicateOneTerminal numReplications dir term@(Terminal _ sig) comp =
  "Middle/Types.replicateOneTerminal" <? do
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
          -- ^ interfunction comment.
      in if compWidth == totalWidth
            -- the component has wirewidth equal to the full
            -- replication width, so the component will need to be
            -- sliced in slices equal to the width of the terminal.

         then do nb "case compWidth == totalWidth"
                 let singles = reverse [0 .. totalWidth - 1]
                     srcs = map (SigIndex compId) singles
                     tmap = zipWith (TermAssoc In) srcs (cycle termSigs)
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
                  return $ case dir of
                             In -> tmap
                             Out -> flipTermMap tmap
                ---                
                False -> do
                  nb "case compWidth > totalWidth"
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

  let itms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- inputTermMaps] :: [[TermMap]]
      otms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- outputTermMaps] :: [[TermMap]]

  buildSubModuleReps itms otms submod (repd - 1)

subModuleInstances topl modname (SubMemUnit memunit) = do
  nb "Middle.Types.subModuleInstances"
  die "This should not be called on SubMemUnit, instead call memUnitInstance"
  

memUnitInstance :: TopLevel -> String -> MemUnit -> J SubModuleRep
memUnitInstance topl modname memunit = "Middle/Types.memUnitInstance" <? do
  let zidx = 0
      repd = 1
  m <- TopLevel.getModule topl modname
  
  inputTerms <- MemUnit.getInputTerminals memunit
  inputComps <- mapM (TopLevel.componentWithTerminal topl modname) inputTerms

  outputTerms <- MemUnit.getOutputTerminals memunit
  outputComps <- mapM (TopLevel.componentWithTerminal topl modname) outputTerms
  

  inputTermMaps <- zipWithM (replicateOneTerminal repd In) inputTerms (map GComp.removeTerms inputComps)
  outputTermMaps <- zipWithM (replicateOneTerminal repd Out) outputTerms (map GComp.removeTerms outputComps)

  let itms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- inputTermMaps] :: [[TermMap]]
      otms = [chunk (length tmap `div` fromIntegral repd) tmap | tmap <- outputTermMaps] :: [[TermMap]]

  
  rep <- buildSubModuleReps itms otms (SubMemUnit memunit) 0
  when (length rep /= 1) (impossible "This should contain one memunit representation")
  return $ head rep

  
