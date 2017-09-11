{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Jade.Back.Viz where
{-
import Control.Monad
import Data.FileEmbed
import Data.Text.Encoding
import Jade.Types
import Jade.Util
import Text.Format
import Text.Mustache
import Text.Mustache.Compile (mustache)
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Map as DM
import qualified Data.Text as T
import qualified Data.Text.IO as DT
import qualified Jade.Decode as Decode
import qualified Jade.Net as Net
import qualified Jade.MemUnit as MemUnit
import qualified Jade.Middle.Types as MT
import qualified Jade.ModTest as ModTest
import qualified Jade.Module as Module
import qualified Jade.Part as Part
import qualified Jade.Sig as Sig
import qualified Jade.TopLevel as TopLevel
import qualified Jade.UnionFindST as UnionFindST

portAssoc :: Sig -> J String
portAssoc (SigConcat _) = die "Viz.portAssoc doesn't support SigConcat"
portAssoc sig = "Viz.portAssoc" <? do
  [name] <- Sig.getNames sig
  let w = Sig.width sig - 1
  return $ format "{0}({1} downto 0) => {0}({1} downto 0)" [name,show w]

mkSignalDecls m modname = "Viz.mkSignalDecls" <? do
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m
  let sigs = ins ++ outs
  
  if null sigs
    then return $ "-- no signal decls"
    else let f (SigConcat _) = die "mkSignalDecls doesn't support SigConcat"
             f sig = do
               [name] <- Sig.getNames sig
               let width = Sig.width sig
                   initialVal = quote $ take (fromIntegral width) (repeat 'U')
                   fmtargs = [name, show (width - 1), initialVal]
               return $ format "signal {0}: std_logic_vector({1} downto 0) := {2};" fmtargs 
         in do sigDecls <- mapM f sigs
               return $ DL.intercalate "\n" sigDecls

------------------------------------------------------------------
genPort dir (SigConcat _) = die "Viz.mkModule/genPort doesn't support SigConcat"
genPort dir sig = "Viz.genPort" <? do
  [name] <- Sig.getNames sig
  let w = Sig.width sig
  return $ format "{1} -> {0}_{2}" [ name, dir, show $ w - 1]

genPorts ins outs = "Viz.genPorts" <? do
  portIns <- mapM (genPort "in") ins
  portOuts <- mapM (genPort "out") outs
  return $ T.pack $ DL.intercalate ";\n" (concat [portIns, portOuts])

mkModule modname = ("Viz.mkModule: " ++ modname) <? do
  nb $ "Jade.Viz.mkModule, convert module to VHDL: " ++ modname
  
  m <- TopLevel.getModule modname ? modname
  schem <- Module.getSchematic m

  subs <- TopLevel.getSubModules modname
  nets <- TopLevel.nets  modname  
  instances <- mapM (mkSubModuleInstance modname) subs
  
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m
  ports <- genPorts ins outs
  
  nodeDecls <- mkNodeDecls modname

  outMap <- mapM (connectOutput modname) outs
  outputWires <- T.intercalate (T.pack "\n") <$> (return outMap)
  inputWires <- connectAllInputs modname ins

  -- TODO this is where all the spaces are being inserted.
  constantWires <- T.intercalate (T.pack "\n") <$> mapM (connectConstant modname) nets
  
  let txt = decodeUtf8 $(embedFile "app-data/viz/template/combinational-module.mustache")
      Right temp = compileTemplate "combinational-module.mustache" txt
      mapping = DM.fromList
        [ ("module-name", toMustache (Module.mangleModName modname))
        , ("ports", toMustache ports)
        , ("node-declarations", toMustache (nodeDecls)) -- ++ nodeDeclsNotFromInput))
        , ("submodule-entity-instances", toMustache  (T.intercalate  (T.pack "\n") instances))
        , ("maybe-wire-input", toMustache inputWires) 
        , ("maybe-wire-constants", toMustache constantWires)
        , ("maybe-wire-output", toMustache outputWires)
        ]
  return $ substitute temp mapping

comma = T.pack ", \n"

mkSigName :: Sig -> J T.Text
mkSigName sig = "Viz.mkSigName" <? do
  let p x = return $ T.pack x
  case sig of
    SigIndex name idx ->
      p $ format "{0}_{1}" [name, show idx]
    SigSimple name ->
      p $ format "{0}" [name]
    x -> do nb "These other sigtype aren't meant to be code generated."
            nb "Boil the signal down to SigIndex in the program"
            nb $ show x
            unimplemented

mkTermAssoc :: MT.TermAssoc -> J T.Text
mkTermAssoc (MT.TermAssoc dir src tgt) = "Viz.mkTermAssoc" <? do
  srcTxt <- mkSigName src
  tgtTxt <- mkSigName tgt
  case dir of 
    In -> return $ T.concat [ tgtTxt , T.pack " -> " , srcTxt ]
    Out -> return $ T.concat [ srcTxt , T.pack " -> " , tgtTxt ]

mkTermAssign :: MT.SigAssign -> J T.Text
mkTermAssign (MT.SigAssign src tgt) = "Viz.mkTermAssign" <? do
  srcTxt <- mkSigName src
  tgtTxt <- mkSigName tgt
  return $ T.concat [ srcTxt , T.pack " -> " , tgtTxt ]

mkTermMap :: MT.TermMap -> J T.Text
mkTermMap xs = "Viz.mkTermMap" <? do
  ts <- mapM mkTermAssoc xs
  return $ T.concat $ DL.intersperse comma ts
    
mkPortMap :: [MT.TermMap] -> [MT.TermMap] -> J T.Text
mkPortMap ins outs = "Viz.mkPortMap" <? do
  portIns <- mapM mkTermMap ins
  portOuts <- mapM mkTermMap outs
  let f= T.intercalate comma
  return $ T.concat [ f portIns, comma, f portOuts ]
    
------------------------------------------------------------------
replicatedLabel subModName loc zIdx = 
  format "{0}_{1}_{2}" [ Module.mangleModName subModName
                       , take 5 $ hashid (zIdx, subModName, loc)
                       , show zIdx ]

mkSubModuleInstance :: String -> SubModule -> J T.Text
mkSubModuleInstance modname submod@(SubModule name loc) = 
  "Jade.Viz.mkSubModuleInstance" <? do
  subModuleReps <- MT.subModuleInstances modname submod
  let repDepth = length subModuleReps
  
  let mkOneInstance submodrep@(MT.SubModuleRep ins outs _ zIdx) = do
        portmap <- mkPortMap ins outs
        let label = replicatedLabel name loc zIdx        
        -- u1 : entity work.AND2 port map (in1 => a, in2 => b, out1 => w1);
        let txt = "{{{port-map}}}" --PORTMAP {{{port-map}}};"
            Right template = compileTemplate "mkSubModuleInstance" (T.pack txt)
            mapping = DM.fromList [ ("label", toMustache label)
                                  , ("submod-name", toMustache $ Module.mangleModName name)
                                  , ("port-map", toMustache portmap)
                                  ]
        return $ substitute template mapping
  T.intercalate (T.pack "\n") <$> mapM mkOneInstance subModuleReps

mkSubModuleInstance modname mem@(SubMemUnit memunit) =
  "Jade.Viz.mkSubModuleInstance" <? do
  nb "MemUnits are not replicated in JADE."
  MT.SubModuleRep ins outs _ zIdx <- MT.memUnitInstance modname memunit

  portmap <- mkPortMap ins outs
  let loc = memCoord3 memunit
      name = memName memunit
  let label = replicatedLabel name loc zIdx        
  -- u1 : entity work.AND2 port map (in1 => a, in2 => b, out1 => w1);
  let txt = "{{label}} : entity work.{{submod-name}} port map ({{{port-map}}});"
      Right template = compileTemplate "mkSubModuleInstance" (T.pack txt)
      mapping = DM.fromList [ ("label", toMustache label)
                            , ("submod-name", toMustache $ Module.mangleModName name)
                            , ("port-map", toMustache portmap)
                            ]
  return $ substitute template mapping
  
------------------------------------------------------------------
mkNetName net = "Viz.mkNetName" <? do        
  n <- Net.name net -- get the net name
  w <- Net.width net -- get the width
  let temp = "{0};"
  return $ format temp [n, show (w - 1)] -- one less because of zero indexing.

-- get all node names needed for wiring.
-- no input names.
mkNodeDecls modname = "Jade.Viz.mkNodeDecls" <? do
  nb "These ins and outs aren't going to be SigConcats, ever."
  Inputs ins <- TopLevel.getInputs modname
  Outputs outs <- TopLevel.getOutputs modname
  ignore <- concatMapM Sig.getNames (ins ++ outs)
  
  nets <- TopLevel.nets modname
  netNames <- mapM Net.name nets
  
  let keepers = [net | (n, net) <- zip netNames nets, n `notElem` ignore]

  if null keepers
    then return "-- no node decls"
    else do sigDecls <- mapM mkNetName keepers
            return $ concat $ DL.intersperse "\n" sigDecls

declareSigName modname signame = "Viz.declareSigName" <? do
  width <- TopLevel.getWidthOfSigName modname signame
  let temp = "signal {0} : std_logic_vector({1} downto 0);"
  return $ format temp [signame, show (width - 1)] -- one less because of zero indexing.

internalSigNames modname = do
  -- these are module level inputs and outputs not 
  Inputs ins <- TopLevel.getInputs modname
  Outputs outs <- TopLevel.getOutputs modname
  ignore <- concatMapM Sig.getNames (ins ++ outs)
  
  nets <- TopLevel.nets modname
  netNames <- mapM Net.name nets
  list netNames
  nb "-- ignores"
  list ignore
  allNames <- TopLevel.getAllSigNames modname
  nb "-- allnames"
  list allNames                                       
  let keepers = (DL.nub allNames) DL.\\ (DL.nub ignore)
  return keepers

mkNodeDeclsNotFromInput modname = "Jade.Viz.mkNodeDeclsFromInput" <? do
  keepers <- internalSigNames modname
  
  nb "-- keepers"
  list keepers
  sigDecls <- mapM (declareSigName modname) keepers
  return $ concat $ DL.intersperse "\n" sigDecls

-- | some signal names are not module inputs or outputs, they are
-- internal signal names.  Search for the internal signal names and
-- declare them.
-- mkInternalDecls topl modname =
--   topl modname

withSemiColonNewLines txts = T.concat [T.append t (T.pack ";\n") | t <- txts]

-- If output signals are not connected directly to a submodule output,
-- then there is no structural output to that output.
connectOutput :: String -> Sig -> J T.Text
connectOutput modname outSig = "Viz.connectOutput" <? do
  assignMap <- MT.connectOneOutput modname outSig
  withSemiColonNewLines <$> mapM mkTermAssign assignMap

-- If input signals are not connected directly to a submodule output,
-- then there is no structural input for that input.
connectAllInputs modname inSigs = "Viz.connectAllInputs" <? do
  nb "!! connectAllInputs.inSigs"
  list inSigs
  assignMap <- concatMapM (MT.connectOneInput modname) inSigs
  withSemiColonNewLines <$> mapM mkTermAssign assignMap
  
connectConstant :: String -> Net -> J T.Text
connectConstant modname net = "Viz.connectConstant" <? do
  assignMap <- MT.connectConstantNet modname net
  withSemiColonNewLines <$> mapM mkTermAssign assignMap


------------------------------------------------------------------
mkAllMods modname = "Viz.mkAllMods" <? do
  userModNames <- TopLevel.dependencyOrder modname
  T.concat <$> mapM mkModule (userModNames ++ [modname])
  
-}
