{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Jade.Vhdl where

import qualified Data.Map as DM
import qualified Data.List as DL
import Jade.Types
import qualified Jade.Part as Part
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import qualified Jade.UnionFindST as UnionFindST
import qualified Jade.Sig as Sig
import qualified Jade.GComp as GComp
import qualified Jade.Middle.Types as T

import Control.Monad
import Jade.Util
import Text.Mustache.Compile (mustache)
import Text.Format
import Text.Mustache
import Text.Mustache.Parser
import Text.Mustache.Compile
import Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Map as DM
import Data.Text.Encoding
import qualified Data.Text.IO as DT

mkTestLine :: Module -> [Action] -> TestLine -> Integer -> J [String]
mkTestLine _ [] _ _ = return []
mkTestLine m (act:actions) testline testnum = "mkTestLine" <? do
  let recurse x = liftM (x++) (mkTestLine m actions testline testnum)
  case act of
    Tran dur ->
      case dur of
        Nanosecond ns -> recurse $ [format "wait for {0} ns;" [show ns]]
        Millisecond ms -> recurse $ [format "wait for {0} ms;" [show ms]]
        
    Assert _ -> "Assert" <? do
      -- a <= '0'; b <= '0'; c <= '0'; d <= '0';
      let TestLine asserts _ _ = testline
      Inputs ins <- Module.getInputs m
      inputNames <- mapM Sig.getName ins
      recurse $ zipWith sigAssert inputNames asserts

    Sample _ -> "Samples" <? do
      Outputs outs <- Module.getOutputs m
      let TestLine _ expecteds comment = testline 
          exps = map binValToStdLogic expecteds
          c = case comment of
                Just s -> "// " ++ s
                Nothing -> ""
      os <- mapM Sig.getName outs      
      let txt = [testCaseIfBlock testnum o e c | (o, e) <- zip os exps]
      recurse $ map T.unpack txt

testCaseIfBlock :: Integer -> String -> String -> String -> T.Text
testCaseIfBlock testnum signal expected comment =
  let txt = decodeUtf8 $(embedFile "app-data/vhdl/template/test-case-if-block.mustache")
      Right temp = compileTemplate "test-case-if-block" txt
      to_string = format "to_string({0})" [signal]
      mapping = DM.fromList [ ("testnum", toMustache testnum)
                            , ("signal", toMustache signal)
                            , ("expected", toMustache expected)
                            , ("signal_to_string", toMustache to_string)
                            , ("comment", toMustache comment)
                            ]
  in substitute temp mapping

binValToStdLogic bv = case bv of { H -> "'1'" ; L -> "'0'" ; Z -> "'Z'" } 
sigAssert x bv = format "{0} <= {1};" [x, binValToStdLogic bv]

portAssoc :: Sig -> J String
portAssoc sig = do
   name <- Sig.getName sig
   return $ format "{0} => {0}" [name]

--dut : entity work.AND23 port map (a => a, b => b, c => c, d => d, output => result);
mkDUT m modname = "Vhdl.testDUT" <? do  
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m
  portAssociates <- mapM portAssoc (ins ++ outs)
  let portmap = DL.intercalate ", " portAssociates
      template = "dut : entity work.{0} port map ({1});"
      mn = Module.mangleModName modname
  return $ format template [mn, portmap]

mkSignalDecls m modname = "Vhdl.mkSignalDecls" <? do
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m
  
  names <- mapM Sig.getName (ins ++ outs)
  if null names
    then return $ "-- no signal decls"
    else return $ "signal " ++ DL.intercalate ", " names ++ ": std_logic;"
  
mkTestBench :: TopLevel -> [Char] -> J T.Text
mkTestBench topl modname =
  "Jade.Vhdl.mkTestBench" <? do
  m <- TopLevel.getModule topl modname
  sigDecls <- mkSignalDecls m modname
  dut <- mkDUT m modname
  tlines <- Module.testLines m
  CycleLine actions <- Module.cycleLine m 
  cases <- sequence [mkTestLine m actions testline testnum | (testline, testnum) <- zip tlines [1..]]
  
  let txt = decodeUtf8 $(embedFile "app-data/vhdl/template/combinational-testbench.mustache")
      Right temp = compileTemplate "combinational-testbench.mustache" txt
      mapping = DM.fromList [ ("testbench-name", toMustache $ (Module.mangleModName modname) ++ "_tb")
                            , ("signal-decls", toMustache sigDecls)
                            , ("dut", toMustache dut)
                            , ("test-cases", toMustache (DL.intercalate  "\n" (concat cases)))
                            ]
  return $ substitute temp mapping

mkCombinationalTest topl modname =
  ("Jade.Vhdl.mkCombinationalTest: " ++ modname) <? do
  mkTestBench topl modname


------------------------------------------------------------------

--mkModule :: TopLevel -> String -> J Maybe String)
mkModule topl modname = do
  nb $ "Jade.Vhdl.mkModule, convert module to VHDL: " ++ modname

  m <- TopLevel.getModule topl modname

  schem <- case moduleSchem m of
             Just x -> return x
             Nothing -> die $ "No schematic found in module: " ++ modname
  
  -- collect all terminals from submodule in the schematic
  subs <- TopLevel.getSubModules topl modname
  terms <- mapM (TopLevel.getInputTerminals topl) subs
  drivers <- mapM (TopLevel.getInputTermDriver topl modname) (concat terms)

  comps <- TopLevel.components topl modname
  instances <- mapM (mkSubModuleInstance topl modname) subs

  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m

  inNames <- mapM Sig.getName ins
  let portIns = map (++" : in std_logic") inNames

  outNames <- mapM Sig.getName outs
  let portOuts = map (++" : out std_logic") outNames

  let ports = T.pack $ DL.intercalate "; " (portIns ++ portOuts)
  mapM (UnionFindST.nameComp) comps

  nodeDecls <- mkNodeDecls topl modname
  outputWires <- liftM (DL.intercalate "\n") (mapM (connectOutput topl modname) outs)
  
  let txt = decodeUtf8 $(embedFile "app-data/vhdl/template/combinational-module.mustache")
      Right temp = compileTemplate "combinational-module.mustache" txt
      mapping = DM.fromList [ ("module-name", toMustache (Module.mangleModName modname))
                            , ("ports", toMustache ports)
                            , ("node-declarations", toMustache nodeDecls)
                            , ("submodule-entity-instances", toMustache  (T.intercalate  (T.pack "\n") instances))
                            , ("maybe-wire-output", toMustache outputWires)
                            ]
  return $ substitute temp mapping

------------------------------------------------------------------
mkSubModuleInstance topl modname submod@(SubModule name loc) = do
  nb "Jade.Vhdl.mkSubModuleInstance"
  
  m <- TopLevel.getModule topl name 
  inputTerms <- Module.getInputTerminals m loc
  outputTerms <- Module.getOutputTerminals m loc

  let termName (Terminal _ sig)  = Sig.getName sig
  
  inComps <- mapM (TopLevel.componentWithTerminal topl modname) inputTerms
  inSigNames <- mapM UnionFindST.nameComp inComps
  inTermNames <- mapM termName inputTerms

  outComps <- mapM (TopLevel.componentWithTerminal topl modname) outputTerms
  outSigNames <- mapM UnionFindST.nameComp outComps
  outTermNames <- mapM termName outputTerms
  
  let ps = zipWith (\x y -> x ++ " => " ++ y) (inTermNames ++ outTermNames) (inSigNames ++ outSigNames)
      portmap = DL.intercalate ", " ps
      label = (Module.mangleModName name) ++ "_" ++ (take 5 $ hashid loc)
  
  -- u1 : entity work.AND2 port map (in1 => a, in2 => b, out1 => w1);
  let txt = "{{label}} : entity work.{{submod-name}} port map ({{{port-map}}});"
      Right template = compileTemplate "mkSubModuleInstance" (T.pack txt)
      mapping = DM.fromList [ ("label", toMustache label)
                            , ("submod-name", toMustache $ Module.mangleModName name)
                            , ("port-map", toMustache portmap)
                            ]
  return $ substitute template mapping

------------------------------------------------------------------
-- get all node names needed for wiring.
-- no input names.
mkNodeDecls topl modname =
  "Jade.Vhdl.mkNodeDecls" <? do
  Inputs ins <- TopLevel.getInputs topl modname
  Outputs outs <- TopLevel.getOutputs topl modname
  ignore <- mapM Sig.getName (ins ++ outs)
  
  comps <- TopLevel.components topl modname
  compNames <- mapM UnionFindST.nameComp comps

  let keepers = DL.intercalate ", " [n | n <- compNames, n `notElem` ignore]
      temp = "signal {0} : std_logic;"
      
  if null keepers
    then return "-- no node decls"
    else return $ format temp [keepers]


-- If output signals are not connected directly to a submodule output,
-- then there is no structural output to that output.
connectOutput :: TopLevel -> String -> Sig -> J String
connectOutput topl modname outSig = "Jade.Vhdl.connectOutput" <? do
  nb "Does outSig share a component with a terminal?"
  connectedToSubMod <- TopLevel.sigConnectedToSubModuleP topl modname outSig
  nb "If so, then it's all set"

  if not connectedToSubMod
    then do nb "otherwise look in that component to see if contains an .input"
            comps <- TopLevel.components topl modname
            let compsWithOutputs = filter (flip GComp.hasSig outSig) comps
            
            case length compsWithOutputs of
              0 -> die $ "No comps found with sig: " ++ show outSig
              1 -> "if it does, then connect .input to outSig" <? do
                      let comp = head compsWithOutputs
                      Inputs ins <- TopLevel.getInputs topl modname
                      
                      case filter (GComp.hasSig comp) ins of
                        [driver] -> do
                          d <- Sig.getName driver
                          o <- Sig.getName outSig
                          return $ format "{0} <= {1};" [o, d]
                        _ -> return "WUT"
                      
              _ -> die $ "strange, more than one component found with outSig: " ++ show outSig
    else return ""
    
  
