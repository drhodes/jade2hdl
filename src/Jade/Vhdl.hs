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
import qualified Jade.Middle.Types as MT

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

  let splitAssert [] xs = [xs]
      splitAssert widths xs = let n = fromIntegral $ head widths
                              in (take n xs) : (splitAssert (tail widths) (drop n xs))
  
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
      let inputWidths = map Sig.width ins
      recurse $ zipWith sigAssert inputNames (splitAssert inputWidths asserts)

    Sample _ -> "Samples" <? do
      Outputs outs <- Module.getOutputs m
      let TestLine _ expecteds comment = testline
          outputWidths = map Sig.width outs
          exps = splitAssert outputWidths (map binValToChar expecteds)
          c = case comment of
                Just s -> "// " ++ s
                Nothing -> ""
      os <- mapM Sig.getName outs      
      let txt = [testCaseIfBlock testnum o e c | (o, e) <- zip os exps]
      recurse $ map T.unpack txt

removeQuotes = filter (/= '"') 

testCaseIfBlock :: Integer -> String -> String -> String -> T.Text
testCaseIfBlock testnum signal expected comment =
  let txt = decodeUtf8 $(embedFile "app-data/vhdl/template/test-case-if-block.mustache")
      Right temp = compileTemplate "test-case-if-block" txt
      to_string = format "to_string({0})" [signal]
      mapping = DM.fromList [ ("testnum", toMustache testnum)
                            , ("signal", toMustache signal)
                            , ("expected", toMustache (quote expected))
                            , ("show-expected", toMustache (removeQuotes expected))
                            , ("signal_to_string", toMustache to_string)
                            , ("comment", toMustache comment)
                            ]
  in substitute temp mapping

binValToStdLogic bv = case bv of { H -> "\"1\"" ; L -> "\"0\"" ; Z -> "\"Z\"" }
binValToChar bv = case bv of { H -> '1' ; L -> '0' ; Z -> 'Z' }
quote x = ['"'] ++ x ++ ['"']


sigAssert :: String -> [BinVal] -> String
sigAssert x bv = format "{0} <= {1};" [x, quote $ map binValToChar bv]

portAssoc :: Sig -> J String
portAssoc sig = do
   name <- Sig.getName sig
   let w = Sig.width sig - 1
   return $ format "{0}({1} downto 0) => {0}({1} downto 0)" [name,show w]

--dut : entity work.AND23 port map (a => a, b => b, c => c, d => d, output => result);
mkDUT m modname = "Vhdl.testDUT" <? do  
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m
  portAssociates <- mapM portAssoc (ins ++ outs)
  let portmap = DL.intercalate ", \n" portAssociates
      template = "dut : entity work.{0} port map ({1});"
      mn = Module.mangleModName modname
  return $ format template [mn, portmap]


-- these need to be added 
    -- wire_dLeX1 <= in1;
    -- wire_9651R <= in2;
    -- out1 <= wire_GPXbK;



mkSignalDecls m modname = "Vhdl.mkSignalDecls" <? do
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m
  
  if null (ins ++ outs)
    then return $ "-- no signal decls"
    else let f sig = do
               name <- Sig.getName sig
               let width = Sig.width sig
                   initialVal = quote $ take (fromIntegral width) (repeat '0')
               return $ format "signal {0}: std_logic_vector({1} downto 0) := {2};" [name
                                                                                    , show (width - 1)
                                                                                    , initialVal
                                                                                    ]
                 
         in do sigDecls <- mapM f (ins ++ outs)
               return $ DL.intercalate "\n" sigDecls
  
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

  let renderPort dir sig = do
        name <- Sig.getName sig
        let w = Sig.width sig
        return $ format "{0}: {1} std_logic_vector({2} downto 0)" [ name, dir, show $ w - 1]

  portIns <- mapM (renderPort "in") ins
  portOuts <- mapM (renderPort "out") outs
  
  let ports = T.pack $ DL.intercalate ";\n" (portIns ++ portOuts)

  nodeDecls <- mkNodeDecls topl modname
  outputWires <- T.intercalate (T.pack "\n") `liftM` mapM (connectOutput topl modname) outs
  inputWires <- T.intercalate (T.pack "\n") `liftM` mapM (connectInput topl modname) ins
  
  let txt = decodeUtf8 $(embedFile "app-data/vhdl/template/combinational-module.mustache")
      Right temp = compileTemplate "combinational-module.mustache" txt
      mapping = DM.fromList [ ("module-name", toMustache (Module.mangleModName modname))
                            , ("ports", toMustache ports)
                            , ("node-declarations", toMustache nodeDecls)
                            , ("submodule-entity-instances", toMustache  (T.intercalate  (T.pack "\n") instances))
                            , ("maybe-wire-input", toMustache inputWires)
                            , ("maybe-wire-output", toMustache outputWires)
                            ]
  return $ substitute temp mapping

------------------------------------------------------------------
comma = T.pack ", \n"

mkSigName :: Sig -> J T.Text
mkSigName sig = do
  case sig of
    SigIndex name idx -> return $ T.pack $ format "{0}({1})" [name, show idx]
    x -> unimplemented $ "Vhdl.mkSigName: " ++ show x

mkTermAssoc :: MT.TermAssoc -> J T.Text
mkTermAssoc (MT.TermAssoc dir src tgt) = do
  -- TermAssoc {taDir = In, taSrc = SigIndex "A" 0, taTgt = SigIndex "in1" 0}
  srcTxt <- mkSigName src
  tgtTxt <- mkSigName tgt
  case dir of 
    In -> return $ T.concat [ tgtTxt , T.pack " => " , srcTxt ]
    Out -> return $ T.concat [ srcTxt , T.pack " => " , tgtTxt ]


mkTermAssign :: MT.TermAssoc -> J T.Text
mkTermAssign (MT.TermAssoc dir src tgt) = do
  -- TermAssoc {taDir = In, taSrc = SigIndex "A" 0, taTgt = SigIndex "in1" 0}
  srcTxt <- mkSigName src
  tgtTxt <- mkSigName tgt
  case dir of 
    Out -> return $ T.concat [ tgtTxt , T.pack " <= " , srcTxt ]
    In -> return $ T.concat [ srcTxt , T.pack " <= " , tgtTxt ]


mkTermMap :: MT.TermMap -> J T.Text
mkTermMap xs = "Vhdl.mkTermMap" <? do
  ts <- mapM mkTermAssoc xs
  return $ T.concat $ DL.intersperse comma ts
    
mkPortMap :: [MT.TermMap] -> [MT.TermMap] -> J T.Text
mkPortMap ins outs = "Vhdl.mkPortMap" <? do
  portIns <- mapM mkTermMap ins
  portOuts <- mapM mkTermMap outs
  return $ T.concat [ T.intercalate comma portIns
                    , comma
                    , T.intercalate comma portOuts ]
    
------------------------------------------------------------------
mkSubModuleInstance :: TopLevel -> String -> SubModule -> J T.Text
mkSubModuleInstance topl modname submod@(SubModule name loc) = do
  nb "Jade.Vhdl.mkSubModuleInstance"
  subModuleReps <- MT.subModuleInstances topl modname submod

  let mkOneInstance submodrep@(MT.SubModuleRep ins outs _ zIdx) = do
        portmap <- mkPortMap ins outs
        let label = format "{0}_{1}_{2}" [ Module.mangleModName name
                                         , take 5 $ hashid loc
                                         , show zIdx ]
        
        -- u1 : entity work.AND2 port map (in1 => a, in2 => b, out1 => w1);
        let txt = "{{label}} : entity work.{{submod-name}} port map ({{{port-map}}});"
            Right template = compileTemplate "mkSubModuleInstance" (T.pack txt)
            mapping = DM.fromList [ ("label", toMustache label)
                                  , ("submod-name", toMustache $ Module.mangleModName name)
                                  , ("port-map", toMustache portmap)
                                  ]
        return $ substitute template mapping
  liftM (T.intercalate (T.pack "\n")) $ mapM mkOneInstance subModuleReps

------------------------------------------------------------------
-- get all node names needed for wiring.
-- no input names.
mkNodeDecls topl modname =
  "Jade.Vhdl.mkNodeDecls" <? do
  Inputs ins <- TopLevel.getInputs topl modname
  Outputs outs <- TopLevel.getOutputs topl modname
  ignore <- mapM Sig.getName (ins ++ outs)
  
  comps <- TopLevel.components topl modname
  compNames <- mapM (GComp.name . GComp.removeTerms) comps
  
  let keepers = [comp | (n, comp) <- zip compNames comps, n `notElem` ignore]

  let f comp = do        
        n <- GComp.name $ GComp.removeTerms comp
        w <- liftM maximum $ GComp.width comp
        let temp = "signal {0} : std_logic_vector({1} downto 0);"
        case w of
          Just w -> return $ format temp [n, show (w - 1)] -- one less because of zero indexing.
          Nothing -> die "Couldn't determine width of this component"
      
  if null keepers
    then return "-- no node decls"
    else do sigDecls <- mapM f keepers
            return $ concat $ DL.intersperse "\n" sigDecls


-- If output signals are not connected directly to a submodule output,
-- then there is no structural output to that output.
connectOutput :: TopLevel -> String -> Sig -> J T.Text
connectOutput topl modname outSig = "Jade.Vhdl.connectOutput" <? do
  outTermMap <- MT.connectOneOutput topl modname outSig
  txts <- mapM mkTermAssign outTermMap
  return $ T.concat [T.append t (T.pack ";\n") | t <- txts]


-- If output signals are not connected directly to a submodule output,
-- then there is no structural output to that output.
connectInput :: TopLevel -> String -> Sig -> J T.Text
connectInput topl modname inSig = "Jade.Vhdl.connectInput" <? do
  inTermMap <- MT.connectOneInput topl modname inSig
  txts <- mapM mkTermAssign inTermMap
  return $ T.concat [T.append t (T.pack ";\n") | t <- txts]

