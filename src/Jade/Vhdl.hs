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
import qualified Jade.ModTest as ModTest
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

  let Just modt = moduleTest m
  
  case act of
    Tran dur ->
      case dur of
        Nanosecond ns -> recurse $ [format "wait for {0} ns;" [show ns]]
        Millisecond ms -> recurse $ [format "wait for {0} ms;" [show ms]]
        
    Assert _ -> "Assert" <? do
      -- a <= '0'; b <= '0'; c <= '0'; d <= '0';
      -- let TestLine _ _ = testline
      asserts <- ModTest.assertBitVals modt testline
      Inputs ins <- Module.getInputs m      
      inputNames <- concatMapM Sig.getNames ins
      let inputWidths = map Sig.width ins
      recurse $ zipWith sigAssert inputNames (splitAssert inputWidths asserts)

    Sample _ -> "Samples" <? do
      expecteds <- ModTest.sampleBitVals modt testline
      Outputs outs <- Module.getOutputs m
      let TestLine _ comment = testline
          outputWidths = map Sig.width outs
          exps = splitAssert outputWidths (map binValToChar expecteds)
          c = case comment of
                Just s -> "// " ++ s
                Nothing -> ""
      nb $ show ("expecteds", expecteds)
      nb $ show ("expected testline", testline)
      os <- concatMapM Sig.getNames outs      
      txt <- sequence [testCaseIfBlock testnum o e c | (o, e) <- zip os exps]
      recurse $ map T.unpack txt
    x -> "Jade.Vhdl" <? unimplemented (show x)
    --SetSignal (SigSimple "CLK") 1.0    

testCaseIfBlock :: Integer -> String -> String -> String -> J T.Text
testCaseIfBlock testnum signal expected comment = do
  nb $ show ("expected", expected)

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
                
  return $ substitute temp mapping

binValToStdLogic bv = case bv of { H -> quote "1"
                                 ; L -> quote "0"
                                 ; Z -> quote "U" }
                      
binValToChar bv = case bv of { H -> '1'
                             ; L -> '0'
                             ; Z -> 'U' }
                  
sigAssert :: String -> [BinVal] -> String
sigAssert x bv = format "{0} <= {1};" [x, quote $ map binValToChar bv]

portAssoc :: Sig -> J String
portAssoc (SigConcat _) = die "Vhdl.portAssoc doesn't support SigConcat"
portAssoc sig = do
  [name] <- Sig.getNames sig
  let w = Sig.width sig - 1
  return $ format "{0}({1} downto 0) => {0}({1} downto 0)" [name,show w]

--dut : entity work.AND23 port map (a => a, b => b, c => c, d => d, output => result);
mkDUT m modname = "Vhdl.testDUT" <? do  
  Inputs ins <- Module.getInputs m ? (format " in module: {0}" [modname])
  Outputs outs <- Module.getOutputs m
  portAssociates <- mapM portAssoc (ins ++ outs)
  let portmap = DL.intercalate ", \n" portAssociates
      template = "dut : entity work.{0} port map ({1});"
      mn = Module.mangleModName modname
  return $ format template [mn, portmap]

mkSignalDecls m modname = "Vhdl.mkSignalDecls" <? do
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m
  
  if null (ins ++ outs)
    then return $ "-- no signal decls"
    else let f (SigConcat _) = die "mkSignalDecls doesn't support SigConcat"
             f sig = do
               [name] <- Sig.getNames sig
               let width = Sig.width sig
                   initialVal = quote $ take (fromIntegral width) (repeat '0')
                   fmtargs = [name, show (width - 1), initialVal]
               return $ format "signal {0}: std_logic_vector({1} downto 0) := {2};" fmtargs                 
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
  cases <- sequence [mkTestLine m actions testline testnum |
                      (testline, testnum) <- zip tlines [1..]]
  
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

mkModule topl modname = ("Vhdl.mkModule: " ++ modname) <? do
  nb $ "Jade.Vhdl.mkModule, convert module to VHDL: " ++ modname  
  m <- TopLevel.getModule topl modname ? modname

  schem <- case moduleSchem m of
             Just x -> return x
             Nothing -> die $ "No schematic found in module: " ++ modname

  -- collect all terminals from submodule in the schematic
  subs <- TopLevel.getSubModules topl modname

  comps <- TopLevel.components topl modname
  instances <- mapM (mkSubModuleInstance topl modname) subs
  
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m

  let renderPort dir (SigConcat _) = die "mkModule/renderPort doesn't support SigConcat"
      renderPort dir sig = do
        [name] <- Sig.getNames sig
        let w = Sig.width sig
        return $ format "{0}: {1} std_logic_vector({2} downto 0)" [ name, dir, show $ w - 1]

  portIns <- mapM (renderPort "in") ins
  portOuts <- mapM (renderPort "out") outs
  let ports = T.pack $ DL.intercalate ";\n" (portIns ++ portOuts)

  nodeDecls <- mkNodeDecls topl modname
  outputWires <- T.intercalate (T.pack "\n") `liftM` mapM (connectOutput topl modname) outs
  inputWires <- connectAllInputs topl modname ins 
  
  constantWires <- T.intercalate (T.pack "\n") `liftM` mapM (connectConstant topl modname) comps
  
  let txt = decodeUtf8 $(embedFile "app-data/vhdl/template/combinational-module.mustache")
      Right temp = compileTemplate "combinational-module.mustache" txt
      mapping = DM.fromList [ ("module-name", toMustache (Module.mangleModName modname))
                            , ("ports", toMustache ports)
                            , ("node-declarations", toMustache nodeDecls)
                            , ("submodule-entity-instances", toMustache  (T.intercalate  (T.pack "\n") instances))
                            , ("maybe-wire-input", toMustache inputWires)
                            , ("maybe-wire-constants", toMustache constantWires)
                            , ("maybe-wire-output", toMustache outputWires)
                            ]
  return $ substitute temp mapping

mkAllMods topl = "Vhdl.mkAllMods" <? do
  let  userModNames = [name | (name, _) <- TopLevel.modules topl, name `startsWith` "/user"]
  T.concat `liftM` mapM (mkModule topl) userModNames

------------------------------------------------------------------
comma = T.pack ", \n"

mkSigName :: Sig -> J T.Text
mkSigName sig = do
  case sig of
    SigIndex name idx -> return $ T.pack $ format "{0}({1})" [name, show idx]
    SigSimple name -> do
      nb $ "mkSigName get a sig: " ++ show sig
      return $ T.pack name
    x -> unimplemented $ "Vhdl.mkSigName: " ++ show x

mkTermAssoc :: MT.TermAssoc -> J T.Text
mkTermAssoc (MT.TermAssoc dir src tgt) = do
  -- TermAssoc {taDir = In, taSrc = SigIndex "A" 0, taTgt = SigIndex "in1" 0}
  srcTxt <- mkSigName src
  tgtTxt <- mkSigName tgt
  case dir of 
    In -> return $ T.concat [ tgtTxt , T.pack " => " , srcTxt ]
    Out -> return $ T.concat [ srcTxt , T.pack " => " , tgtTxt ]

mkTermAssign :: MT.SigAssign -> J T.Text
mkTermAssign (MT.SigAssign src tgt) = do
  srcTxt <- mkSigName src
  tgtTxt <- mkSigName tgt
  return $ T.concat [ tgtTxt , T.pack " <= " , srcTxt ]

mkTermMap :: MT.TermMap -> J T.Text
mkTermMap xs = "Vhdl.mkTermMap" <? do
  ts <- mapM mkTermAssoc xs
  return $ T.concat $ DL.intersperse comma ts
    
mkPortMap :: [MT.TermMap] -> [MT.TermMap] -> J T.Text
mkPortMap ins outs = "Vhdl.mkPortMap" <? do
  portIns <- mapM mkTermMap ins
  portOuts <- mapM mkTermMap outs
  let f= T.intercalate comma
  return $ T.concat [ f portIns, comma, f portOuts ]
    
------------------------------------------------------------------
replicatedLabel subModName loc zIdx = 
  format "{0}_{1}_{2}" [ Module.mangleModName subModName
                       , take 5 $ hashid loc
                       , show zIdx ]

mkSubModuleInstance :: TopLevel -> String -> SubModule -> J T.Text
mkSubModuleInstance topl modname submod@(SubModule name loc) = do
  nb "Jade.Vhdl.mkSubModuleInstance"
  subModuleReps <- MT.subModuleInstances topl modname submod
  let repDepth = length subModuleReps

  let mkOneInstance submodrep@(MT.SubModuleRep ins outs _ zIdx) = do
        portmap <- mkPortMap ins outs
        let label = replicatedLabel name loc zIdx
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
  nb "These ins and outs aren't going to be SigConcats, ever."
  Inputs ins <- TopLevel.getInputs topl modname
  Outputs outs <- TopLevel.getOutputs topl modname
  ignore <- concatMapM Sig.getNames (ins ++ outs)
  
  comps <- TopLevel.components topl modname
  compNames <- mapM GComp.name comps
  
  let keepers = [comp | (n, comp) <- zip compNames comps, n `notElem` ignore]

  let f comp = do        
        n <- GComp.name comp
        w <- GComp.width comp
        let temp = "signal {0} : std_logic_vector({1} downto 0);"
        return $ format temp [n, show (w - 1)] -- one less because of zero indexing.
      
  if null keepers
    then return "-- no node decls"
    else do sigDecls <- mapM f keepers
            return $ concat $ DL.intersperse "\n" sigDecls

-- If output signals are not connected directly to a submodule output,
-- then there is no structural output to that output.
connectOutput :: TopLevel -> String -> Sig -> J T.Text
connectOutput topl modname outSig = "Jade.Vhdl.connectOutput" <? do
  assignMap <- MT.connectOneOutput topl modname outSig
  txts <- mapM mkTermAssign assignMap
  return $ T.concat [T.append t (T.pack ";\n") | t <- txts]

-- If input signals are not connected directly to a submodule output,
-- then there is no structural input for that input.
connectAllInputs topl modname inSigs = "Jade.Vhdl.connectAllInputs" <? do  
  assignMap <- concatMapM (MT.connectOneInput topl modname) inSigs
  txts <- mapM mkTermAssign assignMap
  return $ T.concat [T.append t (T.pack ";\n") | t <- txts]
  
connectConstant :: TopLevel -> String -> GComp -> J T.Text
connectConstant topl modname comp = "Jade.Vhdl.connectConstant" <? do
  assignMap <- MT.connectConstantComp topl modname comp
  txts <- mapM mkTermAssign assignMap
  return $ T.concat [T.append t (T.pack ";\n") | t <- txts]
