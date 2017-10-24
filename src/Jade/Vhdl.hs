{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Jade.Vhdl where
import Control.Monad
import Data.FileEmbed
import Data.Text.Encoding
import Data.Maybe
import Text.Format
import Text.Mustache
import Text.Mustache.Compile (mustache)
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Text as T
import qualified Data.Text.IO as DT
import qualified Jade.Decode.Decode as Decode
import qualified Jade.Decode.Sig as Sig
import qualified Jade.Net as Net
import qualified Jade.Decode.ModTest as ModTest
import qualified Jade.Module as Module
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Signal as Signal
import qualified Jade.Bundle as Bundle
import qualified Data.ByteString.Lazy.Char8 as DBL8
import Data.Aeson
import Jade.Middle.Types as JMT
import Jade.Common
import qualified Jade.Middle.Middle as JMM
import qualified Text.PrettyPrint.Leijen as P
import Data.Maybe

splitAssert _ [] = []
splitAssert [] xs = [xs]
splitAssert widths xs =
  let n = fromIntegral $ head widths
  in (take n xs) : (splitAssert (tail widths) (drop n xs))

mkTestLine :: Module -> [Action] -> TestLine -> Integer -> J [String]
mkTestLine _ [] _ _ = return []
mkTestLine m (act:actions) testline testnum = "Vhdl.mkTestLine" <? do  
  let recurse x = (x++) <$> mkTestLine m actions testline testnum
  let Just modt = moduleTest m
  
  case act of
    Tran dur ->
      case dur of
        Nanosecond ns -> recurse $ [format "wait for {0} ns;" [show ns]]
        Millisecond ms -> recurse $ [format "wait for {0} ms;" [show ms]]
        
    Assert _ -> "Assert" <? do
      mkAssert m modt testline >>= recurse 
      
    Sample _ -> "Samples" <? do
      txt <- mkSampleLine testnum modt testline m
      recurse $ map T.unpack txt

    SetSignal _ x -> case x of
      0.0 -> recurse [format "{0} <= {1};" ["CLK", quote "0"]];
      1.0 -> recurse [format "{0} <= {1};" ["CLK", quote "1"]];
      otherwise -> die $ "SetSignal needs to be 1 or 0, got: " ++ (show x)
    x -> dief "not unimplemented {0}" [show x]

mkAssert :: Module -> ModTest -> TestLine -> J [String]
mkAssert m modt testline = "Vhdl.mkAssert" <? do
  -- a <= '0'; b <= '0'; c <= '0'; d <= '0';
  asserts <- ModTest.assertBitVals modt testline
  Inputs sigs <- Module.getInputs m
  let tgts = concatMap JMM.explode sigs
  if (length tgts == length tgts)
    then return $ map sigAssert (zip tgts asserts)
    else die "expecteds not the same length as lits."

sigAssert (ValIndex name idx, assert) = 
  format "{0}({1}) <= '{2}';" [name, show idx, [binValToChar assert]]

mkSampleLine :: Integer -> ModTest -> TestLine -> Module -> J [T.Text]
mkSampleLine testnum modt testline m = "Vhdl.mkSampleLine" <? do
  expecteds <- ModTest.sampleBitVals modt testline
  Outputs sigs <- Module.getOutputs m
  let singles = concatMap JMM.explode sigs
  if (length expecteds == length singles)
    then mapM (mkSamplePair testnum modt testline m) (zip singles expecteds)
    else die "expecteds not the same length as singles."

mkSamplePair testnum modt testline m (ValIndex name idx, exp) = "Vhdl.mkSamplePair" <? do
  let n = format "{0}({1})" [name, show idx]
      comment = ModTest.getTestlineComment testline
  testCaseIfBlock testnum n ['\'', binValToChar exp, '\''] comment

testCaseIfBlock :: Integer -> String -> String -> String -> J T.Text
testCaseIfBlock testnum signal expected comment = "Vhdl.testCaseIfBlock" <? do
  let txt = decodeUtf8 $(embedFile "app-data/vhdl/template/test-case-if-block.mustache")
      Right temp = compileTemplate "test-case-if-block" txt
      to_string = format "to_string({0})" [signal]
      mapping = DM.fromList [ ("testnum", toMustache testnum)
                            , ("signal", toMustache signal)
                            , ("expected", toMustache expected)
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
portAssoc sig = do
  name <- case sig of 
    SigSimple name -> return name 
    SigIndex name x -> return name 
    SigHash name x -> return name 
    SigRange name from to -> return name 
    SigRangeStep name from to step -> return name 
    SigQuote _ _ -> die "SigQuote isn't handled for test sig decl, because test inputs can't be SigQuotes"
    SigConcat sigs -> impossible "Vhdl.PortAssoc can't take a SigConcat, these are module transputs"
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



mkTestSigDecl (SigConcat sigs) = DL.intercalate "\n" <$> (mapM mkTestSigDecl sigs)
mkTestSigDecl sig = "Vhdl.mkSigDecl" <? do
  name <- case sig of 
    SigSimple name -> return name 
    SigIndex name x -> return name 
    SigHash name x -> return name 
    SigRange name from to -> return name 
    SigRangeStep name from to step -> return name 
    SigQuote _ _ -> die "SigQuote isn't handled for test sig decl, because test inputs can't be SigQuotes"
    SigConcat sigs -> impossible "This case should be handled in it's own funcation match"
  
  let width = Sig.width sig
      initialVal = quote $ take (fromIntegral width) (repeat 'U')
      fmtargs = [name, show (width - 1), initialVal]
  return $ format "signal {0}: std_logic_vector({1} downto 0) := {2};" fmtargs

mkSignalDecls m modname = "Vhdl.mkSignalDecls" <? do
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m
  let sigs = ins ++ outs
  if null sigs
    then return $ "-- no signal decls"
    else do sigDecls <- mapM mkTestSigDecl sigs
            return $ DL.intercalate "\n" sigDecls



--mkTestBench _ = return (T.pack "mkTestBench unimplemented")   
mkTestBench modname = "Jade.Vhdl.mkTestBench" <? do
  m <- getModule modname
  sigDecls <- mkSignalDecls m modname :: J String
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

mkCombinationalTest modname = "Vhdl.mkCombinationalTest" <? mkTestBench modname

portname sig = "Vhdl.portname" <? do
  case sig of
    SigSimple n -> return n
    SigIndex n _ -> return n
    SigRange n from to -> return n
    x -> die $ "portname doesn't handle signal: " ++ (show x)

------------------------------------------------------------------
--genPort dir (SigConcat _) = die "Vhdl.mkModule/genPort doesn't support SigConcat"
genPort dir sig = "Vhdl.genPort" <? do
  let width = fromIntegral $ Sig.width sig -- todo change this to return Int instead of Integer. 
  name <- portname sig
  return $ format "{0}: {1} std_logic_vector({2} downto 0)" [ name, dir, show $ width-1 ]

genPorts ins outs = "Vhdl.genPorts" <? do
  portIns <- mapM (genPort "in") ins
  portOuts <- mapM (genPort "out") outs
  return $ T.pack $ DL.intercalate ";\n" (concat [portIns, portOuts])

mkModule modname = "Vhdl.mkModule" <? do
  m <- getModule modname
  schem <- Module.getSchematic m
  subs <- TopLevel.getSubModules modname
  instances <- mapM (mkSubModuleInstance modname) subs
  
  Inputs ins <- Module.getInputs m
  Outputs outs <- Module.getOutputs m
  ports <- genPorts ins outs

  internalNodeDecls <- mkInternalNodeDecls modname
  
  let txt = decodeUtf8 $(embedFile "app-data/vhdl/template/combinational-module.mustache")
      Right temp = compileTemplate "combinational-module.mustache" txt
      mapping = DM.fromList
        [ ("module-name", toMustache (Module.mangleModName modname))
        , ("ports", toMustache ports)
        --, ("node-declarations", toMustache (nodeDecls)) 
        , ("internal-node-declarations", toMustache internalNodeDecls)
        , ("submodule-entity-instances", toMustache  (T.intercalate  (T.pack "\n") instances))
        -- , ("maybe-wire-input", toMustache inputWires)
        -- , ("maybe-internal-assignments", toMustache (internalAssignments :: T.Text))
        -- , ("maybe-wire-constants", toMustache constantWires)
        -- , ("maybe-wire-output", toMustache outputWires)
        ]
  return $ substitute temp mapping

mkAllMods modname = "Vhdl.mkAllMods" <? do
  userModNames <- TopLevel.dependencyOrder modname
  T.concat <$> mapM mkModule (userModNames ++ [modname])

mkVectorDecl name width = 
  T.pack $ format "signal {0}: std_logic_vector({1} downto 0);" [name, show $ width - 1]



  
mkInternalNodeDecls modname = "Vhdl.mkInternalNodeDecks" <? do
  -- get the module input and output names.
  inOutNames <- Module.getInputOutputNames =<< getModule modname
  -- figure out the names of all the node signals in the module.
  signals <- TopLevel.getSignals <$> TopLevel.getAllPartsNoTerms modname
  let sigs = mapMaybe Signal.getSig signals

  -- GOOD PLACE TO STOP, For tomorrow, need to create a local function
  -- in here getSigName that works.  Module.getSigName doesn't 
  
  names <- mapM Module.getSigName sigs
  
  let mkSig sig =
        do name <- Module.getSigName sig
           if name `elem` inOutNames
             then return Nothing
             else return $ Just $ mkVectorDecl name (Sig.width sig)
             
  T.intercalate (T.pack "\n") <$> catMaybes <$> mapM mkSig (DL.nub sigs)
  
  -- remove the input and output names, because they are already
  -- declared in the module entity
  -- format the keepers into VHDL signal declaration names.

-- mkNodeDecls modname = "Jade.Vhdl.mkNodeDecls" <? do
--   ignore <- Module.getInputOutputNames <$> getModule modname
  
--   let netIds = map netId nets
--       keepers = [net | (n, net) <- zip netIds nets, (format "net{0}" [show n]) `notElem` ignore]
      
--   if null (keepers :: [Net])
--     then return "-- no node decls"
--     else do sigDecls <- mapM mkNetName keepers
--             return $ concat $ DL.intersperse "\n" sigDecls

------------------------------------------------------------------
comma = T.pack ", \n"

mkValName :: Val -> J T.Text
mkValName val = "Vhdl.mkValName" <? do
  let p x = return $ T.pack x
  case val of
    ValIndex name idx -> p $ format "{0}({1})" [name, show idx]
    Lit H -> p "'1'"
    Lit L -> p "'0'"
    Lit Z -> p "'Z'"
    NetIndex netid idx -> p $ format "net{0}({1})" [show netid, show idx]

mkTermAssoc :: JMT.TermAssoc -> J T.Text
mkTermAssoc (JMT.TermAssoc dir src tgt) = "Vhdl.mkTermAssoc" <? do
  -- TermAssoc {taDir = In, taSrc = SigIndex "A" 0, taTgt = SigIndex "in1" 0}
  srcTxt <- mkValName src
  tgtTxt <- mkValName tgt
  case dir of 
    In -> return $ T.concat [ tgtTxt , T.pack " => " , srcTxt ]
    Out -> return $ T.concat [ srcTxt , T.pack " => " , tgtTxt ]
  
mkTermAssign :: JMT.ValAssign -> J T.Text
mkTermAssign (JMT.ValAssign src tgt) = "Vhdl.mkTermAssign" <? do
  srcTxt <- mkValName src
  tgtTxt <- mkValName tgt
  return $ T.concat [ tgtTxt , T.pack " <= " , srcTxt ]

mkValAssign :: JMT.ValAssign -> J T.Text
mkValAssign (JMT.ValAssign src tgt) = "Vhdl.mkValAssign" <? do
  srcTxt <- mkValName src
  tgtTxt <- mkValName tgt
  return $ T.concat [ tgtTxt , T.pack " <= " , srcTxt ]

mkTermMap :: JMT.TermMap -> J T.Text
mkTermMap xs = "Vhdl.mkTermMap" <? do
  ts <- mapM mkTermAssoc xs
  return $ T.concat $ DL.intersperse comma ts
    
mkPortMap :: [JMT.TermMap] -> [JMT.TermMap] -> J T.Text
mkPortMap ins outs = "Vhdl.mkPortMap" <? do
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
  "Jade.Vhdl.mkSubModuleInstance" <? do
  subModuleReps <- JMM.subModuleInstances modname submod

  let mkOneInstance submodrep@(JMT.SubModuleRep ins outs _ zIdx) = do
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
  T.intercalate (T.pack "\n") <$> mapM mkOneInstance subModuleReps

{- the memory mod, todo  
mkSubModuleInstance modname mem@(SubMemUnit memunit) = "Jade.Vhdl.mkSubModuleInstance" <? do
  JMM.SubModuleRep ins outs _ zIdx <- JMM.memUnitInstance modname memunit

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
-}

{-
  
------------------------------------------------------------------
mkNetName net = "Vhdl.mkNetName" <? do        
  let n = netId net -- get the net name
  w <- Net.width net -- get the width
  let temp = "signal net{0} : std_logic_vector({1} downto 0);"
  return $ format temp [show n, show (w - 1)] -- one less because of zero indexing.

-- get all node names needed for wiring.
-- no input names.

            
declareSigName :: String -> String -> J String
declareSigName modname signame = "Vhdl.declareSigName" <? do
  width <- TopLevel.getWidthOfValName modname signame
  let temp = "signal {0} : std_logic_vector({1} downto 0);"
  return $ format temp [signame, show (width - 1)] -- one less because of zero indexing.


-----------------------------------------------------------------------------
declareInternalSigNames :: String -> J [String]
declareInternalSigNames modname = "Vhdl.declareInternalSigNames" <? do
  signames <- TopLevel.getInternalSigNames modname
  --when (modname `contains` "Rep1FA2") (enb signames >> unimplemented)
  mapM (declareSigName modname) signames

-----------------------------------------------------------------------------

-- | required inference.  A net can only be an input net or an output net.

assignInternalSigNames modname subs inputAssignments = "Vhdl.assignInternalSigNames" <? do
  internalNames <- TopLevel.getInternalSigNames modname
  if True -- modname `contains` "Rep1FA2"
    then do return ()
            reps <- concatMapM (JMM.subModuleInstances modname) subs
            netsIn <- DL.nub <$> concatMapM JMM.getAllInputNetIdsFromRep reps
            netsOut <- DL.nub <$> concatMapM JMM.getAllOutputNetIdsFromRep reps

            -- remove the intersection from both.
            let netsIn' = netsIn DL.\\ netsOut
                netsOut' = netsOut --DL.\\ netsIn
            -- subtract from all internal sigs.
            -- assign these as outputs.
            -- what remains are inputs, and assign those as such.
    
            outs <- concatMapM (assignOneInternalSigName' modname Out netsOut') internalNames
            ins <- concatMapM (assignOneInternalSigName' modname In netsIn') internalNames
            
            let outs' = uniqueTargets outs Out
                ins' = uniqueTargets (ins ++ inputAssignments) In
            outs'' <- withSemiColonNewLines <$> mapM mkValAssign outs'
            ins'' <- withSemiColonNewLines <$> mapM mkValAssign ins'
            
            return $ T.concat [ T.pack "\n"
                              , T.pack "-- inputs\n"
                              , ins''
                              , T.pack "\n"
                              , T.pack "--outputs\n"
                              , outs'']
    
    else return (T.pack "// the universe has halted.")


uniqueTargets keepers dir = case dir of
                  Out -> let pairs = [(tgt, net) | ta@(JMM.ValAssign net tgt) <- keepers]
                             m = DM.fromList pairs -- this wipes out the repeats.
                             pairs' = [JMM.ValAssign net tgt | (tgt, net) <- DM.toList m]
                         in pairs'
                  In -> let pairs = [(net, tgt) | ta@(JMM.ValAssign tgt net) <- keepers]
                            m = DM.fromList pairs -- this wipes out the repeats.
                            pairs' = [JMM.ValAssign tgt net | (net, tgt) <- DM.toList m]
                        in pairs'


assignOneInternalSigName' modname direction netids signame  = "Vhdl.assignOneInternalSigName" <? do
  idxs <- TopLevel.getAllIndexesWithName modname signame
  assignInternalBundles' modname [Bundle idxs] direction netids 

-- assignInternalBundles :: String -> [ValBundle] -> Direction -> [NetId] -> J T.Text
-- assignInternalBundles modname bundle dir netids = "Vhdl.assignInternalBundles" <? do
--   assignMap <- concatMapM (JMM.assignBundle dir modname) bundle
--   let keepers = case dir of
--                   Out -> [ ta | ta@(JMM.ValAssign (NetIndex nid _) _) <- assignMap, nid `elem` netids]
--                   In  -> [ ta | ta@(JMM.ValAssign _ (NetIndex nid _)) <- assignMap, nid `elem` netids]

--   -- jumpers associate different names for the same net, so may
--   -- introduce multiple assignments to the same net.  make sure that
--   -- each net has only one assignment
--   withSemiColonNewLines <$> mapM mkTermAssign uniqueTargets

assignInternalBundles' modname bundle dir netids = "Vhdl.assignInternalBundles" <? do
  assignMap <- concatMapM (JMM.assignBundle dir modname) bundle
  let keepers = case dir of
                  Out -> [ ta | ta@(JMM.ValAssign (NetIndex nid _) _) <- assignMap, nid `elem` netids]
                  In  -> [ ta | ta@(JMM.ValAssign _ (NetIndex nid _)) <- assignMap, nid `elem` netids]

  -- jumpers associate different names for the same net, so may
  -- introduce multiple assignments to the same net.  make sure that
  -- each net has only one assignment

  let uniqueTargets = case dir of
        Out -> let pairs = [(tgt, net) | ta@(JMM.ValAssign net tgt) <- keepers]
                   m = DM.fromList pairs -- this wipes out the repeats.
                   pairs' = [JMM.ValAssign net tgt | (tgt, net) <- DM.toList m]
               in pairs'
        In -> let pairs = [(net, tgt) | ta@(JMM.ValAssign tgt net) <- keepers]
                  m = DM.fromList pairs -- this wipes out the repeats.
                  pairs' = [JMM.ValAssign tgt net | (net, tgt) <- DM.toList m]
              in pairs'
  return uniqueTargets

withSemiColonNewLines txts = T.concat [T.append t (T.pack ";\n") | t <- txts]


-- If input signals are not connected directly to a submodule output,
-- then there is no structural input for that input.
assignAllInputs :: String -> [ValBundle] -> J T.Text
assignAllInputs modname modInputBundles = "Vhdl.assignAllInputs" <? do
  assignMap <- concatMapM (JMM.assignBundle In modname) modInputBundles
  withSemiColonNewLines <$> mapM mkTermAssign assignMap
  
connectConstant :: String -> Net -> J T.Text
connectConstant modname net = "Vhdl.connectConstant" <? do
  assignMap <- JMM.assignConstantNet net
  withSemiColonNewLines <$> mapM mkTermAssign assignMap

assignAllInputs' :: String -> [ValBundle] -> J [JMM.ValAssign]
assignAllInputs' modname modInputBundles = "Vhdl.assignAllInputs" <? do
  concatMapM (JMM.assignBundle In modname) modInputBundles

assignAllOutputs' :: String -> [ValBundle] -> J [JMM.ValAssign]
assignAllOutputs' modname modInputBundles = "Vhdl.assignAllOutputs" <? do
  concatMapM (JMM.assignBundle Out modname) modInputBundles
  
-}

{-
-- If output signals are not connected directly to a submodule output,
-- then there is no structural output to that output.
connectOutput modname outBundle = "Vhdl.connectOutput" <? do
  assignMap <- JMM.assignBundle Out modname outBundle
  withSemiColonNewLines <$> mapM mkTermAssign assignMap
-}
