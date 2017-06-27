{-# LANGUAGE FlexibleContexts #-}
module Jade.Vhdl where

import qualified Data.Map as DM
import qualified Language.VHDL.Syntax as S
import qualified Language.VHDL.Pretty as P
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import Control.Monad

tident :: String -> S.TypeMark
tident s = (S.TMType (S.NSimple (S.Ident s)))

mkSigDecl :: S.Mode -> Sig -> S.InterfaceDeclaration
mkSigDecl way (SigSimple n) =
  let ident = [S.Ident n]
      mode = Just way
      subtypeInd = S.SubtypeIndication Nothing (tident "std_logic") Nothing
      bus = False
      expr = Nothing
  in S.InterfaceSignalDeclaration ident mode subtypeInd bus expr
mkSigDecl _ _ = undefined

replace :: [Char] -> [Char] -> [Char] -> [Char]
replace c r xs = "mod" ++ (concat [if [x] == c then r else [x] | x <- xs])

mkPortClause' :: Inputs -> Outputs -> S.PortClause
mkPortClause' (Inputs ins) (Outputs outs) = 
  let inSigs = map (mkSigDecl S.In) ins
      outSigs = map (mkSigDecl S.Out) outs
  in S.PortClause $ S.InterfaceList (inSigs ++ outSigs) 

mkPortClause :: (String, Module) -> S.PortClause
mkPortClause (name, Module schem modTests _) = 
  let (Just ins) = join $ liftM modInputs modTests
      (Just outs) = join $ liftM modOutputs modTests
  in mkPortClause' ins outs

mkEntityDecl (name, m@(Module schem modTests _)) =
  let portClause = mkPortClause (name, m)
      entId = S.Ident (replace "/" "__" name)
      entHeader = S.EntityHeader Nothing (Just portClause)
      entDecl = []
      entStat = Nothing
  in S.EntityDeclaration entId entHeader entDecl entStat

mkArchBody :: ([Char], Module) -> S.ArchitectureBody
mkArchBody (name, m@(Module schem modTests _)) =
  let portClause = mkPortClause (name, m)
      archId = S.Ident "func"
      entName = S.NSimple $ S.Ident (replace "/" "__" name)
      label = Nothing

      sas = S.SSignalAss $ S.SignalAssignmentStatement
        label
        (S.TargetName $ S.NSimple $ S.Ident "asdf")
        Nothing
        (S.WaveElem [S.WaveEExp (expName "something") Nothing])

      stmts = [S.ConProcess (S.ProcessStatement Nothing False Nothing [] [sas, sas, sas])]
      
  in S.ArchitectureBody archId entName [] stmts

expName :: String -> S.Expression
expName s =
  let name = S.PrimName (S.NSimple $ S.Ident s)
      term = S.Term (S.FacPrim name Nothing) []
      simp = S.SimpleExpression Nothing term []
      shft = S.ShiftExpression simp Nothing
      rel = S.Relation shft Nothing
      exp = S.EAnd [rel]
  in exp

mkProcStmt stmts = S.ProcessStatement Nothing False Nothing [] stmts

------------------------------------------------------------------
-- BUILTINS

-- | create an AND2 module usable by other modules.

relation relname =
  let name = S.PrimName (S.NSimple $ S.Ident relname)
      term = S.Term (S.FacPrim name Nothing) []
      simp = S.SimpleExpression Nothing term []
      shft = S.ShiftExpression simp Nothing
      rel = S.Relation shft Nothing
  in rel

builtInExpr constructor in1 in2 = constructor [ relation in1
                                              , relation in2 ]

mkBuiltInArchBody :: String -> ([S.Relation] -> S.Expression) -> S.ArchitectureBody
mkBuiltInArchBody name operator = 
  let ins = Inputs [ SigSimple "in1", SigSimple "in2" ]
      outs = Outputs [ SigSimple "out1" ]
      portClause = mkPortClause' ins outs
      archId = S.Ident "Behavioral"
      entName = S.NSimple $ S.Ident name
      label = Nothing

      sas = S.SSignalAss $ S.SignalAssignmentStatement
        label
        (S.TargetName $ S.NSimple $ S.Ident "out1")
        Nothing
        (S.WaveElem [S.WaveEExp (builtInExpr operator "in1" "in2") Nothing])
      stmts = [S.ConProcess (S.ProcessStatement Nothing False Nothing [] [sas])]
      
  in S.ArchitectureBody archId entName [] stmts

mkBuiltInEntityDecl :: String -> S.EntityDeclaration
mkBuiltInEntityDecl name = 
  let ins = Inputs [ SigSimple "in1", SigSimple "in2" ]
      outs = Outputs [ SigSimple "out1" ]
      portClause = mkPortClause' ins outs
      entId = S.Ident name
      entHeader = S.EntityHeader Nothing (Just portClause)
      entDecl = []
      entStat = Nothing
  in S.EntityDeclaration entId entHeader entDecl entStat

library s = S.ContextLibrary $ S.LibraryClause $ S.LogicalNameList [S.Ident s]
use name =
  let prefix = S.PName $ S.NSimple $ S.Ident name
  in S.ContextUse $ S.UseClause [S.SelectedName prefix S.SAll]

--suffix = S.SSimple $ S.
ctxClause = S.ContextClause
------------------------------------------------------------------
-- COMBINATIONAL TEST GENERATION.

-- entity AND23_TB is end AND23_TB;

mkCTestEntityDecl modname m@(Module schem modTests _) =
  let name = modname ++ "_tb"
      portClause = mkPortClause (name, m)
      entId = S.Ident (replace "/" "__" name)
      entHeader = S.EntityHeader Nothing Nothing
      entDecl = []
      entStat = Nothing
  in S.EntityDeclaration entId entHeader entDecl entStat

  -- for every input in the whosie whatsie get declare a signal.
  -- signal testnum : unsigned(7 downto 0) := x"00";
  -- signal a, b, c, d, result : std_logic;
  
mkCTestInputSignalDecls m@(Module schem modTests _) =
  "Jade.Vhdl.mkCTestInputSignalDecls" <?
  do Inputs inSigs <- Module.getInputs m
     Outputs outSigs <- Module.getOutputs m
  
     let subTypeInd = S.SubtypeIndication Nothing (tident "std_logic") Nothing
  
     forM (inSigs ++ outSigs) $ \sig ->
       case sig of
         SigSimple name ->
           do let sigDecl = S.SignalDeclaration [S.Ident name] subTypeInd Nothing Nothing
              return $ S.BDISignal sigDecl
         x -> die $ show x ++ " hasn't been implemented to declare signal yet."


mkCTestArchBody topl modname = "Jade.Vhdl.mkCTestArchBody" <? do
  -- architecture behaviour of AND23_TB is
  --  STUFF
  -- begin
  --  STUFF
  -- end behaviour;
  m <- TopLevel.getModule topl modname
  sigDecls <- mkCTestInputSignalDecls m
  dut <- mkDutInstance modname m
  
  let archName = S.Ident "behaviour"
      entName = S.NSimple $ S.Ident $ (replace "/" "__" modname) ++ "_tb"
      archDeclPart = sigDecls  -- [] -- declare wires
      archStmtPart = [S.ConComponent dut] -- the actual test cases.
      
  return $ S.ArchitectureBody archName entName archDeclPart archStmtPart


tbName modname = (replace "/" "__" modname) ++ "_tb"


mkName s = S.NSimple $ S.Ident s

-- dut : entity work.AND23 port map (a => a, b => b, c => c, d => d, output => result);
mkDutInstance modname m = "Jade.Vhdl.mkDutInstance" <? do
  Inputs inSigs   <- Module.getInputs m
  Outputs outSigs <- Module.getOutputs m

  let toAssoc sig =
        case sig of
          SigSimple name ->
            let formalPart = Just $ S.FPDesignator $ S.FDPort $ mkName name
                actualPart = S.APDesignator $ S.ADSignal $ mkName name
            in return $ S.AssociationElement formalPart actualPart
          x -> die $ "have yet to implement toAssoc for this signal type: " ++ show x
          
  assocElems <- mapM toAssoc (inSigs ++ outSigs) ? "creating associations list for port map"
                
  let portMap = S.PortMapAspect portAssocList
      portAssocList = S.AssociationList assocElems
      label = S.Ident "dut"
      unit = S.IUEntity (mkName $ "work." ++ (tbName modname)) Nothing
      
  return $ S.ComponentInstantiationStatement label unit Nothing (Just portMap)


mkCombinationalAction :: Action -> J (Maybe S.SequentialStatement)
mkCombinationalAction action = "Jade.Vhdl.mkCombinationalAction" <? do
  case action of
    Assert groupInputs -> "assert .group inputs" <? do      
      return Nothing
    Sample groupOutputs -> "assert .group outputs" <? do
      return Nothing
    Tran dur -> ("tran: " ++ show dur) <? do
      case dur of
        Nanosecond ns -> do
          -- wait for 99 ns;
          let timeoutClause = S.TimeoutClause $ expName (show ns ++ " ns")
              wait = S.WaitStatement Nothing Nothing Nothing (Just timeoutClause)
          return $ Just $ S.SWait wait          
        Millisecond ms -> do
          let timeoutClause = S.TimeoutClause $ expName (show ms ++ " ms")
              wait = S.WaitStatement Nothing Nothing Nothing (Just timeoutClause)
          return $ Just $ S.SWait wait          
    x -> die $ "Unhandled action: " ++ show x

{-
mkTestLine testLine@(TestLine asserts samples comment) testnum =
  "Jade.Vhdl.mkTestLine" <? do
  -- a <= '0'; b <= '0'; c <= '0'; d <= '0';
  -- wait for 99.0 ns;
  -- if result = '1' then
  --   report "TestNum 1";
  --   report "expecting: result = 0";
  --   report "got      : result = " & to_string(result);
  --   stop(-1);
  -- else
  --   write(OUTPUT, "TEST 1: PASSED" & LF);
  -- end if;
  -- wait for 1 ns;
-}
  
  
  
  


mkCombinationalTest topl modname =
  ("Jade.Vhdl.mkCombinationalTest: " ++ modname) <? do
  let libSection = ctxClause [ library "STD"
                             , use "STD.textio"
                             , use "STD.env"
                             , library "IEEE"
                             , use "IEEE.std_logic_1164"
                             , use "IEEE.std_logic_textio"
                             , use "ieee.numeric_std"
                             ]
                   
  m <- TopLevel.getModule topl modname
  arch <- mkCTestArchBody topl modname

  (CycleLine actions) <- Module.cycleLine m 
  acts <- mapM mkCombinationalAction actions

  

  
  return $ do
    let newline = putStrLn ""
    newline
    print $ P.pp libSection
    newline
    print $ P.pp $ mkCTestEntityDecl modname m
    newline
    print $ P.pp arch
    newline
    print $ P.pp $ mkProcStmt []
    newline
    print $ P.pp acts
  {-
  process
  begin
    a <= '0'; b <= '0'; c <= '0'; d <= '0';
    wait for 99 ns;
    if result = '1' then
      report "TestNum 1";
      report "expecting: result = 0";
      report "got      : result = " & to_string(result);
      stop(-1);
    else
      write(OUTPUT, "TEST 1: PASSED" & LF);
    end if;
    wait for 1 ns;

    
    wait;
  end process;    
end behaviour;



-}










mkTestNum =
  let subTypeInd = S.SubtypeIndication Nothing (tident "integer") Nothing
      sigDecl = S.SignalDeclaration [S.Ident "testnum"] subTypeInd Nothing Nothing
  in S.BDISignal sigDecl
