{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Jade.Vhdl where

import qualified Data.Map as DM
import qualified Language.VHDL.Syntax as S
import qualified Language.VHDL.Pretty as P
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode
import qualified Jade.Module as Module
import Control.Monad
import Jade.Util

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

shiftExpName s =
  let name = S.PrimName (S.NSimple $ S.Ident s)
      term = S.Term (S.FacPrim name Nothing) []
      simp = S.SimpleExpression Nothing term []
      shft = S.ShiftExpression simp Nothing
  in shft


mkProcStmt :: S.ProcessStatementPart -> S.ProcessStatement
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

mkCTestArchBody :: TopLevel -> String -> _ -> J S.ArchitectureBody
mkCTestArchBody topl modname stuff = "Jade.Vhdl.mkCTestArchBody" <? do
  -- architecture behaviour of AND23_TB is
  --  STUFF
  -- begin
  --  STUFF
  -- end behaviour;
  m <- TopLevel.getModule topl modname
  sigDecls <- mkCTestInputSignalDecls m
  dut <- mkDutInstance modname m
  let procStmt = S.ConProcess $ S.ProcessStatement Nothing False Nothing [] stuff
      archName = S.Ident "behaviour"
      entName = S.NSimple $ S.Ident $ (replace "/" "__" modname) ++ "_tb"
      archDeclPart = sigDecls  -- [] -- declare wires
      archStmtPart = [S.ConComponent dut] ++ [procStmt] -- the actual test cases.
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

conAssign :: Sig -> BinVal -> S.SequentialStatement
conAssign sig binval =
  case sig of
    SigSimple sigName ->
      let label = Nothing
          target = S.TargetName $ S.NSimple $ S.Ident sigName
          delay = Nothing
          waveform = S.WaveElem [S.WaveEExp (mkBinValExpr binval) Nothing]
          sas = S.SSignalAss $ S.SignalAssignmentStatement label target delay waveform
      in sas

-- these are kind of ridiculous.
mkBinValExpr binval =
  let c = case binval of
            H -> S.CLit '1'
            L -> S.CLit '0'
            Z -> S.CLit 'Z'
      charLit = S.EChar c
      enumLit = S.LitEnum charLit
      primLit = S.PrimLit enumLit
      factor = S.FacPrim primLit Nothing
      term = S.Term factor []
      simpleExpr = S.SimpleExpression Nothing term []
      shiftExpr = S.ShiftExpression simpleExpr Nothing
      relation = S.Relation shiftExpr Nothing
      expr = S.EAnd [relation]
  in expr

mkBinValShiftExpr binval =
  let c = case binval of
            H -> S.CLit '1'
            L -> S.CLit '0'
            Z -> S.CLit 'Z'
      charLit = S.EChar c
      enumLit = S.LitEnum charLit
      primLit = S.PrimLit enumLit
      factor = S.FacPrim primLit Nothing
      term = S.Term factor []
      simpleExpr = S.SimpleExpression Nothing term []
      shiftExpr = S.ShiftExpression simpleExpr Nothing
  in shiftExpr

mkBinValRelation binval =
  let c = case binval of
            H -> S.CLit '1'
            L -> S.CLit '0'
            Z -> S.CLit 'Z'
      charLit = S.EChar c
      enumLit = S.LitEnum charLit
      primLit = S.PrimLit enumLit
      factor = S.FacPrim primLit Nothing
      term = S.Term factor []
      simpleExpr = S.SimpleExpression Nothing term []
      shiftExpr = S.ShiftExpression simpleExpr Nothing
      relation = S.Relation shiftExpr Nothing
  in relation

litExpr lit = 
  let primLit = S.PrimLit lit
      factor = S.FacPrim primLit Nothing
      term = S.Term factor []
      simpleExpr = S.SimpleExpression Nothing term []
      shiftExpr = S.ShiftExpression simpleExpr Nothing
      relation = S.Relation shiftExpr Nothing
      expr = S.EAnd [relation]
  in expr

litRel lit = 
  let primLit = S.PrimLit lit
      factor = S.FacPrim primLit Nothing
      term = S.Term factor []
      simpleExpr = S.SimpleExpression Nothing term []
      shiftExpr = S.ShiftExpression simpleExpr Nothing
      relation = S.Relation shiftExpr Nothing
  in relation

litTerm lit = 
  let primLit = S.PrimLit lit
      factor = S.FacPrim primLit Nothing
      term = S.Term factor []
  in term

stringLitExpr s = litExpr $ S.LitString $ S.SLit s
stringLitRel s = litRel $ S.LitString $ S.SLit s
stringLitTerm s = litTerm $ S.LitString $ S.SLit s

funcCall :: String -> [String] -> S.Factor
funcCall name args =
  let fname = mkName name
      oneArg arg = S.AssociationElement Nothing (S.APDesignator $ S.ADVariable $ mkName arg)
  in S.FacPrim (S.PrimFun $ S.FunctionCall fname (Just $ S.AssociationList (map oneArg args))) Nothing

primToExpr p = S.EAnd [S.Relation
                       (S.ShiftExpression
                        (S.SimpleExpression Nothing
                         (S.Term (S.FacPrim p Nothing) []) []) Nothing) Nothing]

factorToShiftExpr f = (S.ShiftExpression (S.SimpleExpression Nothing (S.Term f []) []) Nothing)
simpleExprToExpr simple = S.EAnd [S.Relation (S.ShiftExpression simple Nothing) Nothing]



mkTestLine :: Module -> [Action] -> TestLine -> Integer -> J [S.SequentialStatement]
mkTestLine m [] _ _ = return [] -- base case, no more actions to consume
mkTestLine m (act:rest) testLine@(TestLine asserts samples comment) testnum =
  "Jade.Vhdl.mkTestLine: " <? do
  let recurse xs = liftM (xs ++) (mkTestLine m rest testLine testnum)
  
  case act of
    ------------------------------------------------------------------
    -- a <= '0'; b <= '0'; c <= '0'; d <= '0';
    Assert _ ->
      "while processing .assert action" <? do
      Inputs sigs <- Module.getInputs m

      when (length sigs /= length asserts) $
        let msg = "Can't render test case: {0}, the number of asserted signals: ({1}) \
                  \ does not match the number of values found in the testline: ({2})" 
        in die $ fmt msg (testnum, length sigs, length asserts)
              
      let stmts = zipWith conAssign sigs asserts
      recurse stmts

    ------------------------------------------------------------------
    s@(Sample _) ->
      ("While generating test case for sample: " ++ show s) <? do
      
      Outputs sigs <- Module.getOutputs m
      ("with expected results: " ++ (show $ zip sigs samples)) <? do
      
      let buildConditionalIfThen s@(sig, sample) = ("buildConditionalIfThen: " ++ (show s)) <? do
            sigName <- case sig of
                         SigSimple name -> return name
                         x -> die $ "unhandled signal type: " ++ show x
            
            let sif = S.SIf $ S.IfStatement Nothing thenClause [] elseClause          
                thenClause = (thenCond, thenStmts)
                thenCond = S.EAnd $ [S.Relation leftExpr $ Just (S.Neq, rightExpr)]  -- result /= expected
                leftExpr = shiftExpName sigName
                rightExpr = mkBinValShiftExpr sample -- expected
                report s = S.SReport $ S.ReportStatement Nothing (stringLitExpr s) Nothing
                expected got = S.SReport $ S.ReportStatement Nothing (S.EAnd [S.Relation got Nothing]) Nothing
                to_string = funcCall "to_string" [sigName]
                instead =  S.SReport $ S.ReportStatement Nothing insteadExpr Nothing
                termFunc = S.Term to_string []
                termString = stringLitTerm "got        : "
                insteadExpr = simpleExprToExpr $ S.SimpleExpression Nothing termString [(S.Concat, termFunc)]
                
                thenStmts = [ report $ "Test Number: " ++ show testnum ++ " fails."
                            , report $ "expecting  : " ++ (show $ P.pp (mkBinValShiftExpr sample))
                            , instead ]
                            
                elseClause = Just []
            return sif
            
      result <- mapM buildConditionalIfThen (zip sigs samples)
      recurse result

    ------------------------------------------------------------------
    Tran dur -> ("tran: " ++ show dur) <? do
      case dur of
        Nanosecond ns -> do
          -- wait for 99 ns;
          let timeoutClause = S.TimeoutClause $ expName (show ns ++ " ns")
              wait = S.WaitStatement Nothing Nothing Nothing (Just timeoutClause)
          recurse [S.SWait wait]
        Millisecond ms -> do
          let timeoutClause = S.TimeoutClause $ expName (show ms ++ " ms")
              wait = S.WaitStatement Nothing Nothing Nothing (Just timeoutClause)
          recurse [S.SWait wait]
      
    x -> (die $ show x) ? "Haven't implemented this for"
  
  


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
  tlines <- Module.testLines m
  (CycleLine actions) <- Module.cycleLine m 
  cases <- sequence [mkTestLine m actions testline testnum | (testline, testnum) <- zip tlines [1..]]
  arch <- mkCTestArchBody topl modname (concat cases)
 
  return $ do
    let newline = putStrLn ""
    newline
    print $ P.pp libSection
    newline
    print $ P.pp $ mkCTestEntityDecl modname m
    newline
    print $ P.pp arch
    newline
    --print $ P.pp $ mkProcStmt (concat cases)
    newline
    --xprint $ P.pp asdf
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
