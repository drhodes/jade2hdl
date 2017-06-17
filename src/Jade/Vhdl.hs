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





-- mkBuiltInArchBody name = 
--   let portClause = mkPortClause (name, m)
--       archId = S.Ident "func"
--       entName = S.NSimple $ S.Ident (replace "/" "__" name)
--       label = Nothing

--       sas = S.SSignalAss $ S.SignalAssignmentStatement
--         label
--         (S.TargetName $ S.NSimple $ S.Ident "asdf")
--         Nothing
--         (S.WaveElem [S.WaveEExp (expName "something") Nothing])

--       stmts = [S.ConProcess (S.ProcessStatement Nothing False Nothing [] [sas, sas, sas])]
      
--   in S.ArchitectureBody archId entName [] stmts


-- structural VHDL
-- u1 : reg1 PORT MAP(d=>d0,clk=>clk,q=>q0);
-- mkComp :: TopLevel -> ModuleName -> S.

-- mkCompDecl topl modname =
--   let mod = TopLevel.getModule topl modname



-- create a built in module for 
-- mkBuiltIn name in1 in2 out = do
  
