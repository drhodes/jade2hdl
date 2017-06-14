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

mkPortClause :: (String, Module) -> S.PortClause
mkPortClause (name, Module schem modTests) = 
  let (Just (Inputs ins)) = join $ liftM modInputs modTests
      (Just (Outputs outs)) = join $ liftM modOutputs modTests
      inSigs = map (mkSigDecl S.In) ins
      outSigs = map (mkSigDecl S.Out) outs
  in S.PortClause $ S.InterfaceList (inSigs ++ outSigs) --

mkEntityDecl (name, m@(Module schem modTests)) =
  let portClause = mkPortClause (name, m)
      entId = S.Ident (replace "/" "__" name)
      entHeader = S.EntityHeader Nothing (Just portClause)
      entDecl = []
      entStat = Nothing
  in S.EntityDeclaration entId entHeader entDecl entStat

-- ConProcess   ProcessStatement

--FUNCTIONAL DESCRIPTION: how the AND Gate works -- architecture func of andGate is
-- begin
--   F <= A and B;
-- end func;

-- data SignalAssignmentStatement = SignalAssignmentStatement
--       (Maybe Label) Target (Maybe DelayMechanism) Waveform
--   deriving (Eq, Show)

mkArchBody :: ([Char], Module) -> S.ArchitectureBody
mkArchBody (name, m@(Module schem modTests)) =
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
      -- archDecls = nuthin. [].
      
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




-- translateSchem (name, Module schem modTests) = do
--   -- calculate the value of each node.
--   -- start from an output
--   -- 
--   print name
--   print schem
--   -- find the output nodes.
  
--   -- work back
--   return ()


{-
inputs
outputs

the directions of them
node identity
directed connections
undirected connections
how to think about that

JADE doesn't have a notion of type, this is great, every thing is a
bit.  emit one large vhdl file? That sounds tempting for a couple
reasons, it's easier to work with, people shouldn't be editing the
emitted source anyways.  
-}

