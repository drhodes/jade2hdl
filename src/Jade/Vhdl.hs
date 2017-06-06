module Jade.Vhdl where

import qualified Language.VHDL.Syntax as S
import qualified Language.VHDL.Pretty as P
import Jade.Types
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode as Decode

tident :: String -> S.TypeMark
tident s = (S.TMType (S.NSimple (S.Ident s)))

asdf = let ident = [S.Ident "alu"]
           mode = Just S.In
           subtypeInd = S.SubtypeIndication Nothing (tident "std_logic") Nothing
           bus = False
           expr = Nothing
       in S.InterfaceSignalDeclaration ident mode subtypeInd bus expr

mkSigDecl way (SigSimple n) =
  let ident = [S.Ident n]
      mode = Just way
      subtypeInd = S.SubtypeIndication Nothing (tident "std_logic") Nothing
      bus = False
      expr = Nothing
  in S.InterfaceSignalDeclaration ident mode subtypeInd bus expr
mkSigDecl _ _ = undefined


-- JADE module -> VHDL module.
-- the module ports will be determined by JADE .group directives.
-- builtin jade modules need to be a thing.

replace c r xs = "mod" ++ (concat [if [x] == c then r else [x] | x <- xs])

translateMod (name, Module schem modTests) = do
  -- get the inputs.
  -- get the modules name
  print (replace "/" "__" name)
  let (Just (Inputs ins)) = modInputs modTests
  let (Just (Outputs outs)) = modOutputs modTests
  let inSigs = map (mkSigDecl S.In) ins
      outSigs = map (mkSigDecl S.Out) outs
  print $ P.pp $ S.PortClause $ S.InterfaceList (inSigs ++ outSigs) --
  return ()
  
foo = do
  topl <- Decode.decodeTopLevel "./test-data/and2.json"
  case topl of
    (Right topl) -> do
      let mods = TopLevel.modules topl

      -- emit the port section
      mapM_ translateMod mods
      
    (Left msg) -> fail msg





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

