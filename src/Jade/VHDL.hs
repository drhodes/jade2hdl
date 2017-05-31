module Jade.VHDL where

import qualified Language.VHDL.Syntax as L
import qualified Jade.Decode as D
import qualified Data.Map as DM


emit (D.TopLevel modmap) =
  let pairs = DM.toList modmap
  in pairs




-- temp :: L.ArchitectureDeclarativePart -> L.ArchitectureStatementPart -> L.ArchitectureBody
-- temp = let L.ArchitectureBody (L.Ident "asdf") (L.NSimple (L.Ident "zxcv"))

