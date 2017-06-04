module Jade.Vhdl where

import qualified Language.VHDL.Syntax as S
import qualified Language.VHDL.Pretty as P


temp = S.PortClause $ S.InterfaceList [asdf, asdf, asdf] --

-- data SubtypeIndication = SubtypeIndication {
--     si_resolution_function_name :: Maybe Name
--   , si_type_mark                :: TypeMark
--   , si_constraint               :: Maybe Constraint
--   }

name :: String -> S.TypeMark
name s = (S.TMType (S.NSimple (S.Ident s)))

asdf = let ident = [S.Ident "alu"]
           mode = Just S.In
           subtypeInd = S.SubtypeIndication Nothing (name "std_logic") Nothing
           bus = False
           expr = Nothing
       in S.InterfaceSignalDeclaration ident mode subtypeInd bus expr
                                    
