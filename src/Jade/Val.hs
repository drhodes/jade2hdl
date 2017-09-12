module Jade.Val where

import qualified Text.Parsec.Number as TPN
import Text.Parsec
import Jade.Types
import qualified Numeric as N
import Text.Format
import Control.Monad
import Jade.Util
import Data.Char as DC



getName (ValIndex n _) = n
getName (Lit _) = ""


hasIdent (ValIndex n _) ident = n == ident
hasIdent _ _ = False

isLit (Lit _) = True
isLit _ = False
