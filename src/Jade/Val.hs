module Jade.Val ( getName
                , hasIdent
                , isLit
                ) where

import Jade.Types

getName (ValIndex n _) = n
getName (Lit _) = ""

hasIdent (ValIndex n _) ident = n == ident
hasIdent _ _ = False

isLit (Lit _) = True
isLit _ = False
