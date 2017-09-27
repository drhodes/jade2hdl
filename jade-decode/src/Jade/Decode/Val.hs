module Jade.Decode.Val ( getName
                       , hasIdent
                       , isLit
                       , getIndexedName
                       ) where

import Jade.Decode.Types

getName (ValIndex n _) = n
getName (Lit _) = "" -- this is also no good. this should be a Maybe String.

getIndexedName (ValIndex n idx) = n ++ show idx
getIndexedName (Lit _) = "" -- this is no good.


hasIdent (ValIndex n _) ident = n == ident
hasIdent _ _ = False

isLit (Lit _) = True
isLit _ = False
