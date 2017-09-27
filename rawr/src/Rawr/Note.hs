{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

module Rawr.Note where

import GHC.Generics
import Text.Printf
import Data.Aeson
import qualified Data.List as DL
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as DB8

data Note = Func String
          | Note String
          | Call String
          | CallEdge String String
          | EndFunc
          deriving (Generic, Show, Eq, ToJSON)

isFunc (Func _) = True
isFunc _ = False

isCallEdge (CallEdge _ _) = True
isCallEdge _ = False

isEndFunc EndFunc = True
isEndFunc _ = False

isNote (Note _) = True
isNote _ = False

-- what is this for?
notesToString :: [Note] -> String
notesToString notes = DB8.unpack $ encode $ map notesToString' notes

replaceChar oldChar newChar cs = [if c == oldChar then newChar else c | c <- cs]

notesToString' :: Note -> String

notesToString' (Func name) =
  let template = "{ \"function\" : \"%s\""
  in printf template name

notesToString' EndFunc = "}"
  
notesToString' (Note note) = note

example = [ Func "f1"
          , Note "asdf"
          , Note "zxcv"
          , Func "f2"
          , Note "c"
          , EndFunc
          , Note "d"
          , Func "f3"
          , Func "f4"
          , EndFunc
          , EndFunc
          , Note "e"
          , EndFunc
          ]

callCollect [] = []
callCollect (Func caller : EndFunc : rest) = (Call caller : rest)
callCollect (Func caller1 : Func caller2 : rest) = callCollect (Func caller1 : (callCollect (Func caller2 : rest)))
callCollect (Func caller : Call callee : rest) = (CallEdge caller callee) : callCollect (Func caller : rest)
callCollect (Func caller : ce@(CallEdge _ _) : rest) = ce : callCollect (Func caller : rest)
callCollect (EndFunc : rest) = callCollect rest
callCollect xs = [EndFunc]

dropNotes xs = filter (not . isNote) xs

dotGraph :: [Note] -> String
dotGraph notes = let template = unlines [ "digraph {"
                                        , "rankdir=\"LR\";"
                                        , " %s "
                                        , "}"
                                        ] 
                     callEdges = DL.nub $ filter isCallEdge (callCollect (dropNotes notes))
                     nodeTxt = concat [printf "%s -> %s;\n" x y | (CallEdge x y) <- callEdges]
                     fixedTxt = replaceChar ':' ' '
                                $ replaceChar '\'' '_'
                                $ replaceChar '/' '_'
                                $ replaceChar '.' '_' nodeTxt
                 in printf template fixedTxt
