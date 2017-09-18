{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

module Jade.Note where

import GHC.Generics
import Text.Format
import Data.Aeson
import qualified Data.List as DL
import Data.Maybe
import Jade.Util

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

notesToString :: [Note] -> String
notesToString notes = format "[{0}]" $ [DL.intercalate "," (map notesToString' notes)]

notesToString' :: Note -> String

notesToString' (Func name) =
  let template = "{ \"function\" : \"{0}\""
  in format template [name]

notesToString' EndFunc = "}"
  
-- notesToString' (Func name) =
--   let template = "{ \"function\" : \"{0}\", \"notes\" : [{1}] }"
--       noteTxt = DL.intercalate "," (map notesToString' notes)
--   in format template [name, noteTxt]
  
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
callCollect xs = error (show xs)

dropNotes xs = filter (not . isNote) xs




dotGraph :: [Note] -> String
dotGraph notes = let template = unlines [ "digraph {"
                                        , "rankdir=\"LR\";"
                                        , " {0} "
                                        , "}"
                                        ] 
                     callEdges = DL.nub $ filter isCallEdge (callCollect (dropNotes notes))
                     nodeTxt = concat [format "{0} -> {1};\n" [x, y] | (CallEdge x y) <- callEdges]
                     fixedTxt = replace ':' ' '
                                $ replace '\'' '_'
                                $ replace '/' '_'
                                $ replace '.' '_' nodeTxt
                 in format template [fixedTxt]
