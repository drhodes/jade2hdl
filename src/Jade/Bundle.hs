{-# LANGUAGE FlexibleContexts #-}
module Jade.Bundle where

import Jade.Common
import Control.Monad
import Data.Maybe
import qualified Data.List as DL
import qualified Jade.Val as Val

containsIdentifier :: Bundle Val -> String -> Bool
containsIdentifier (Bundle xs) ident = or $ map (flip Val.hasIdent ident) xs

intersection (Bundle xs) (Bundle ys) = Bundle $ xs `DL.intersect` ys

width :: Bundle a -> Int
width (Bundle xs) = length xs

getVals :: Bundle t -> [t]
getVals (Bundle xs) = xs

getLitVals (Bundle xs) = [lit | lit@(Lit _) <- xs]

getNames :: Bundle Val -> [String]
getNames (Bundle xs) = [name | name <- map Val.getName xs, not $ null name]

getIndexesWithName (Bundle xs) name = [v | v@(ValIndex vname _) <- xs, vname == name]

getName :: Bundle Val -> J String
getName bundle = do
  let names = DL.nub $ getNames bundle
  case names of
    [name] -> return name
    [] -> die "no names found"
    _ -> die "too many names found"

reverse (Bundle xs) = Bundle (DL.reverse  xs)

getIndexedNames (Bundle xs) = map Val.getIndexedName xs

getValsWithIdent :: Bundle Val -> String -> [Val]
getValsWithIdent (Bundle vals) ident = [v | v <- vals, v `Val.hasIdent` ident]

-- check to see if this bundle contains a ValIndex with name. 
hasSigName :: Bundle Val -> Bool
hasSigName b = if null $ getNames b then False else True

hasName b name = name `elem` getNames b
hasLit = not . null . getLitVals 

hasVal :: Eq a => Bundle a -> a -> Bool
hasVal (Bundle xs) val = or $ map (==val) xs
