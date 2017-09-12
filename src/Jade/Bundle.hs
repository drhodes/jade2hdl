{-# LANGUAGE FlexibleContexts #-}
module Jade.Bundle where

import Jade.Types
import Control.Monad
import Data.Maybe
import qualified Jade.Sig as Sig
import qualified Jade.Val as Val



containsIdentifier :: Bundle Val -> String -> Bool
containsIdentifier (Bundle xs) ident = or $ map (flip Val.hasIdent ident) xs

width :: Bundle a -> Int
width (Bundle xs) = length xs

getVals :: Bundle t -> [t]
getVals (Bundle xs) = xs

getLitVals (Bundle xs) = [lit | lit@(Lit _) <- xs]

getNames :: Bundle Val -> [String]
getNames (Bundle xs) = map Val.getName xs

getValsWithIdent :: Bundle Val -> String -> [Val]
getValsWithIdent (Bundle vals) ident = [v | v <- vals, v `Val.hasIdent` ident]

hasAnyValName :: Bundle Val -> Bool
hasAnyValName b = null $ getNames b


hasName b name = name `elem` getNames b
hasLit = not . null . getLitVals 

hasVal :: Eq a => Bundle a -> a -> Bool
hasVal (Bundle xs) val = or $ map (==val) xs
