{-# LANGUAGE FlexibleContexts #-}
module Jade.Bundle where

import Control.Monad
import Jade.Common
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

getName :: Bundle Val -> Either String String
getName bundle = do
  let names = DL.nub $ getNames bundle
  case names of
    [name] -> Right name
    [] -> Left "no names found"
    _ -> Left "too many names found"

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

explode :: Sig -> Bundle Val
explode sig =
  let result =
        case sig of 
          SigSimple name -> [ValIndex name 0]
          SigIndex name x -> [ValIndex name x]
          SigHash name x -> error "Decode.Decode.Sig.explode doesn't handle SigHash yet"
          SigRange name from to ->
            map (ValIndex name) (range from to 1)
          SigRangeStep name from to step ->
            map (ValIndex name) (range from to step)
          SigQuote val width -> map Lit $ twosComplement val width
          --SigConcat val width -> map Lit $ twosComplement val width
  in Bundle result

range from to step = if from < to 
                     then [from, from+step .. to] -- ascending
                     else [from, from-step .. to] -- descending

genbits n | n == 0 = []
          | n `mod` 2 == 0 = L : (genbits next)
          | otherwise = H : (genbits next)
  where next = n `div` 2

twosComplement :: Integral a => a -> Int -> [BinVal]
twosComplement val numBits = Prelude.reverse $
                             take numBits $
                             (genbits val) ++ repeat L
