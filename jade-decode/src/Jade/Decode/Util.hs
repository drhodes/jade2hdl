{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Jade.Decode.Util where

import Data.Hashable
import Control.Monad
import Text.Printf
import Data.List as DL
import Data.Char as DC
import qualified Data.Hashable as DH
import qualified Web.Hashids as WH
import qualified Data.ByteString.Char8 as B

startsWith tgt src = take (length src) tgt == src

uniq [] = []
uniq [x] = [x]
uniq [x, y] = if x == y then [y] else [x, y]
uniq (x:y:rest) = if x == y
                  then uniq (y:rest)
                  else x : (uniq (y:rest))

-- | break a list into a number of n-sized lists
chunk :: Integral t => t -> [a] -> [[a]]
--chunk 0 xs = [xs]
chunk n [] = []
chunk n xs = take (fromIntegral n) xs : chunk (fromIntegral n) (drop (fromIntegral n) xs)

strip x = let x1 = dropWhile DC.isSpace x
              x2 = dropWhile DC.isSpace (reverse x1)
          in reverse x2

quote x = ['"'] ++ x ++ ['"']

bust :: [Int] -> [a] -> [[a]]
bust _ [] = []
bust (chunk:rest) xs = take chunk xs : (bust rest $ drop chunk xs)

removeQuotes = filter (/= '"') 

concatMapM f xs = concat <$> mapM f xs

hashid :: Hashable a => a -> String
hashid x =
  let ctx = WH.hashidsSimple "salt"
      in B.unpack $ WH.encode ctx . abs . DH.hash $ x

slashesToScores str = [if x == '/' then '_' else x | x <- str]

readHex :: Num b => Char -> Either String b
readHex c = let wrap x = Right $ fromIntegral x
            in case DC.toUpper c of
  '0' -> wrap 0
  '1' -> wrap 1
  '2' -> wrap 2
  '3' -> wrap 3
  '4' -> wrap 4
  '5' -> wrap 5
  '6' -> wrap 6
  '7' -> wrap 7
  '8' -> wrap 8
  '9' -> wrap 9
  'A' -> wrap 0xA
  'B' -> wrap 0xB
  'C' -> wrap 0xC
  'D' -> wrap 0xD
  'E' -> wrap 0xE
  'F' -> wrap 0xF
  _ -> Left $ printf "Couldn't parse '%s' as a hex digit" c
  
ok :: a -> Either String a
ok x = Right x

contains a b = DL.isInfixOf b a

downFrom 0 = [0]
downFrom 1 = [1, 0]
downFrom n = [n, n-1 .. 0]

filterOut f xs = filter (not . f) xs

evenlyDivides x y = let n = y `div` x
                    in n * x == y

mapF [] _ = []
mapF _ [] = []
mapF (f:fs) (x:xs) = (f x):(mapF fs xs)

triangleProd [] = []
triangleProd (x:xs) = zip (repeat x) xs ++ (triangleProd xs)

replace x y str = [if c == x then y else c | c <- str]
