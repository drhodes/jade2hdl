module Jade.Sig where

import Text.Parsec.String
import qualified Text.Parsec.Number as TPN
import Text.Parsec
import Jade.Types
import qualified Numeric as N
import Text.Format
import Control.Monad
import Jade.Util
import Data.Char as DC
{-
signal names need to be parsed.

    // parse string into an array of symbols.  Canonicalize all text to lower case. 
    //  sig_list := sig[,sig]... 
    //  sig := symbol 
    //      := sig#count         replicate sig specified number of times 
    //      := sig[start:stop:step]   expands to sig[start],sig[start+step],...,sig[end] 
    //      := number'size       generate appropriate list of vdd, gnd to represent number
-}

symbol :: Parser String
symbol = do
  x <- letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  let sym = map DC.toUpper (x:rest)
  case sym of
    -- "OUT" is a reserved name in HDL, save the user from having to change it by renaming it.
    "OUT" -> return "RESERVED_OUT"
    x -> return x


-- := sig#count         replicate sig specified number of times 
sigHash :: Parser Sig
sigHash = do name <- symbol
             char '#'
             n <- many1 digit -- this may start with zero!
             return $ SigHash name (read n)

-- := sig[idx] 
sigIndex :: Parser Sig
sigIndex = do
  name <- symbol
  char '['
  idx <- many1 digit
  char ']'
  return $ SigIndex name (read idx)

-- := sig[start:stop]   expands to sig[start],sig[start+step],...,sig[end]
sigRange :: Parser Sig
sigRange = do
  name <- symbol
  char '['
  from <- many1 digit
  char ':'
  to <- many1 digit
  char ']'
  return $ SigRange name (read from) (read to)


-- := sig[start:stop:step]   expands to sig[start],sig[start+step],...,sig[end]
--sigRangeStep
sigRangeStep :: Parser Sig
sigRangeStep = do name <- symbol
                  char '['
                  from <- many1 digit
                  char ':'
                  to <- many1 digit
                  char ':'
                  step <- many1 digit
                  char ']'
                  return $ SigRangeStep name (read from) (read to) (read step)

hex :: Parser Integer
hex = do string "0"
         n <- TPN.hexadecimal
         return n

hspaces = do many $ oneOf " \t"
           
bin :: Parser Integer
bin = do string "0b"
         n <- TPN.binary
         return n

number :: Parser Integer
number = choice [ try hex
                , try bin
                , do n <- try $ many1 digit
                     return $ (read n :: Integer)
                ]
  
-- := number'size // generate appropriate list of vdd, gnd to represent number
sigQuote :: Parser Sig
sigQuote = do val <- number
              char '\''
              width <- many1 digit
              return $ SigQuote val (read width)

sigSimple :: Parser Sig
sigSimple = do s <- symbol
               return $ SigSimple s


oneSig = choice $ map try [ sigQuote
                          , sigHash
                          , sigRange
                          , sigRangeStep
                          , sigIndex
                          , sigSimple
                          ] 

sigConcat :: Parser Sig
sigConcat = do x <- oneSig
               xs <- many1 $ do hspaces
                                char ','
                                hspaces
                                oneSig
               return $ SigConcat (x:xs)

sig = choice $ map try [ sigConcat, oneSig ]

parseSig :: String -> Either ParseError Sig
parseSig s = parse sig "signal" s

width :: Sig -> Integer
width sig = case sig of
              SigSimple _ -> 1
              SigIndex _ _ -> 1
              SigHash _ n -> fromIntegral n
              SigRange _ from to -> fromIntegral $ from - to + 1
              SigRangeStep _ from to step ->
                1 + (((fromIntegral from) - (fromIntegral to)) `div` (fromIntegral step))
              SigQuote _ w -> fromIntegral w
              SigConcat xs -> sum $ map width xs
              
hashMangle :: String -> Sig -> J Sig
hashMangle s sig =
  let f x = s ++ "_" ++ x
  in case sig of 
    SigSimple name -> return $ SigSimple (f name)
    SigIndex name x -> return $ SigIndex (f name) x
    SigHash name x -> return $ SigHash (f name) x
    SigRange name x y -> return $ SigRange (f name) x y 
    SigRangeStep name x y z -> return $ SigRangeStep (f name) x y z
    x -> die $ "hashMangle doesn't support: " ++ show x

getNames :: Sig -> J [String]
getNames sig =
  case sig of
    SigSimple name -> return [name]
    SigRange name from to -> return [name] 
    SigIndex name _ -> return [name]
    (SigQuote val width) -> return $ [format "SigQuote_{0}_{1}" [show val, show width]]
    SigConcat xs -> concatMapM getNames xs
    SigRangeStep n _ _ _ -> return [n]
    x -> die $ "Sig.getNames doesn't support: " ++ show x


hasIdent :: Sig -> String -> J Bool
hasIdent sig ident = "Sig.hasIdent" <? do
  ns <- getNames sig
  return $ ident `elem` ns

explode :: Sig -> J [Sig]
explode sig = "Sig.explode" <?
  case sig of 
    SigSimple name -> return $ [SigIndex name 0]
    SigIndex name x -> return $ [SigIndex name x]
    SigHash name x -> die "explode doesn't handle SigHash yet"
    SigRange name x y -> return $ [SigIndex name i | i <- if x == y then [x]
                                                          else if x < y
                                                               then [y, y-1 .. x]
                                                               else [x, x-1 .. y]]
    SigRangeStep name from to step -> do
      let range = if from < to 
                  then [from, from+step .. to] -- ascending
                  else [from, from-step .. to] -- descengind
      return $ map (SigIndex name) range

      
    SigConcat xs -> concatMapM explode xs
    x -> die $ "Sig.explode doesn't support: " ++ show x


flatten :: Sig -> J [Sig]
flatten (SigConcat xs) = return xs
flatten x = return [x]
