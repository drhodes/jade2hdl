module Jade.Decode.Sig ( sigBundle
                       , explode
                       , parseSig
                       , width
                       , twosComplement
                       ) where

import Control.Monad
import Data.Char as DC
import Text.Printf
import Jade.Decode.Types
import Text.Parsec
import Text.Parsec.String
import qualified Numeric as N
import qualified Text.Parsec.Number as TPN
import qualified Jade.Decode.Bundle as Bundle

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
    -- | "OUT" is a reserved name in HDL, save the user from having to change it by renaming it.
    "OUT" -> return "RESERVED_OUT"
    x -> return x

-- := sig#count         replicate sig specified number of times 
sigHash :: Parser ValBundle
sigHash = do name <- symbol
             char '#'
             n <- many1 digit -- this may start with zero!
             return $ explode $ SigHash name (read n)

-- := sig[idx] 
sigIndex :: Parser ValBundle
sigIndex = do
  name <- symbol
  char '['
  idx <- many1 digit
  char ']'
  return $ explode $ SigIndex name (read idx)

-- := sig[start:stop]   expands to sig[start],sig[start+step],...,sig[end]
sigRange :: Parser ValBundle
sigRange = do
  name <- symbol
  char '['
  from <- many1 digit
  char ':'
  to <- many1 digit
  char ']'
  return $ explode (SigRange name (read from) (read to)) 
    
-- explode s = case explode s of
--                  Left msg -> fail $ msg ++ (show log)
--                  Right xs -> xs

-- := sig[start:stop:step]   expands to sig[start],sig[start+step],...,sig[end]
sigRangeStep :: Parser ValBundle
sigRangeStep = do name <- symbol
                  char '['
                  from <- many1 digit
                  char ':'
                  to <- many1 digit
                  char ':'
                  step <- many1 digit
                  char ']'
                  return $ explode $ SigRangeStep name (read from) (read to) (read step)

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
sigQuote :: Parser ValBundle
sigQuote = do val <- number
              char '\''
              width <- many1 digit
              return $ explode (SigQuote val (read width))

sigSimple :: Parser ValBundle
sigSimple = do s <- symbol
               return $ explode (SigSimple s)

oneBundle :: Parser ValBundle
oneBundle = choice $ map try [ sigQuote
                             , sigHash
                             , sigRange
                             , sigRangeStep
                             , sigIndex
                             , sigSimple
                             ] 

sigConcat :: Parser ValBundle
sigConcat = do x <- oneBundle
               xs <- many1 $ do hspaces
                                char ','
                                hspaces
                                oneBundle
               return $ mconcat (x:xs)

sigBundle :: Parser ValBundle
sigBundle = choice $ map try [ sigConcat, oneBundle ]

parseSig :: String -> Either ParseError ValBundle
parseSig s = parse sigBundle "signal" s

width :: Sig -> Integer
width sig = case sig of
              SigSimple _ -> 1
              SigIndex _ _ -> 1
              SigHash _ n -> fromIntegral n
              SigRange _ from to -> fromIntegral $ abs (from - to) + 1
              SigRangeStep name from to 0 -> width (SigRangeStep name from to 1)
              SigRangeStep _ from to step -> let range = if from < to 
                                                         then [from, from+step .. to]
                                                         else [from, from-step .. to]
                                             in fromIntegral $ length range
              SigQuote _ w -> fromIntegral w

explode :: Sig -> ValBundle
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
  in Bundle result

range from to step = if from < to 
                     then [from, from+step .. to] -- ascending
                     else [from, from-step .. to] -- descending

genbits n | n == 0 = []
          | n `mod` 2 == 0 = L : (genbits next)
          | otherwise = H : (genbits next)
  where next = n `div` 2

twosComplement :: Integral a => a -> Integer -> [BinVal]
twosComplement val numBits = reverse $ take (fromInteger numBits) $ (genbits val) ++ repeat L
