module Sig where

import Text.Parsec.String
import Text.Parsec

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
  return $ x:rest

data Sig = SigSimple String
         | SigIndex String Integer
         | SigHash String Integer
         | SigRange String Integer Integer
         | SigRangeStep String Integer Integer Integer
         | SigQuote String Integer
         deriving (Show, Eq)

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

-- := number'size // generate appropriate list of vdd, gnd to represent number
sigQuote :: Parser Sig
sigQuote = do val <- many1 digit
              char '\''
              width <- many1 digit
              return $ SigQuote (read val) (read width)


sigSimple :: Parser Sig
sigSimple = do s <- symbol
               return $ SigSimple s

sig = choice $ map try [ sigQuote
                       , sigHash
                       , sigRange
                       , sigRangeStep
                       , sigIndex
                       , sigSimple
                       ] 


parseSig :: String -> Either ParseError Sig
parseSig s = parse sig "signal" s
