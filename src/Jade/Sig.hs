module Jade.Sig ( sigBundle
                , explode
                , parseSig
                , width
                , twosComplement
                ) where

import Control.Monad
import Data.Char as DC
import Jade.Common
import Text.Format
import Text.Parsec
import Text.Parsec.String
import qualified Numeric as N
import qualified Text.Parsec.Number as TPN
import qualified Jade.Bundle as Bundle

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
             return $ runExplode $ SigHash name (read n)

-- := sig[idx] 
sigIndex :: Parser ValBundle
sigIndex = do
  name <- symbol
  char '['
  idx <- many1 digit
  char ']'
  return $ runExplode $ SigIndex name (read idx)

-- := sig[start:stop]   expands to sig[start],sig[start+step],...,sig[end]
sigRange :: Parser ValBundle
sigRange = do
  name <- symbol
  char '['
  from <- many1 digit
  char ':'
  to <- many1 digit
  char ']'
  return $ runExplode (SigRange name (read from) (read to)) 
    
runExplode s = case runX emptyTopl $ explode s of
                 (Left msg, log) -> fail $ msg ++ (show log)
                 (Right xs, _) -> xs

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
                  return $ runExplode $ SigRangeStep name (read from) (read to) (read step)

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
              return $ runExplode (SigQuote val (read width))

sigSimple :: Parser ValBundle
sigSimple = do s <- symbol
               return $ runExplode (SigSimple s)

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

explode :: Sig -> J ValBundle
explode sig = "Sig.explode" <? do
  result <- case sig of 
    SigSimple name -> return $ [ValIndex name 0]
    SigIndex name x -> return $ [ValIndex name x]
    SigHash name x -> die "explode doesn't handle SigHash yet"
    SigRange name from to ->
      return $ map (ValIndex name) (range from to 1)
    SigRangeStep name from to step ->
      return $ map (ValIndex name) (range from to step)
    SigQuote val width -> do
      -- need to convert val to twos complement and make a bundle
      return $ map Lit $ twosComplement val width
  return $ Bundle result

range from to step = if from < to 
                     then [from, from+step .. to] -- ascending
                     else [from, from-step .. to] -- descengind

genbits n | n == 0 = []
          | n `mod` 2 == 0 = L : (genbits next)
          | otherwise = H : (genbits next)
  where next = n `div` 2

twosComplement :: Integral a => a -> Integer -> [BinVal]
twosComplement val numBits = reverse $ take (fromInteger numBits) $ (genbits val) ++ repeat L
