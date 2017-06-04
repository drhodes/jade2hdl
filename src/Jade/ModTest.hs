module Jade.ModTest where

import Text.Parsec.String
import qualified Text.Parsec.Number as TPN
import Text.Parsec
import Jade.Types
import qualified Numeric as N
import qualified Jade.Sig as Sig
import Control.Monad

ident :: Parser String
ident = do start <- letter <|> char '_'
           rest <- many1 $ alphaNum <|> char '_'
           return $ start : rest

numval = choice [ try TPN.floating
                , liftM fromIntegral $ try TPN.decimal 
                ]

strAndNum s =
  do string s
     spaces
     string "=" 
     spaces
     numval <?> "Expecting a whole number or float here"

power :: Parser Power
power = do string ".power"
           spaces
           liftM Power $ strAndNum "Vdd"

thresholds :: Parser Thresholds
thresholds = do string ".thresholds"
                vol <- spaces >> strAndNum "Vol"
                vil <- spaces >> strAndNum "Vil"
                vih <- spaces >> strAndNum "Vih"
                voh <- spaces >> strAndNum "Voh"
                return $ Thresholds vol vil vih voh

inputs :: Parser Inputs
inputs = do string ".group"
            spaces
            liftM Inputs $ many1 (spaces >> Sig.sig)

outputs :: Parser Outputs
outputs = do string ".group"
             spaces
             liftM Outputs $ many1 (spaces >> Sig.sig)

mode :: Parser Mode
mode = do string ".mode"
          spaces
          mtype <- choice [ try $ string "gate"
                          , try $ string "device" ]
          case mtype of
            "gate" -> return Gate
            "device" -> return Device
            _ -> fail "mode must be either 'gate' or 'device'"

actionAssert :: Parser Action
actionAssert = do
  string "assert" >> spaces
  liftM Assert ident

actionDeassert :: Parser Action
actionDeassert = do
  string "deassert" >> spaces
  liftM Deassert ident

actionSample :: Parser Action
actionSample = do
  string "sample" >> spaces
  liftM Sample ident

duration :: Parser Duration
duration = do
  n <- numval
  choice [ do try $ char 'n'
              return $ Nanosecond n
         , do try $ char 'm'
              return $ Millisecond n
         ]

actionTran :: Parser Action
actionTran = do
  string "tran" >> spaces
  liftM Tran duration

actionSetSignal :: Parser Action
actionSetSignal = do
  sigName <- Sig.sig
  spaces
  string "="
  spaces 
  n <- numval
  return $ SetSignal sigName n
  
action :: Parser Action
action = do
  choice $ map try [ actionAssert
                   , actionDeassert
                   , actionSample
                   , actionTran
                   , actionSetSignal
                   ]

cycleLine :: Parser CycleLine
cycleLine = do
  string ".cycle"
  liftM CycleLine $ many1 (spaces >> action)


binNum :: Parser BinVal
binNum = choice $ map try [ char '1' >> return H
                          , char '0' >> return L ]

binLetter :: Parser BinVal
binLetter = choice $ map try [ char 'H' >> return H
                             , char 'L' >> return L
                             , char '-' >> return Z ]
            
lineAsserts :: Parser [BinVal]
lineAsserts = many $ do b <- binNum
                        spaces
                        return b

lineSamples :: Parser [BinVal]
lineSamples = many $ do b <- binLetter
                        spaces
                        return b

comment :: Parser String
comment = do string "//"
             xs <- many $ noneOf "\n"             
             return xs

testLine :: Parser TestLine
testLine =
  do las <- lineAsserts
     lss <- lineSamples
     c <- optionMaybe comment
     return $ TestLine las lss c


parseTest s = do
  undefined

