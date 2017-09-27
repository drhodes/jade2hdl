module Jade.Decode.ModTest ( parseModTestString
                           , setSignals
                           , assertBitVals
                           , sampleBitVals
                           , getTestlineComment
                           ) where

import Control.Monad
import Jade.Decode.Types
import Jade.Decode.Util
import Text.Parsec
import Text.Parsec.String
import qualified Data.List as DL
import qualified Jade.Decode.Bundle as Bundle
import qualified Jade.Decode.Sig as Sig
import qualified Numeric as N
import qualified Text.Parsec.Number as TPN

ident :: Parser String
ident = do start <- letter <|> char '_'
           rest <- many1 $ alphaNum <|> char '_'
           return $ start : rest

numval = choice [ try TPN.floating
                , fromIntegral <$> try TPN.decimal 
                ]

strAndNum s =
  do string s
     spaces
     string "=" 
     spaces
     numval <?> "Expecting a whole number or float here"

power :: Parser Power
power = do string ".power"
           hspaces
           Power <$> strAndNum "Vdd"

plotDef :: Parser PlotDef
plotDef = do string ".plotdef"
             hspaces
             sig <- Sig.sigBundle
             labels <- many1 $ try (hspaces >> (many1 alphaNum <|> string "?"))
             return $ PlotDef sig labels

simplePlot :: Parser PlotStyle
simplePlot = do string ".plot"
                hspaces
                SimplePlot <$> Sig.sigBundle


basePlot :: Parser PlotStyle
basePlot = do string ".plot"
              hspaces
              base <- oneOf "XDB"
              char '('
              hspaces
              sig <- Sig.sigBundle
              hspaces
              char ')'
              return $ case base of
                         'X' -> HexStyle sig
                         'B' -> BinStyle sig
                         'D' -> DecStyle sig

plotDefStyle :: Parser PlotStyle
plotDefStyle = do string ".plot"
                  hspaces
                  name <- ident
                  char '('
                  hspaces
                  sig <- Sig.sigBundle
                  hspaces 
                  char ')'
                  return $ PlotDefStyle name sig

plot :: Parser PlotStyle
plot = choice $ map try [ simplePlot
                        , basePlot
                        , plotDefStyle ]

thresholds :: Parser Thresholds
thresholds = do string ".thresholds"
                vol <- spaces >> strAndNum "Vol"
                vil <- spaces >> strAndNum "Vil"
                vih <- spaces >> strAndNum "Vih"
                voh <- spaces >> strAndNum "Voh"
                return $ Thresholds vol vil vih voh

inputs :: Parser Inputs
inputs = do string ".group"
            hspaces
            string "inputs"
            ins <- many1 $ try (hspaces >> Sig.sigBundle)
            return $ Inputs ins

outputs :: Parser Outputs
outputs = do string ".group"
             hspaces
             string "outputs"
             hspaces
             outs <- many1 $ try (hspaces >> Sig.sigBundle)
             return $ Outputs outs

mode :: Parser Mode
mode = do string ".mode"
          hspaces
          mtype <- choice [ try $ string "gate"
                          , try $ string "device" ]
          hspaces
          case mtype of
            "gate" -> return Gate
            "device" -> return Device
            _ -> fail "mode must be either 'gate' or 'device'"

actionAssert :: Parser Action
actionAssert = do
  string "assert" >> spaces
  Assert <$> ident

actionDeassert :: Parser Action
actionDeassert = do
  string "deassert" >> spaces
  Deassert <$> ident

actionSample :: Parser Action
actionSample = do
  string "sample" >> spaces
  Sample <$> ident

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
  Tran <$> duration

actionSetSignal :: Parser Action
actionSetSignal = do
  sigName <- Sig.sigBundle
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
  clines <- many1 (hspaces >> action)
  return $ CycleLine clines
            
binVal :: Parser BinVal
binVal = choice $ map try [ char '1' >> return H
                          , char '0' >> return L
                          , char '-' >> return Z
                          , char 'H' >> return H
                          , char 'L' >> return L
                          ]

hspaces :: Parser ()
hspaces = do many $ oneOf " \t"
             return ()

lineBinVals = many $ do b <- binVal
                        hspaces
                        return b

comment :: Parser String
comment = do string "//"
             xs <- many $ noneOf "\n"             
             return xs

testLine :: Parser TestLine
testLine = do
  bvs <- lineBinVals
  c <- optionMaybe comment
  return $ TestLine bvs c

defaultModTest = let
  n = Nothing
  in ModTest n n n n n n [] [] [] 

acreteTestLine filename mt lineNum line =
  case parse testLine filename line of
    Left msg -> mt
    Right p -> let tlines = modTestLines mt ++ [p]
               in mt { modTestLines = tlines }

acreteCycleLine filename mt lineNum line =
  case parse cycleLine filename line of
    Left msg -> mt
    Right p -> mt { modCycleLine = Just p }
    
acreteMode filename mt lineNum line =
  case parse mode filename line of
    Left msg -> mt
    Right p -> mt { modMode = Just p }

acretePower filename mt lineNum line =
  case parse power filename line of
    Left msg -> mt
    Right p -> mt { modPower = Just p }

acretePlotDef filename mt lineNum line =
  case parse plotDef filename line of
    Left msg -> mt
    Right pdef -> let pdefs = modPlotDef mt
                  in mt { modPlotDef = pdef:pdefs }

acretePlot filename mt lineNum line =
  case parse plot filename line of
    Left msg -> mt
    Right p -> let ps = modPlotStyles mt
               in mt { modPlotStyles = p:ps }

acreteThresholds filename mt lineNum line =
  case parse thresholds filename line of
    Left msg -> mt
    Right p -> mt { modThresholds = Just p }

commented c = do x <- c
                 hspaces
                 optional comment
                 return x

acreteInputs filename mt lineNum line =
  case parse (commented inputs) filename line of
    Left msg -> mt
    Right p -> mt { modInputs = Just p }
  
acreteOutputs filename mt lineNum line =
  case parse outputs filename line of
    Left msg -> mt
    Right p -> mt { modOutputs = Just p }

acreteTry [] filename mt lineNum line = Left $ "Couldn't match this line: " ++ show (line, lineNum)
acreteTry (f:fs) filename mt lineNum line =
  let mt' = f filename mt lineNum line
  in if mt' == mt
  then acreteTry fs filename mt lineNum line
  else Right mt'
       
--acreteOne filename mt lineNum "" = Right mt 
acreteOne filename mt lineNum line =
  let line' = strip line
      fs = [ acreteCycleLine
           , acreteMode
           , acretePower
           , acreteThresholds
           , acreteInputs
           , acreteOutputs
           , acretePlotDef
           , acretePlot
           , acreteTestLine
           ]
  in if null line'
     then Right mt
     else acreteTry fs filename mt lineNum line'

acreteAll _ mt [] = Right mt
acreteAll filename mt ((lineNum,line):rest) =
  let mt' = acreteOne filename mt lineNum line
  in case mt' of
    Left msg -> Left msg
    Right asdf -> acreteAll filename asdf rest
  
parseModTestString :: String -> Either [Char] ModTest
parseModTestString s =
  let xs = lines s
      linePairs = zip [1..] xs
  in acreteAll "change-me" defaultModTest linePairs

assertBitVals modt (TestLine bvs _) = do
  let Just (Inputs inSigs) = modInputs modt
      inWidths = map Bundle.width inSigs
      totalInWidth = sum inWidths
  return $ take (fromIntegral totalInWidth) bvs

sampleBitVals modt testline@(TestLine bvs _) = do
  assertBvs <- assertBitVals modt testline
  return $ drop (length assertBvs) bvs

setSignals :: ModTest -> [ValBundle]
setSignals modt =
  case modCycleLine modt of
    Just (CycleLine actions) -> DL.nub [bndl | SetSignal bndl _ <- actions]
    Nothing -> []
  
getTestlineComment (TestLine _ (Just s)) = s
getTestlineComment _ = "empty comment"
