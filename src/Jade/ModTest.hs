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
           hspaces
           liftM Power $ strAndNum "Vdd"

plotDef :: Parser PlotDef
plotDef = do string ".plotdef"
             hspaces
             sig <- Sig.sig
             labels <- many1 $ try (hspaces >> (many1 alphaNum <|> string "?"))
             return $ PlotDef sig labels

simplePlot :: Parser PlotStyle
simplePlot = do string ".plot"
                hspaces
                sig <- Sig.sig
                return $ SimplePlot sig

basePlot :: Parser PlotStyle
basePlot = do string ".plot"
              hspaces
              base <- oneOf "XDB"
              char '('
              hspaces
              sig <- Sig.sig
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
                  sig <- Sig.sig
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
            ins <- many1 $ try (hspaces >> Sig.sig)
            return $ Inputs ins

outputs :: Parser Outputs
outputs = do string ".group"
             hspaces
             string "outputs"
             hspaces
             outs <- many1 $ try (hspaces >> Sig.sig)
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
  clines <- many1 (hspaces >> action)
  return $ CycleLine clines

binNum :: Parser BinVal
binNum = choice $ map try [ char '1' >> return H
                          , char '0' >> return L ]

binLetter :: Parser BinVal
binLetter = choice $ map try [ char 'H' >> return H
                             , char 'L' >> return L
                             , char '-' >> return Z ]
            
lineAsserts :: Parser [BinVal]
lineAsserts = many1 $ do b <- binNum
                         hspaces
                         return b

hspaces :: Parser ()
hspaces = do many $ oneOf " \t"
             return ()

lineSamples :: Parser [BinVal]
lineSamples = many $ do b <- binLetter
                        hspaces
                        return b

comment :: Parser String
comment = do string "//"
             xs <- many $ noneOf "\n"             
             return xs

testLine :: Parser TestLine
testLine = do
  las <- lineAsserts
  lss <- lineSamples
  c <- optionMaybe comment
  return $ TestLine las lss c

defaultModTest = let
  n = Nothing
  in ModTest n n n n n n [] [] [] 

acreteTestLine filename mt lineNum line =
  case parse testLine filename line of
    Left msg -> mt
    Right p -> let tlines = modTestLines mt
               in mt { modTestLines = p:tlines  }

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

acreteTry [] filename mt lineNum line = Left $ "Couldn't match this line: " ++ line
acreteTry (f:fs) filename mt lineNum line =
  let mt' = f filename mt lineNum line
  in if mt' == mt
  then acreteTry fs filename mt lineNum line
  else Right mt'
    
acreteOne filename mt lineNum "" = Right mt 
acreteOne filename mt lineNum line = 
  let fs = [ acreteTestLine
           , acreteCycleLine
           , acreteMode
           , acretePower
           , acreteThresholds
           , acreteInputs
           , acreteOutputs
           , acretePlotDef
           , acretePlot
           ]
  in acreteTry fs filename mt lineNum line

acreteAll _ mt [] = Right mt
acreteAll filename mt ((lineNum,line):rest) =
  let mt' = acreteOne filename mt lineNum line
  in case mt' of
    Left msg -> Left msg
    Right asdf -> acreteAll filename asdf rest

parseModTestFile filename = do
  xs <- liftM lines (readFile filename)
  let linePairs = zip [1..] xs
  return $ acreteAll filename defaultModTest linePairs
  
parseModTestString :: String -> Either [Char] ModTest
parseModTestString s =
  let xs = lines s
      linePairs = zip [1..] xs
  in acreteAll "change-me" defaultModTest linePairs

bust _ [] = []
bust (chunk:rest) xs = take chunk xs : (bust rest $ drop chunk xs)

testLineSignals :: ModTest -> TestLine -> Either String [(Sig, [BinVal])]
testLineSignals modt (TestLine asserts samples _) = do
  let Just (Inputs inSigs) = modInputs modt
      Just (Outputs outSigs) = modOutputs modt
      inWidths = map Sig.width inSigs
      outWidths = map Sig.width outSigs

  -- improve these error messages
  when (sum inWidths /= length asserts) $
    fail $ "testline asserts doesn't match the width of the signals"
    ++ (show (inSigs, asserts))

  when (sum outWidths /= length samples) $
    fail $ "testline samples do not match the width of the signals"
    ++ (show (outSigs, samples))

  return $ concat [ zip inSigs (bust inWidths asserts)
                  , zip outSigs (bust outWidths samples) ]




