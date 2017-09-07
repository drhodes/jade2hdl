module Main where

import Jade.Types
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import qualified Jade.Back.Viz as Viz
import qualified Jade.Decode as Decode
import qualified Data.Text.IO as TIO

data Opts = Opts { infile :: String
                 , topLevelMod :: String
                 , viz :: Bool
                 , hdlType :: Maybe String
                 } deriving (Show, Eq)


infileOption = strOption $ mconcat [ long "infile"
                                   , metavar "SOURCEPATH"
                                   , help "JADE exported json module"
                                   ]

topLevelModArg = strOption $ mconcat [ long "toplevel-mod"
                                     , metavar "STRING"
                                     , help "the name of the top level entity"
                                     ]

vizOption = switch $ mconcat [ long "viz"
                             , short 'z'
                             , help "emit graphviz of the jade module"
                             ]

hdl = strOption $ mconcat [ long "hdl"
                          , metavar "[vhdl|verilog]"
                          , help "HDL language, vhdl or verilog"
                          ]

sample :: Parser Opts
sample = Opts
         <$> infileOption
         <*> topLevelModArg
         <*> vizOption
         <*> optional hdl 
  
main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (sample <**> helper)
      ( fullDesc
        <> progDesc "a JADE module coverter from schematic to HDL"
        <> header "hello - not sure what this string goes to")

greet :: Opts -> IO ()
greet opts = do
  --formatOptions opts
  generateViz opts

formatOptions opts = do
  putStrLn $ "Jade module   :  " ++ (infile opts)
  putStrLn $ "graph viz?    :  " ++ (show $ viz opts)
  case hdlType opts of
    Just s -> putStrLn $ "HDL           :  " ++ s
    Nothing -> putStrLn $ "HDL           :  " ++ "None"

generateViz opts = when (viz opts) $ do
  -- putStrLn "Emitting graphviz dot file!!"
  -- putStrLn "Well... not yet.. "

  topl <- Decode.decodeTopLevel $ infile opts
  
  let dotSrc = case topl of
        Left msg -> error msg
        Right topl -> runJ topl $ do
          Viz.mkAllMods (topLevelMod opts)

  case dotSrc of
    Left msg -> putStrLn msg
    Right src -> TIO.putStrLn src
