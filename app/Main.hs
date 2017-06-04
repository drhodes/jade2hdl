module Main where

import Jade.Types
import Options.Applicative
import Data.Semigroup ((<>))

data Example = Example { infile :: String
                       , hdl :: String
                       , enthusiasm :: Int
                       }

sample :: Parser Example
sample = Example
  ------------------------------------------------------------------
  <$> strOption
  ( long "infile"
    <> metavar "SOURCE"
    <> help "JADE exported json module" )
  
  ------------------------------------------------------------------
  <*> strOption
  ( long "target-lang"
    <> short 't'
    <> metavar "TARGETLANG"
    <> help "Which HDL to export" )
  
  ------------------------------------------------------------------
  <*> option auto
  ( long "enthusiasm"
    <> help "How enthusiastically to greet"
    <> showDefault
    <> value 1
    <> metavar "INT" )
  
main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Example -> IO ()
greet (Example h s n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()
