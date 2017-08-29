module Main where

import Jade.Types
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad

data Example = Example { infile :: String
                       , hdl :: String
                       , enthusiasm :: Int
                       }



targetOption :: Parser String
targetOption = strOption $ mconcat [ long "target-lang"
                                   , short 't'
                                   , metavar "TARGETLANG"
                                   , help "[vhdl|verilog]" ]

infileOption = strOption $ mconcat [ long "infile"
                                   , metavar "SOURCE"
                                   , help "JADE exported json module" ]


thuseOption = option auto $ mconcat [ long "enthusiasm"
                                    , help "How enthusiastically to greet"
                                    , showDefault
                                    , value 1
                                    , metavar "INT" ]

sample :: Parser Example
sample = liftA3 Example targetOption infileOption thuseOption
  
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
